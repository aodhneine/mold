#![no_std]
#![no_main]
#![feature(asm, const_ptr_offset)]

mod stivale;

// Allocate 8 KiB for the stack.
static STACK: [u8; 8192] = [0u8; 8192];

static STIVALE2_FRAMEBUFFER_TAG: stivale::stivale2_header_tag_framebuffer = stivale::stivale2_header_tag_framebuffer {
	tag: stivale::stivale2_header_tag {
		identifier: stivale::STIVALE2_HEADER_TAG_FRAMEBUFFER_ID,
		next: core::ptr::null(),
	},
	framebuffer_width: 0,
	framebuffer_height: 0,
	framebuffer_bpp: 24,
	unused: 0,
};

#[no_mangle]
#[link_section = ".stivale2hdr"]
#[used]
static STIVALE2_HEADER: stivale::stivale2_header = stivale::stivale2_header {
	entry_point: 0,
	stack: unsafe {
		STACK.as_ptr().add(STACK.len())
	},
	flags: (1 << 1),
	tags: &STIVALE2_FRAMEBUFFER_TAG as *const stivale::stivale2_header_tag_framebuffer as *const stivale::stivale2_header_tag,
};

mod x86 {
	pub fn hlt() -> ! {
		// SAFETY: Calling halt is really unsafe as it, uhm, halts the CPU. But as
		// we're writing a bare metal OS, we don't care about safety anyway. So if
		// you're calling this you know what you're doing.
		unsafe {
			asm!("hlt")
		};

		// SAFETY: We are calling hlt before this instruction, so we should never,
		// ever call this instruction. If we did, this is VERY VERY BAD. The ONLY
		// purpose of this instruction is to signalise to rust that hlt diverges,
		// so we can use it in places which expect the `!` return type.
		unsafe {
			core::hint::unreachable_unchecked()
		}
	}

	pub fn outb(port: u16, c: u8) {
		// SAFETY: I don't know, I don't think it should fail ever?
		unsafe {
			asm!("out dx, al", in("dx") port, in("al") c)
		};
	}
}

struct SerialPort {
	port: u16,
}

impl SerialPort {
	pub const fn new(port: u16) -> Self {
		return Self {
			port,
		};
	}

	pub fn write_byte(&mut self, b: u8) {
		x86::outb(self.port, b);
	}

	pub fn write_str(&mut self, s: &str) {
		for c in s.as_bytes() {
			self.write_byte(*c);
		}
	}
}

impl core::fmt::Write for SerialPort {
	fn write_str(&mut self, s: &str) -> core::fmt::Result {
		self.write_str(s);
		return Ok(());
	}
}

mod spin {
	use core::sync::atomic::AtomicBool;
	use core::sync::atomic::Ordering::{ Acquire, Release };

	/// Naive spinlock implementation for embedded systems.
	#[repr(transparent)]
	pub struct Spinlock {
		locked: AtomicBool,
	}

	impl Spinlock {
		pub fn new() -> Self {
			return Self {
				locked: AtomicBool::new(false),
			};
		}

		// Based on the source code taken from https://gpuopen.com/gdc-presentations/2019/gdc-2019-s2-amd-ryzen-processor-software-optimization.pdf (page 46)
		// and https://probablydance.com/2019/12/30/measuring-mutexes-spinlocks-and-how-bad-the-linux-scheduler-really-is/.
		pub fn lock(&self) {
			loop {
				if self.locked.compare_exchange(false, true, Acquire, Acquire).is_ok() {
					break;
				}

				core::hint::spin_loop();
			}
		}

		pub fn unlock(&self) {
			self.locked.store(false, Release);
		}
	}

	/// Mutually exclusive data implemented using spinlocks.
	pub struct Mutex<T> {
		lock: Spinlock,
		data: core::cell::UnsafeCell<T>,
	}

	// Same impl's as the standard library's Mutex.
	unsafe impl<T: Send> Send for Mutex<T> {}
	unsafe impl<T: Send> Sync for Mutex<T> {}

	impl<T> Mutex<T> {
		pub fn new(value: T) -> Self {
			return Self {
				lock: Spinlock::new(),
				data: core::cell::UnsafeCell::new(value),
			};
		}

		#[must_use = "this will immediately drop the mutex guard"]
		pub fn lock(&self) -> MutexGuard<T> {
			self.lock.lock();

			return MutexGuard {
				mutex: self,
			};
		}
	}

	pub struct MutexGuard<'a, T> {
		mutex: &'a Mutex<T>,
	}

	impl<'a, T> core::ops::Deref for MutexGuard<'a, T> {
		type Target = T;

		fn deref(&self) -> &Self::Target {
			return unsafe {
				&*self.mutex.data.get()
			};
		}
	}

	impl<'a, T> core::ops::DerefMut for MutexGuard<'a, T> {
		fn deref_mut(&mut self) -> &mut Self::Target {
			return unsafe {
				&mut *self.mutex.data.get()
			};
		}
	}

	impl<'a, T> Drop for MutexGuard<'a, T> {
		fn drop(&mut self) {
			self.mutex.lock.unlock();
		}
	}
}

mod lazy {
	use core::sync::atomic::AtomicU8;
	use core::sync::atomic::Ordering::{ Acquire, AcqRel, Release };

	/// Atomically-synchronised value that can be initialised only once.
	pub struct Once<T> {
		state: AtomicU8,
		inner: core::cell::UnsafeCell<core::mem::MaybeUninit<T>>,
	}

	const UNINITIALISED: u8 = 0x1;
	const INITIALISING: u8 = 0x2;
	const INITIALISED: u8 = 0x4;

	impl<T> Once<T> {
		/// Creates new uninitialised cell.
		pub const fn new() -> Self {
			return Self {
				state: AtomicU8::new(UNINITIALISED),
				inner: core::cell::UnsafeCell::new(core::mem::MaybeUninit::uninit()),
			};
		}

		pub fn set_once(&self, value: T) {
			match self.state.compare_exchange(UNINITIALISED, INITIALISING, AcqRel, Acquire) {
				Err(_) => panic!("cell already set"),
				Ok(_) => unsafe {
					(*self.inner.get()).write(value)
				},
			};

			self.state.store(INITIALISED, Release);
		}

		pub fn get(&self) -> &T {
			return match self.state.load(Acquire) {
				INITIALISED => unsafe {
					&*((*self.inner.get()).as_ptr())
				},
				INITIALISING => panic!("cell panicked while initialising"),
				UNINITIALISED => panic!("cell not set"),
				// SAFETY: We know that state only holds one of these three values, as
				// this is guaranteed by our control flow, so we can call unrachable
				// without risking UB.
				_ => unsafe {
					core::hint::unreachable_unchecked()
				},
			};
		}
	}

	unsafe impl<T: Sync + Send> Sync for Once<T> {}
}

pub struct GlyphMapping {
	// For example, a mapping `0\x20\x7f` would map each character from printable
	// ASCII set to a glyph index 0 to 96 (exclusive).
	// A mapping `\x41\x42\x43\x44\x45` would correspond to mapping characters
	// from 'A' to 'E' to indices 0 to 4.
	map: &'static str,
	/// Index of the glyph to be used if the character isn't in the supported
	/// mapping range.
	replacement: usize,
}

impl GlyphMapping {
	pub fn glyph(&self, c: char) -> usize {
		if c < ' ' || c > '~' {
			return self.replacement;
		}

		return (c as u8 - b' ') as usize;
	}
}

pub struct FontInfo {
	/// Region of memory where the spritesheet with the glyphs is stored.
	chars: &'static [u8],
	/// Mapping from character code point to glyph index.
	glyph_mapping: GlyphMapping,
	/// Width of the image containing glyphs.
	glyphsheet_width: u32,
	/// Size of each glyph in pixels.
	size: (u8, u8),
	/// Offset in pixels from the top of the glyph at which is the baseline.
	baseline: u16,
}

/// Global 6x13 font, based on [Cozette](https://github.com/slavfox/Cozette).
const FONT: FontInfo = FontInfo {
	chars: include_bytes!("../fonts/font.raw"),
	glyph_mapping: GlyphMapping {
		map: "\0\u{20}\u{7F}",
		// TODO: Potential issue? In the mapping we are specifying characters that
		// range from 0x20 to 0x7f, but we use a replacement glyph that falls out
		// of that character set (the DEL character).
		replacement: 95,
	},
	glyphsheet_width: 96,
	size: (6, 13),
	baseline: 10,
};

// char_offset = ' ' - character
// glyph_line_offset = char_offset / 16
// glyph_offset = char_offset - (glyph_line_offset * 12)
// pixel_offset = glyph_offset * 6
// k = pixel_offset / 8
// bitgroup_offset = k / 4

// .000....0...0000...000..   011100.00  1000.1111  00.011100
// 0...0..0.0...0..0.0...0.   100010.01  0100.0100  10.100010
// 0...0.0...0..0..0.0.....   100010.10  0010.0100  10.100000
// 0..00.0...0..0..0.0.....   100110.10  0010.0100  10.100000
// 0.0.0.0...0..000..0.....   101010.10  0010.0111  00.100000
// 0.0.0.00000..0..0.0.....   101010.11  1110.0100  10.100000
// 0.00..0...0..0..0.0.....   101100.10  0010.0100  10.100000
// 0.....0...0..0..0.0...0.   100000.10  0010.0100  10.100010
// .0000.0...0.0000...000..   011110.10  0010.1111  00.011100

// '@' => byte0 >> 2
// 'A' => (byte0 & 0b11) << 4 | byte1 >> 4
// 'B' => (byte1 & 0b1111) << 2 | byte2 >> 6
// 'C' => byte2 & 0b111111

mod framebuffer {
	pub struct Framebuffer {
		inner: *mut u8,
		pitch: u16,
	}

	impl Framebuffer {
		pub fn new(tag: &crate::stivale::stivale2_struct_tag_framebuffer) -> Self {
			return Self {
				inner: tag.framebuffer_addr as *mut u8,
				pitch: tag.framebuffer_pitch,
			};
		}

		// TODO: This is probably slow as heck!
		pub fn write_glyph(&mut self, x: usize, y: usize, c: char, font: &crate::FontInfo) {
			let mut cx = x;
			let mut cy = y;

			// Get the glyph index in the glyph sheet. For ASCII characters it's going
			// be (code point of the character) - (code point of the space), but for the
			// Unicode characters we probably need to use glyph mapping tables to get
			// indices of the glyphs in the font.
			let index = font.glyph_mapping.glyph(c);

			// Get the row and column where we can find that glyph.
			let row = index / 16;
			let column = index % 16;

			// Glyph column * width of each glyph.
			let pixel = column * 6;

			// 13 lines of 12 bytes in each glyph line.
			let lines = font.chars[row * 12 * 13..(row + 1) * 12 * 13].chunks(12);

			// Loop over each of the 13 lines that are part of the single glyph line.
			for line in lines {
				// Loop over each of the 12 bytes in the line.
				for (byte_index, byte) in line.iter().enumerate() {
					// Loop over each pixel in byte.
					for pixel_index in 0..8 {
						let pixel_offset = byte_index * 8 + pixel_index;

						// Ignore those pixels which are not in the column we want.
						if !(pixel..pixel + 6).contains(&pixel_offset) {
							continue;
						}

						// Get the pixel value.
						let px = (byte >> (8 - pixel_index - 1)) & 1;

						unsafe {
							(self.inner.add(self.pitch as usize * cy + 4 * cx) as *mut u32).write_volatile(match px {
								// TODO: Replace the hardcoded colour values with user-provided ones.
								0 => 0xff0000ff,
								1 => 0xffffffff,
								// Since we are doing `& 1` operation, this value cannot be
								// anything else but 0 or 1.
								_ => core::hint::unreachable_unchecked(),
							});
						}

						cx += 1;
					}
				}

				cx = x;
				cy += 1;
			}
		}

		// Framebuffer only exposes the memory address and some related values, as
		// width or pitch. It's purpose it not being a TTY like in Linux, but just
		// a general way to draw pixels on the screen. So you need to provide all
		// these arguments when writing a string.
		pub fn write_str(&mut self, x: usize, y: usize, s: &str, font: &crate::FontInfo) {
			for (i, c) in s.chars().enumerate() {
				let x = x + i * 6;
				self.write_glyph(x, y, c, font);
			}
		}
	}

	// SAFETY: The only place where we use the inner pointer in the framebuffer
	// and we care about threads is when using it as a static type, but then we
	// have to use it through a mutex.
	//
	// Note that this DOES NOT imply that framebuffer is safe to access between
	// threads, only that it can be sent between them. The only way to actually
	// access the inner pointer is in the static variable, which, again, is safe-
	// guarded behind a mutex.
	unsafe impl Send for Framebuffer {}

	pub static FRAMEBUFFER: crate::lazy::Once<crate::spin::Mutex<Framebuffer>> = crate::lazy::Once::new();
}

#[cfg_attr(debug_assertions, allow(unused_must_use))]
#[no_mangle]
pub extern "C" fn _start(info: *const stivale::stivale2_struct) {
	use core::fmt::Write;

	const COM1: u16 = 0x3F8;
	let mut com1 = SerialPort::new(COM1);

	let framebuffer_tag = stivale::get_tag(info, stivale::STIVALE2_STRUCT_TAG_FRAMEBUFFER_ID);

	if framebuffer_tag.is_null() {
		write!(com1, "no framebuffer available");
		x86::hlt();
	}

	let framebuffer_tag = unsafe {
		&*(framebuffer_tag as *const stivale::stivale2_struct_tag_framebuffer)
	};

	// Create new framebuffer from the framebuffer given to us by the bootloader
	// and set it as the global framebuffer.
	framebuffer::FRAMEBUFFER.set_once(spin::Mutex::new(framebuffer::Framebuffer::new(framebuffer_tag)));

	// We don't need to lock the mutex multiple times, we can just lock it once
	// and then unlock when we are done writing everything.
	let mut guard = framebuffer::FRAMEBUFFER.get().lock();

	guard.write_str(16, 16, "Il1egal 0O", &FONT);
	guard.write_str(16, 29, "It finally works!", &FONT);

	let memmap_tag = stivale::get_tag(info, stivale::STIVALE2_STRUCT_TAG_MEMMAP_ID);

	if memmap_tag.is_null() {
		write!(com1, "no memory app available");
		x86::hlt();
	}

	let memmap_tag = unsafe {
		&*(memmap_tag as *const stivale::stivale2_struct_tag_memmap)
	};

	// Print memory map (for debug purposes). This way we know which areas of the
	// memory we are allowed to use.
	for i in 0..memmap_tag.entries {
		let entry = unsafe {
			&*memmap_tag.memmap.as_ptr().add(i as usize)
		};

		writeln!(com1, "[{:>2}] {:>#10x} {:8} {:?}", i, entry.base, entry.length, entry.ty);
	}

	write!(com1, "{}", "welcome to mold");
	x86::hlt();
}

#[panic_handler]
#[cold]
fn panic(info: &core::panic::PanicInfo) -> ! {
	use core::fmt::Write;

	const COM1: u16 = 0x3F8;
	let mut com1 = SerialPort::new(COM1);

	// TODO: Use framebuffer in addition to the serial port if available.

	let _ = writeln!(com1, "panic: {}", info);
	loop {}
}
