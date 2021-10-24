#![no_std]
#![no_main]
#![feature(asm)]

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
	stack: &STACK[8191] as *const u8,
	flags: (1 << 1),
	tags: &STIVALE2_FRAMEBUFFER_TAG as *const stivale::stivale2_header_tag_framebuffer as *const stivale::stivale2_header_tag,
};

// THERE IS NOTHING WRONG WITH USING A THIRD-PARTY BOOTLOADER SUCH AS GRUB OR
// LIMINE OR BOOTBOOT OR PHIL OPP'S BOOTLOADER IF THIS MAKES YOUR LIFE EASIER

mod x86 {
	pub fn hlt() {
		// SAFETY: Calling halt is really unsafe as it, uhm, halts the CPU. But as
		// we're writing a bare metal OS, we don't care about safety anyway. So if
		// you're calling this you know what you're doing.
		//
		// And if not, then what are you even doing?
		unsafe {
			asm!("hlt")
		};
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

struct FontInfo {
	/// Region of memory where the spritesheet with the glyphs is stored.
	chars: &'static [u8],
	/// Width of the image containing glyphs.
	glyphsheet_width: u32,
	/// Size of each glyph in pixels.
	size: (u8, u8),
	/// Offset in pixels from the top of the glyph at which is the baseline.
	baseline: u16,
}

/// Global 6x13 font, based on default X11 fonts.
const FONT: FontInfo = FontInfo {
	chars: include_bytes!("../font.raw"),
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

	let framebuffer = framebuffer_tag.framebuffer_addr as *mut u8;
	writeln!(com1, "bpp: {}", framebuffer_tag.framebuffer_bpp);
	writeln!(com1, "pitch: {}", framebuffer_tag.framebuffer_pitch);

	let str = "Il1egal 0O";

	let x = 16;
	let y = 16;

	// TODO: This is probably slow as heck!
	for (i, b) in str.as_bytes().iter().enumerate() {
		let mut cx = x + i * 6;
		let mut cy = y;

		// Get the glyph index in the glyph sheet. For ASCII characters it's going
		// be (code point of the character) - (code point of the space, but for the
		// Unicode characters we probably need to use glyph mapping tables to get
		// indices of the glyphs in the font.
		let index = b - b' ';

		// Get the row and column where we can find that glyph.
		let row = (index / 16) as usize;
		let column = (index % 16) as usize;

		// Glyph column * width of each glyph.
		let pixel = column * 6;

		// 13 lines of 12 bytes in each glyph line.
		let lines = FONT.chars[row * 12 * 13..(row + 1) * 12 * 13].chunks(12);

		// Loop over each of the 13 lines that are part of the single glyph line.
		for line in lines {
			// Loop over each of the 12 bytes in the line.
			for (byte_index, byte) in line.iter().enumerate() {
				// Loop over each pixel in byte.
				for pixel_index in 0..8 {
					let pixel_offset = byte_index * 8 + pixel_index;

					// Ignore these pixels which are not in the column we want.
					if !(pixel..pixel + 6).contains(&pixel_offset) {
						continue;
					}

					// Get the pixel value.
					let px = (byte >> (8 - pixel_index - 1)) & 1;

					unsafe {
						(framebuffer.add(framebuffer_tag.framebuffer_pitch as usize * cy + 4 * cx) as *mut u32).write(match px {
							0 => 0xff0000ff,
							1 => 0xffffffff,
							// Doing `& 1` operation means that this can never be anything
							// else but 0 or 1.
							_ => core::hint::unreachable_unchecked(),
						});
					}

					cx += 1;
				}
			}

			cx = x + i * 6;
			cy += 1;
		}
	}

	let memmap_tag = stivale::get_tag(info, stivale::STIVALE2_STRUCT_TAG_MEMMAP_ID);

	if memmap_tag.is_null() {
		write!(com1, "no memory app available");
		x86::hlt();
	}

	let memmap_tag = unsafe {
		&*(memmap_tag as *const stivale::stivale2_struct_tag_memmap)
	};

	// Print memory map (for debug purposes.) This way we know which areas of the
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

	let _ = writeln!(com1, "panic: {}", info);
	loop {}
}
