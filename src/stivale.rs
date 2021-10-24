#[repr(C)]
#[allow(non_camel_case_types)]
pub struct stivale2_header {
	pub entry_point: u64,
	pub stack: *const u8,
	pub flags: u64,
	pub tags: *const stivale2_header_tag,
}

unsafe impl Sync for stivale2_header {}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct stivale2_header_tag {
	pub identifier: u64,
	pub next: *const stivale2_header_tag,
}

unsafe impl Sync for stivale2_header_tag {}

#[repr(C)]
#[allow(non_camel_case_types)]
pub struct stivale2_header_tag_framebuffer {
	pub tag: stivale2_header_tag,
	pub framebuffer_width: u64,
	pub framebuffer_height: u64,
	pub framebuffer_bpp: u64,
	pub unused: u64,
}

pub const STIVALE2_HEADER_TAG_FRAMEBUFFER_ID: u64 = 0x3ecc1bc43d0f7971;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
pub struct stivale2_struct {
	/// 0-terminated ASCII bootloader brand string.
	pub bootloader_brand: [u8; 64],
	/// 0-terminated ASCII bootloader version string.
	pub bootloader_version: [u8; 64],
	/// Address of the first of the linked list of tags.
	pub tags: *const stivale2_struct_tag,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
pub struct stivale2_struct_tag {
	pub identifier: u64,
	pub next: u64,
}

#[repr(C)]
#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types)]
pub struct stivale2_struct_tag_framebuffer {
	pub tag: stivale2_struct_tag,
	/// Address of the buffer containing framebuffer pixels.
	pub framebuffer_addr: u64,
	// Width and height of the framebuffer.
	pub framebuffer_width: u16,
	pub framebuffer_height: u16,
	/// Offset in the framebuffer in bytes to go one pixel down.
	pub framebuffer_pitch: u16,
	/// Bits per pixel. You can get the amounts of bytes in each pixel by dividing
	/// this value by 8.
	pub framebuffer_bpp: u16,
	/// Value of 1 represents RGB. Other values are undefined.
	pub memory_model: u8,
	// Values specifing how to format colours in RGB format to display them.
	pub red_mask_size: u8,
	pub red_mask_shift: u8,
	pub green_mask_size: u8,
	pub green_mask_shift: u8,
	pub blue_mask_size: u8,
	pub blue_mask_shift: u8,
	// Padding.
	unused: u8,
}

pub const STIVALE2_STRUCT_TAG_FRAMEBUFFER_ID: u64 = 0x506461d2950408fa;

#[repr(u32)]
#[derive(Debug, Clone, Copy)]
#[allow(non_camel_case_types, dead_code)]
pub enum stivale2_mmap_type {
	USABLE = 1,
	RESERVED = 2,
	ACPI_RECLAIMABLE = 3,
	ACPI_NVS = 4,
	BAD_MEMORY = 5,
	BOOTLOADER_RECLAIMABLE = 0x1000,
	KERNEL_AND_MODULES = 0x1001,
	FRAMEBUFFER = 0x1002,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
#[allow(non_camel_case_types)]
pub struct stivale2_mmap_entry {
	pub base: u64,
	pub length: u64,
	pub ty: stivale2_mmap_type,
	// Padding.
	_unused: u32,
}

#[repr(C)]
#[derive(Debug, Copy, Clone)]
#[allow(non_camel_case_types)]
pub struct stivale2_struct_tag_memmap {
	pub tag: stivale2_struct_tag,
	// Amount of entries in the memmap array; also represents the size of that
	// array, since it's a flexible array member (see below.)
	pub entries: u64,
	// This represents a flexible array member, an array whose size is known only
	// at runtime. This is obviously not a concept in rust, so we have to emulate
	// it with an array of size 0.
	//
	// C99 [6.7.2.1], §16: As a special case, the last element of a structure
	// with more than one named member may have an incomplete array type; this
	// is called a flexible array member.
	pub memmap: [stivale2_mmap_entry; 0],
}

pub const STIVALE2_STRUCT_TAG_MEMMAP_ID: u64 = 0x2187f79e8612de07;

// -=- High-level wrappers for stivale 2 boot protocol. -=-

pub fn get_tag(info: *const stivale2_struct, id: u64) -> *const stivale2_struct_tag {
	let mut tag = unsafe {
		&*info
	}.tags as *const stivale2_struct_tag;

	loop {
		if tag.is_null() {
			return core::ptr::null();
		}

		let t = unsafe {
			&*tag
		};

		if t.identifier == id {
			return tag;
		}

		tag = t.next as *const stivale2_struct_tag;
	}
}
