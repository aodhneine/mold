ENTRY(_start)
SECTIONS
{
	. = 0xffffffff80000000 + 2M;

	.stivale2hdr ALIGN(4096) : {
		KEEP(*(.stivale2hdr))
	}

	.text ALIGN(4096) : {
		*(.text)
	}

	.data ALIGN(4096) : {
		*(.data)
	}

	.rodata ALIGN(4096) : {
		*(.rodata)
	}

	.bss : {
		*(COMMON)
		*(.bss)
	}
}
