#!/usr/bin/env fish

function info
  printf "%s=>%s " (set_color -o blue) (set_color normal)
  printf "%s%s%s\n" (set_color -o) $argv (set_color normal)
end

# Build the kernel (ELF file).
info "building kernel"
cargo build

# Configure loopback device to setup setup the bootloader and kernel properly.
info "mounting loopback device"
sudo losetup --offset 1048576 --sizelimit 46951424 /dev/loop0 kernel.img

info "mounting disk image"
sudo mount /dev/loop0 /mnt

# Copy kernel into the disk image.
info "copying kernel into disk image"
sudo cp target/x86_64-mold/debug/mold /mnt/

info "detaching disk image and loopback device"
sudo "umount /mnt && losetup --detach /dev/loop0"
