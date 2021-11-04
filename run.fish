#!/usr/bin/env fish

# Replace these with your paths to QEMU and OVMF. You can find instructions how
# to build OVMF from source here:
#   https://gist.github.com/aodhneine/0dded8440c77392ecd733f7818ace411
set qemu_bin ~/src/qemu/build/qemu-system-x86_64
set ovmf_path ~/src/edk2/Build/OvmfX64/RELEASE_GCC5/FV/OVMF.fd

$qemu_bin \
  -enable-kvm \
  -m 128 \
  -bios $ovmf_path \
  -drive format=raw,file=kernel.img,if=ide \
  -serial stdio \
  -net none \
  -vga std \
