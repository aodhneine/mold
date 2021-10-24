#!/usr/bin/env fish

function info
  printf "%s=>%s " (set_color -o blue) (set_color normal)
  printf "%s%s%s\n" (set_color -o) $argv (set_color normal)
end

set -xa PATH ~/src/qemu/build
set ovmf_path ~/src/edk2/Build/OvmfX64/RELEASE_GCC5/FV/OVMF.fd

info "running kernel in qemu with ovmf"
qemu-system-x86_64 \
  -enable-kvm \
  -m 128 \
  -bios $ovmf_path \
  -drive format=raw,file=kernel.img,if=ide \
  -serial stdio \
  -net none \
  -vga std \
