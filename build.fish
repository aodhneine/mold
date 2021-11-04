#!/usr/bin/env fish

function info
  printf '%s=>%s ' (set_color -o blue) (set_color normal)
  printf '%s%s%s\n' (set_color -o) $argv (set_color normal)
end

info "building kernel"
cargo build

set image "kernel.img"

# Default image size is 48 MB, split into blocks of 512 bytes.
set imagesize (math '48 * 1000 * 1000')
set countsize (math "$imagesize / 512")

dd if=/dev/zero of=$image bs=512 count=$countsize

# Got this from running 'sfdisk -d /dev/sda'.
set sfdiskefiparttype 'C12A7328-F81F-11D2-BA4B-00A0C93EC93B'
set sfdiskscript "label: gpt
unit: sectors

start=2048, type=$sfdiskefiparttype"

info "formatting $image"
echo $sfdiskscript | sfdisk $image

info "fetching available loop devices"
set loopdevice (sudo losetup -f)
info "setting up loop device at $loopdevice"
sudo losetup --offset (math '2048 * 512') --sizelimit (math "$countsize * 512 - 2048 * 512") $loopdevice $image

info "formatting image with fat32"
sudo mkfs.fat -F 32 $loopdevice

set mntdir (mktemp -d)
info "mounting loop device at $mntdir"
sudo mount $loopdevice $mntdir

info "creating EFI directories"
sudo mkdir -pv $mntdir/EFI/BOOT

info "copying files into mount point"
sudo cp -v limine/limine-source/bin/BOOTX64.EFI $mntdir/EFI/BOOT
sudo cp -v limine/limine.cfg $mntdir
sudo cp -v target/x86_64-mold/debug/mold $mntdir

info "unmounting and detaching loop device $loopdevice"
sudo umount $mntdir
rmdir $mntdir
sudo losetup --detach $loopdevice
