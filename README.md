# mold

This supposedly does something.

## Building

This project is using a simple build script written in fish, which automates
the process of compiling the kernel and building a bootable disk image. Simply
run `./build.fish` to use it.

#### Dependencies

To run the build script, you need fish itself, as the build script is written
in it. You also need `dd`, `sfdisk`, `losetup`, `mkfs.fat` and `mount`, which
should be already installed on your system if you're running Linux.

Otherwise, you can find `dd` in `coreutils` package, `sfdisk`, `losetup` and
`mount` in `util-linux`, and `mkfs.fat` in `dosfstools`.

To compile mold for amd64, you need nightly rustc compiler and `rust-src`
component. It is alread specified in the `rust-toolchain.toml` file, so you
don't need to install it manually.

## Running

You can use `./run.fish` to simplify running the kernel in the QEMU. You need
to have `qemu-system-x86_64` and OVMF installed. You probably want to configure
paths to them, see inside [`run.fish`](./run.fish) file for how.
