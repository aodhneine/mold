#!/usr/bin/env fish

function info
  printf '%s=>%s ' (set_color -o blue) (set_color normal)
  printf '%s%s%s\n' (set_color -o) $argv (set_color normal)
end

info "cloning limine source code"
git clone https://github.com/limine-bootloader/limine --depth 1

pushd limine

# For some reason I can't build limine using clang anymore.
info "building limine-uefi"
make limine-uefi -j17

popd
