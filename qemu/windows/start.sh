#!/bin/sh

qemu-system-x86_64 \
    -enable-kvm \
    -m 3072 \
    -net user -net nic \
    -drive file=windows.qcow2,media=disk,if=virtio \
    -drive file=virtio.iso,media=cdrom \
    #-drive file=windows.iso,media=cdrom \
    -vga virtio \
    -display sdl,gl=on
