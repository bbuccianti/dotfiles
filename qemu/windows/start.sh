#!/bin/sh

export SDL_VIDEO_X11_DGAMOUSE=0

qemu-system-x86_64 \
    -enable-kvm \
    -m 3072 \
    -cpu host,hv_relaxed,hv_spinlocks=0x1fff,hv_vapic,hv_time \
    -net user -net nic \
    -drive file=windows.qcow2,media=disk,if=virtio,aio=native,cache.direct=on \
    -drive file=virtio.iso,media=cdrom \
    #-drive file=windows.iso,media=cdrom \
    -usbdevice tablet \
    -vga virtio \
    -display sdl,gl=on
