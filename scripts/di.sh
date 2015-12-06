#!/bin/sh

if [ -z "$1" ]; then
    echo "Usage: $0 debian-*.iso [PRESEED_FILE]">&2
    exit 1
fi

tmpdir=$(mktemp -p "${TMPDIR:-/tmp/}" -d di-XXXX) || exit 1
trap 'rm -r '"$tmpdir" 0 2 15

if [ -n "$2" ]; then
    cp "$2" "$tmpdir"
    PRESEED="auto=true priority=critical url=tftp://10.0.2.2/$(basename $2)"
fi

(
    cd "$tmpdir"
    7z x "$1" install.amd
    if [ ! -d install.amd ]; then
        echo "Extracting Debian installer kernel+inird failed">&2
        exit 1b
    fi
)

qemu-system-x86_64 -nographic -vga none -cpu host -machine pc,accel=kvm -m 1024 -smp 4 -cdrom "$1" -kernel $tmpdir/install.amd/vmlinuz -initrd $tmpdir/install.amd/initrd.gz -append "console=ttyS0,9600 $PRESEED" -net user,tftp="$tmpdir" -net nic -hda /tmp/di.img
