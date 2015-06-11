sudo vgchange -an kib || true
sudo losetup -d /dev/loop0 || true

dd if=/dev/zero of=lvmtest0.img bs=1000 count=1M

sudo losetup /dev/loop0 lvmtest0.img
sudo pvcreate /dev/loop0
sudo vgcreate kib /dev/loop0

sudo lvcreate -L 100M -n vm0 kib
sudo lvcreate -L 100M -n vm1 kib
sudo lvcreate -L 100M -n vm2 kib
