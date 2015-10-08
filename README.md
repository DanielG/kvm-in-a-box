kvm-in-a-box (KIB)
==================

Single box KVM virtualization server.


Usage
-----

```
Usage: kib COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  list                     Print names of all VMs
  list-resources           Print all resource paths
  info                     Print details of a single VM
  console                  Connect to VM serial console (socat)
  create                   Create a new VM
  destroy                  Destroy an existing VM
  change                   Change an existing VM
  start                    Start an existing VM
  stop                     Stop an existing VM
  setup                    Perform initial envirnment configuration
```

All commands also have their own --help output.

Walkthrough (WIP)
-----------------

Network setup

    cat > /etc/kvm-in-a-box.cfg <<EOF
    domain = kvm-box.example.net
    upstream-interface = eth0

    public-address = 10.0.0.1/16
    public-address6 = 2001:DB8::2/64

    private-address6 = fd13:5b76:af07::2/64
    group-address6   = fded:4154:dde6::2/48
    EOF

We assume you have a LVM volume group called vg0 already, you can change the
name kib expects using the `--vg` switch though.

    root@kvm-box $ lvcreate vg0 -n foo -L 2G
    root@kvm-box $ cat debian-7.img > /dev/vg0/foo
    root@kvm-box $ kib create foo --public --mem 1024 --cpus 2
    root@kvm-box $ kib key add foo <<EOF
    ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDbkFdWWgJ76Nzg3F8WvnUmC3skEmY5AlkE6ah0ssDdVihUc6H2UVhdmaZTUg7mCkRSqsVVKAfILkOLcXKpBGzeTUkN312v2HOfj5LskdFjmQPhf+3vAIYHnUg/fptXkRd+9OMSdSwBv8ej2/BH9aRo+aoPvM1kw/ZlMDQAA83sIgvzqqcXKneX7KUAzplh05igMmxp9+EcZ2DEH343VD9jC7BXeF79m83nmLblyqwCS5Jty/dMqJGsSv6Z/eHpOSsDV0kBTWGZhHuZmvYaqHMYCznjZLJemJ/ipsjYn4UKNG5U7aN2Za2yKOOr8JkmE71+Ty2rbPM+Y+5LJy15Bc+P user@laptop
    EOF
    root@kvm-box $ systemctl enable kib-foo && systemctl start kib-foo

You can now log into the VM's serial console via ssh:

    user@laptop $ ssh kib-foo@kvm-box
    kvm-in-a-box VM admin console
    Commands:
      > console
      > reset

    > console
    Type RET to get a prompt (serial console)
    Type ^] (ASCII GS) to exit console.

    Debian GNU/Linux 7 (none) ttyS0

    (none) login:
