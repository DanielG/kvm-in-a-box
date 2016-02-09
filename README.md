kvm-in-a-box (KIB)
==================

Single box KVM virtualization server.

Usage
-----

```
Usage: kib [--root ROOT] [-q|--quiet] COMMAND

Available options:
  --root ROOT              Use this directory instead of '/'
  -q,--quiet               Be quiet
  -h,--help                Show this help text

Available commands:
  list                     Print names of all VMs
  list-resources           Print all resource paths
  info                     Print details of a single VM
  console                  Connect to VM serial console (socat)
  create                   Create a new VM
  remove                   Remove a VM's configuration (not disk state)
  change                   Change an existing VM
  start                    Start an existing VM
  stop                     Stop an existing VM
  setup                    Perform initial environment configuration
Usage: Kip COMMAND
```

All commands also have their own --help output.

Installation
------------

First a word of caution: KIB is designed to be run on a dedicated machine, it
modifies your configuration files, _cannot_ cleanly uninstall itself (after you
have used the `setup` or `create` commands) and will probably eat your babies.

If you want to use it anyways at least install
[etckeeper](https://joeyh.name/code/etckeeper/) before using KIB so you can
revert `/etc` to a good state.

KIB can be built using the usual Haskell package installation dance however some
config files and links are setup by the debian package build that you'll have to
do manually.

### Deployment

We use KIB on a Debian 8 (jessie) system so that's where things should work. Our
deployment uses a Debian package built by *sbuild* (though other tools like
pbuilder and cowbuilder should work just as well), so for us deployment is as
simple as:

```
$ sbuild
$ scp ../kvm-in-a-box*.deb kib.example.net:
$ ssh kib.example.net -- sudo dpkg -i kvm-in-a-box*.deb
```

For more information on how to setup *sbuild* see https://wiki.debian.org/sbuild .

If you'd like a more principled approach you can set up a proper APT package
repository using something like [aptly](http://www.aptly.info/),
[reprepro](https://mirrorer.alioth.debian.org/) or even just apt-ftparchive
(which comes with APT). See also
[How To Setup A Debian Repository at the Debian Wiki](https://wiki.debian.org/HowToSetupADebianRepository)

Walkthrough
-----------

### Network setup

The following config should get you started. Fill in the appropriate addresses
though. You can use `subnetcalc fd::/64 -uniquelocal` to generate new unique
local addresses for the private and group interfaces.

The `upstream-interface` will be added to the public interface's bridge, routing
and firewall rules will be setup to let traffic to and from the public interface
pass.

Per convention the public/private bridge interfaces are called `kipubr` and
`kiprivbr` respectively.


    cat > /etc/kvm-in-a-box.cfg <<EOF
    domain = kvm-box.example.net
    upstream-interface = eth0

    public-address = 10.0.0.1/16
    public-address6 = 2001:DB8::2/64

    private-address6 = fd13:5b76:af07::2/64
    group-address6   = fded:4154:dde6::2/48
    EOF




We assume you have a LVM volume group called vg0 already, you can change the
name KIB expects using the `--vg` switch though.

Let's create a VM with access to the public network interface, some RAM and some
cores:

    admin@kvm-box $ sudo kib create foo --public --mem 1024 --cpus 2 --ssh-key "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINQdQvclJHzfvJH650Nngv6v2MZ+rNZ4rUeJx3i3MCrA demo@foobar"
    resource '/etc/udev/rules.d/50-kib.rules' missing, creating.
    resource '/etc/ssh/authorized_keys/kib-foo' missing, creating.
    resource '/etc/passwd' changed, rewriting.
    resource '/etc/shadow' changed, rewriting.
    resource '/etc/dnsmasq.d/kib' missing, creating.
    resource '/etc/dnsmasq.kib.hosts' missing, creating.
    resource '/etc/iptables/rules.v4' changed, rewriting.
    resource '/etc/iptables/rules.v6' changed, rewriting.
    resource '/etc/systemd/user/kib-vm.service' missing, creating.
    resource '/var/lib/systemd/linger/kib-vm' missing, creating.

All this command does is change a bunch of config files. We let systemd do all
the start/stop management. In KIB all VMs are run under distinct system users
(not root like some other KVM management tools that shall remain unnamed) so we
use systemd user instances to let each VMs owner manage it via the SSH console.

Lastly we need to create some storage for the VM, pretty standard stuff:

    root@kvm-box $ lvcreate vg0 -n foo -L 100G


And that's it from the hypervisor operator's point of view. Now the user who's
ssh key we added above can do the rest via the SSH console:

    user@laptop $ ssh kib-foo@kvm-box

    kvm-in-a-box VM admin console
    Commands:
      > status
      > console
      > reset
      > start
      > stop
      > enable
      > disable
      > install debian auto
      > install debian [PRESEED_URL]

    >

Entering the command `install debian auto` will start the debian-installer with
the preeseed file `preseed.cfg` from the KIB repository (TODO: make
configurable) after which the VM can be started. (TODO: support uploading
preseed files and thus the `install debian [PRESEED_URL]` command).

By default the VM will not start at boot, to make it do that just `enable` it.

    > enable

To start the VM immediately `start` it (duh):

    > start
    ● kib-foo.service - Kvm-in-a-box VM: foo
       Loaded: loaded (/etc/systemd/user/kib-foo.service; enabled)
       Active: active (running) since Mon 2016-01-25 21:20:12 CET; 1 weeks 3 days ago
     Main PID: 1234 (kib-supervise)
       CGroup: /user.slice/user-5000.slice/user@5000.service/kib-foo.service
               ├─1234 kib-supervise kib-foo /usr/bin/qemu-system-x86_64...
               └─1235 /usr/bin/qemu-system-x86_64 -sandbox on -cpu host...

To access the VMs serial console (after starting it) `console` is the command
for you:

    > console
    Type RET to get a prompt (serial console)
    Type ^] (ASCII GS) to exit console.

    Debian GNU/Linux 8 (foo) ttyS0

    (foo) login:

And that's it.

Hardening
---------

Since root is required to change the config files KIB needs to access we
recommend using sudo with a dedicated group for users allowed to administer
KIB. We use something like the following in our sudoers config:

```
Cmnd_Alias  KIB       =   /usr/sbin/kib
%kib-admin ALL=(root) NOPASSWD: KIB
```

`kib` probably has a priviledge escalation lurking in it somewhere so don't give
this to anyone you don't trust. Also don't use the `NOPASSWD` option unless you
can trust your admins to have good opsec (passphrases on their ssh key and
whatnot).

WIP: add lvm management wrappers to `kib` so you don't need to do some weird
glob stuff in sudoers to restrict admins to just the VG/LVs they should be
touching.
