#!/bin/sh

changelog=debian/changelog
ver=$(dpkg-parsechangelog -S version)
new_ver=$(( $ver + 1 ))

cat >> $changelog <<EOF
kvm-in-a-box ($new_ver) jessie; urgency=medium

  * New upstream release

 -- $DEBFULLNAME <$DEBEMAIL>  $(date -R)

EOF

sbuild

deb=$(ls -vr ../kvm-in-a-box_*.deb | head -n 1)

echo $deb
