#!/bin/sh

changelog=debian/changelog
ver=$(dpkg-parsechangelog -S version)
new_ver=$(( $ver + 1 ))

cat - $changelog > ${changelog}_ <<EOF
kvm-in-a-box ($new_ver) buster; urgency=medium

  * New upstream release

 -- $DEBFULLNAME <$DEBEMAIL>  $(date -R)

EOF

mv ${changelog}_ $changelog

gbp buildpackage --git-export-dir=$HOME/dev/deb --git-builder=sbuild

deb=$(ls -vr ../kvm-in-a-box_*.deb | head -n 1)

echo $deb
