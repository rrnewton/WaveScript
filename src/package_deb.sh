#!/bin/bash

# This packages it using Petit Chez.

# Next I need to make it work with ikarus.

#make clean wsparse ikarus boot

#SCRIPTSTART=`pwd`

DEBDIR=$REGIMENTD/deb_pkg

## TODO: Set the version automatically!!
VER=0.9.0

echo
echo PACKAGING REGIMENT/WAVESCRIPT AS A .DEB 
echo =================================================================
echo Output directed to $DEBDIR

rm -rf $DEBDIR
mkdir -p $DEBDIR/data/

WSDIR=$DEBDIR/data/usr/lib/wavescript/$VER

mkdir -p $WSDIR/depends
mkdir -p $WSDIR/src/

cp -pr $REGIMENTD/install_environment_vars $WSDIR/
cp -pr $REGIMENTD/src/bin                  $WSDIR/
cp -pr $REGIMENTD/src/build                $WSDIR/
cp -pr $REGIMENTD/lib                      $WSDIR/
cp -pr $REGIMENTD/src/linked_lib           $WSDIR/src/
cp -pr $REGIMENTD/depends/petite_chez_scheme*  $WSDIR/depends/

mkdir -p $DEBDIR/data/usr/bin

for cmd in ws wsc2 wsmlton; do
 ln -s /usr/lib/wavescript/$VER/bin/$cmd $DEBDIR/data/usr/bin/
done

echo "2.0" > $DEBDIR/debian-binary

cd $DEBDIR/

cat > postinst <<EOF
#!/bin/sh
EOF

cat > prerm <<EOF
#!/bin/sh
EOF

chmod +x postinst prerm

ARCH=`uname -m`

## HMM, not sure what the right way to do this is:
if [ "$ARCH" = "i686" ]; 
then ARCH=i386
fi

cat > conffiles <<EOF
Package: wavescript
Version: 0.9
Section: devel
Priority: optional
Architecture: $ARCH
Essential: no
Depends: libc6 (>= 2.6-1), libgmp3c2, libc6-dev, libgmp3-dev (>= 4.0.1), gcc, mzscheme (>= 1:370)
Installed-Size: 8192
Maintainer: Ryan Newton [ryan.newton@alum.mit.edu]
Conflicts: 
Replaces: 
Provides: wavescript
Description: High performance distributed/parallel stream-processing language & compiler
WaveScript (wavescope.csail.mit.edu) is a functional language for
writing highly abstract programs that generate efficient dataflow
graphs through a two-stage evaluation model.  This graph intermediate
language is executable in several backends (Scheme, ML, Java[ME],
C/C++, TinyOS) that enable support for various embedded platforms and
offer different tradeoffs in features, performance, portability,
compile times, and parallel/distributed execution.  WaveScript is
primarily used for programming sensor networks, or for parallel
computation on multicores or clusters.  Includes a C FFI and various
libraries.
EOF

find $DEBDIR -name "*.svn" | xargs rm -rf 
find $DEBDIR -name "*~" | xargs rm -rf 
find $DEBDIR -name "#*" | xargs rm -rf 
find $DEBDIR -name "_*" | xargs rm -rf 

cd $DEBDIR/data
find -type f | xargs md5sum > ../md5sums
tar czf data.tar.gz *

cd $DEBDIR
mv $DEBDIR/data/data.tar.gz $DEBDIR/
rm -rf $DEBDIR/data/

tar czf control.tar.gz conffiles postinst prerm md5sums
rm -f conffiles postinst prerm md5sums

