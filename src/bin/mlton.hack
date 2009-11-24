#!/bin/bash

# NOTE, to get GC stats pass the compiled executable this flag:
# ./query.mlton.exe @MLton gc-summary --

#UNSAFE="-const 'MLton.safe false' -const 'MLton.detectOverflow false'"

#EXTRAMLTONFLAGS=$EXTRAMLTONFLAGS
#EXTRAMLTONFLAGS=$EXTRAMLTONFLAGS -profile count

#mlton $EXTRAMLTONFLAGS -const 'MLton.safe false' -const 'MLton.detectOverflow false' -default-ann 'allowFFI true' -codegen x86 $*

# [2008.06.20] Removing -cc-opt '-O3'... have to test that later.

MACHINE=`uname -m`
#if [ "$MACHINE" == "x86_64" ];
if test "$MACHINE" = "x86_64" ;
then mlton $EXTRAMLTONFLAGS -default-ann 'allowFFI true' -codegen amd64  $*;
else mlton $EXTRAMLTONFLAGS -default-ann 'allowFFI true' -codegen x86    $*;
fi

#mlton -default-ann 'allowFFI true' -codegen c -cc-opt '-O3' $*
#mlton -default-ann 'allowFFI true' -codegen amd64 $*
