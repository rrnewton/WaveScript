#!/bin/sh

# NOTE, to get GC stats pass the compiled executable this flag:
# ./query.mlton.exe @MLton gc-summary 

#UNSAFE="-const 'MLton.safe false' -const 'MLton.detectOverflow false'"

#EXTRAMLTONFLAGS=$EXTRAMLTONFLAGS
#EXTRAMLTONFLAGS=$EXTRAMLTONFLAGS -profile count

#mlton $EXTRAMLTONFLAGS -const 'MLton.safe false' -const 'MLton.detectOverflow false' -default-ann 'allowFFI true' -codegen x86 $*

mlton -default-ann 'allowFFI true' -codegen x86 -cc-opt '-O3' $*
#mlton -default-ann 'allowFFI true' -codegen c -cc-opt '-O3' $*
#mlton -default-ann 'allowFFI true' -codegen amd64 $*
