#!/bin/sh

#mlton -default-ann 'allowFFI true' -codegen c -cc-opt '-O3' $*
mlton -default-ann 'allowFFI true' -codegen x86 $*


# -const 'MLton.safe false' -const 'MLton.detectOverflow false' 

