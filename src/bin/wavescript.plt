#!/bin/bash 

source `dirname $0`/assert_wavescriptd

# TEMPORARILY HARDWIRED TO ASSUME A GIVEN PLT INSTALLATION DIRECTORY:
#export PLTCOLLECTS=/usr/plt/collects:$WAVESCRIPTD/src
export PLTCOLLECTS=$PLTCOLLECTS:$WAVESCRIPTD/src

#(cd $WAVESCRIPTD/src; plt-r6rs ++path $WAVESCRIPTD/src $WAVESCRIPTD/src/wavescript.ss $*;)
#(cd $WAVESCRIPTD/src; plt-r6rs  $WAVESCRIPTD/src/wavescript.ss $*;)

plt-r6rs  $WAVESCRIPTD/src/wavescript.ss $PWD $*
