#!/bin/bash 

source `dirname $0`/assert_regimentd

# TEMPORARILY HARDWIRED TO ASSUME A GIVEN PLT INSTALLATION DIRECTORY:
#export PLTCOLLECTS=/usr/plt/collects:$WAVESCRIPTD/src
export PLTCOLLECTS=$PLTCOLLECTS:$WAVESCRIPTD/src

#(cd $WAVESCRIPTD/src; plt-r6rs ++path $WAVESCRIPTD/src $WAVESCRIPTD/src/regiment.ss $*;)
#(cd $WAVESCRIPTD/src; plt-r6rs  $WAVESCRIPTD/src/regiment.ss $*;)

plt-r6rs  $WAVESCRIPTD/src/regiment.ss $PWD $*
