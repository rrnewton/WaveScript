#!/bin/bash 

source `dirname $0`/assert_regimentd

# TEMPORARILY HARDWIRED TO ASSUME A GIVEN PLT INSTALLATION DIRECTORY:
export PLTCOLLECTS=/usr/plt/collects:$REGIMENTD/src

#(cd $REGIMENTD/src; plt-r6rs ++path $REGIMENTD/src $REGIMENTD/src/regiment.ss $*;)
#(cd $REGIMENTD/src; plt-r6rs  $REGIMENTD/src/regiment.ss $*;)

plt-r6rs  $REGIMENTD/src/regiment.ss $PWD $*
