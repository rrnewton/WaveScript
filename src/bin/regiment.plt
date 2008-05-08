#!/bin/bash 

source `dirname $0`/assert_regimentd

# TEMPORARILY HARDWIRED:
export PLTCOLLECTS=/usr/plt_svn/collects:$REGIMENTD/src

#(cd $REGIMENTD/src; plt-r6rs ++path $REGIMENTD/src $REGIMENTD/src/regiment.ss $*;)
#(cd $REGIMENTD/src; plt-r6rs  $REGIMENTD/src/regiment.ss $*;)

plt-r6rs  $REGIMENTD/src/regiment.ss $PWD $*
