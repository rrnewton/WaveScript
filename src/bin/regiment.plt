#!/bin/bash 

source `dirname $0`/assert_regimentd

SCRIPT=$REGIMENTD/src/regiment_pltscript.ss
EXE=$REGIMENTD/src/regiment_pltscript.exe

if [ -f "$EXE" ];
then exec $EXE    $0 $*
else exec $SCRIPT $0 $*
fi
