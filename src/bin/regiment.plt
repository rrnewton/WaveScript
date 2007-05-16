#!/bin/bash 

source `dirname $0`/assert_regimentd

EXE=$REGIMENTD/src/regiment_pltscript.exe
#EXE=$REGIMENTD/src/regiment_pltscript/bin/regiment_pltscript.exe
SCRIPT=$REGIMENTD/src/regiment_pltscript.ss

if [ -f "$EXE" ];
then exec $EXE    $0 $*
else exec $SCRIPT $0 $*
fi
