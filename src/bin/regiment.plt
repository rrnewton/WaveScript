#!/bin/bash 

source `dirname $0`/assert_regimentd

$REGIMENTD/src/regiment_pltscript.ss $0 $*
