#!/bin/bash

#PREFIX=bhatta
#preindex=000

PREFIX=my_int
PREINDEX=_

echo Reference file: processed/Feeder*/"$PREFIX"*/Diff*"$PREINDEX"0.*bmp 

hexdump processed/Diff_0.bmp > processed/dumpws.txt
#hexdump processed/Feeder*/"$PREFIX"*/Diff_FeederStation_2007-06-26_14-00-03.000_00000000.jpg.bmp > processed/dumpcpp.txt
hexdump processed/Feeder*/"$PREFIX"*/Diff*"$PREINDEX"0.*bmp > processed/dumpcpp.txt
diff processed/dumpws.txt processed/dumpcpp.txt
