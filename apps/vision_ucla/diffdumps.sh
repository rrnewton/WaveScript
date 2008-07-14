#!/bin/bash

PREFIX=bhatta

hexdump processed/Diff_0.bmp > processed/dumpws.txt
hexdump processed/FeederStation_2007-06-26_14-00-03.000/"$PREFIX"3_20_1_0_128_16_2_2_30_0_bl/Diff_FeederStation_2007-06-26_14-00-03.000_00000000.jpg.bmp > processed/dumpcpp.txt
diff processed/dumpws.txt processed/dumpcpp.txt
