#!/bin/bash

#TMPDAT=/tmp/duplicated_6sec_marmot_data_`date +%s`.raw
TMPDAT=/tmp/duplicated_6sec_marmot_data.raw

rm -rf $TMPDAT

  echo "  Making a big enough audio file."; echo
  cd "$REGIMENTD/apps/marmot";
  make testdata.txt
  (rm -f 6sec_marmot_sample.raw)
  # ensure that we have sample data:
  #make 6sec_marmot_sample.raw
  ./download_small_sample_data
  for ((i=0; i<100; i++)) do
    cat 6sec_marmot_sample.raw >> $TMPDAT
  done;
  rm -f 6sec_marmot_sample.raw
  ln -s $TMPDAT 6sec_marmot_sample.raw

