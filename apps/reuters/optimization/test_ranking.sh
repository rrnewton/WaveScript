#!/bin/bash

# This just tests that when we feed back in the answers we get the same value out.

TESTS="test test2 test3 test4"

MODES="latency bandwidth bottleneck"

TEMP1=`tempfile`
TEMP2=`tempfile`
TEMP3=`tempfile`
TEMP4=`tempfile`

for tst in $TESTS; do
for mode in $MODES; do
  echo 
  ./formulate_ilp.ss $mode $tst".qopt" | lp_solve > $TEMP1  
  ./read_lp_solution.ss $TEMP1 > $TEMP2
  
  echo "Answer ($mode) for \"$tst".qopt"\":"
  cat $TEMP2

  grep Value $TEMP1 > $TEMP3
  echo "  Initial solution value: "`cat $TEMP3 | sed 's/Value of objective function://'`  
  
  ./rank_solution.ss $mode $tst".qopt" $TEMP2 > $TEMP4
  echo "  Read back and reranked: "`cat $TEMP4 | sed 's/Value of objective function://'`  

  if [ "`diff $TEMP3 $TEMP4`" == "" ]; 
  then echo "  PASSED"
  else echo "  FAILED"
       exit 1
  fi

done
done

## We could be even stricter and make sure all the other variables in
## the program receive the same values...

