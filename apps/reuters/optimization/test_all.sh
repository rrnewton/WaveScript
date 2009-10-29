#!/bin/bash

TESTS="test test2 test3"

for tst in $TESTS; do
  echo 
  ./formulate_ilp.ss $tst".qopt" | lp_solve | ./read_lp_solution.ss
done
