#!/bin/bash

source install_environment_vars

if [ "$REGIMENTHOST" == chez ]; then

    cd src
    make 

elif [ "$REGIMENTHOST" == plt ]; then

    cd src
    make wsparse
    make aggregated
    make plt

else
    echo "REGIMENTHOST is not set to a known setting! ($REGIMENTHOST)"
    exit 1
fi 

which regiment
echo "Testing system load/JIT time:"
time regiment

# TODO: make these tests apply to PLT also when it is in good shape:
if [ "$REGIMENTHOST" == chez ]; then
  echo "Running tests"
  make fulltest
else
  echo "WARNING WARNING WARNING"
  echo "Skipping tests under PLT.  This is temporary, FIXME."
fi
