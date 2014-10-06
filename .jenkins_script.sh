#!/bin/bash

source install_environment_vars

if [ "$WAVESCRIPTHOST" == chez ]; then

    cd src
    make 

elif [ "$WAVESCRIPTHOST" == plt ]; then

    cd src
    make wsparse
    make aggregated
    make plt

else
    echo "WAVESCRIPTHOST is not set to a known setting! ($WAVESCRIPTHOST)"
    exit 1
fi 

which regiment
echo "Testing system load/JIT time:"
time regiment

echo "Running minimal tests"
make test
