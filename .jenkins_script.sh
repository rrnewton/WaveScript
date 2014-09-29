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

echo "Running minimal tests"
make test
