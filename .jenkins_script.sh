#!/bin/bash

source install_environment_vars

if [ "$R6RSVER" == chez ]; then

    cd src
    make 

elif [ "$R6RSVER" == plt ]; then

    cd src
    make wsparse
    make aggregated
    make pltbc

else
    echo "R6RSVER is not set to a known setting! ($R6RSVER)"
    exit 1
fi 

which regiment
regiment
