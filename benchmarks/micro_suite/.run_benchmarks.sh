#!/bin/bash

echo "Begin running jenkins benchmark script for WaveScript..."
set -x

shift

if [ "$CHECKOUT" == "" ]; then
  CHECKOUT=`pwd`
fi
#if [ "$JENKINS_GHC" == "" ]; then
#  export JENKINS_GHC=7.6.3
#fi
if [ -f "$HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh" ]; then
  source $HOME/continuous_testing_setup/rn_jenkins_scripts/acquire_ghc.sh
fi

echo "Running benchmarks remotely on server `hostname`"

which ghc

unset GHC
unset GHC_PKG
unset CABAL

set -e

# Switch to where the benchmarks are
cd "$CHECKOUT"

make clean
make

export TRIALS=1

CID=937837560440-3fcb6n1g9eik1qp0kpvsandf7o0qncr8.apps.googleusercontent.com
SEC=qse3bKGAadEMdsAIgYdjjavF
TABID=1s9sgCMhhzgsjhAK64RXVOJmBq2mfQeHxAxQBtm0d

export NUMOPS=100
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
export NUMOPS=200
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
export NUMOPS=300
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
export NUMOPS=400
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
export NUMOPS=500
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
export NUMOPS=600
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
export NUMOPS=600
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
export NUMOPS=700
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
export NUMOPS=800
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
export NUMOPS=900
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
export NUMOPS=1000
./run_benchmarks.exe --keepgoing --trials=$TRIALS --fusion-upload --name=WaveScript_Benchmarks --clientid=$CID --clientsecret=$SEC $*
