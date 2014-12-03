#!/bin/bash
export NUMOPS=1;

rm -rf benchmarks/ benchmarks.tgz

mkdir benchmarks;

wsc2 copy_linear_merge.ws -o benchmarks/copy_linear_merge
cp copy_linear_merge.ws benchmarks/copy_linear_merge.ws

wsc2 copy_tree_merge.ws -o benchmarks/copy_tree_merge
cp copy_tree_merge.ws benchmarks/copy_tree_merge.ws

wsc2 dead_code_pruning.ws -o benchmarks/dead_code_pruning
cp dead_code_pruning.ws benchmarks/dead_code_pruning.ws

wsc2 linear_merge.ws -o benchmarks/linear_merge
cp linear_merge.ws benchmarks/linear_merge.ws

wsc2 pipeline_complex.ws -o benchmarks/pipeline_complex
cp pipeline_complex.ws benchmarks/pipeline_complex.ws

wsc2 pipeline_sieve_of_eratosthenes.ws -o benchmarks/pipeline_sieve_of_eratosthenes.ws
cp pipeline_sieve_of_eratosthenes.ws benchmarks/pipeline_sieve_of_eratosthenes.ws

wsc2 pipeline_simple.ws -o benchmarks/pipeline_simple
cp pipeline_simple.ws benchmarks/pipeline_simple.ws

wsc2 simple.ws -o benchmarks/simple
cp simple.ws benchmarks/simple.ws

wsc2 split_linear_merge.ws -o benchmarks/split_linear_merge
cp split_linear_merge.ws benchmarks/split_linear_merge.ws

wsc2 split_tree_merge.ws -o benchmarks/split_tree_merge
cp split_tree_merge.ws benchmarks/split_tree_merge.ws

wsc2 tree_merge.ws -o benchmarks/tree_merge
cp tree_merge.ws benchmarks/tree_merge.ws

rm benchmarks/*.exe

tar -cvzf benchmarks.tgz benchmarks/
