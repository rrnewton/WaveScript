#!/bin/bash

mkdir benchmarks;

export NUMOPS=1;

wsc2 copy_linear_merge.ws -o benchmarks/copy_linear_merge
wsc2 copy_tree_merge.ws -o benchmarks/copy_tree_merge
wsc2 dead_code_pruning.ws -o benchmarks/dead_code_pruning
wsc2 linear_merge.ws -o benchmarks/linear_merge
wsc2 pipeline_complex.ws -o benchmarks/pipline_complex
wsc2 pipeline_sieve_of_eratosthenes.ws -o benchmarks/pipeline_sieve_of_eratosthenes.ws
wsc2 pipline_simple.ws -o benchmarks/pipeline_simple
wsc2 simple.ws -o benchmarks/simple
wsc2 split_linear_merge.ws -o benchmarks/split_linear_merge
wsc2 split_tree_merge -o benchmarks/split_tree_merge
wsc2 tree_merge.ws -o benchmarks/tree_merge
