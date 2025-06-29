#!/bin/bash

cargo build || exit 1

if [[ $# -eq 0 ]]; then
    echo "Running all test files"
    test_dir=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
    cd "$test_dir"
    for file in *test.el; do
        [[ -f "$file" ]] || break
        echo "Running test file: $file"
        emacs -batch -l ert -l "$file" --eval '(ert-run-tests-batch-and-exit)' || break
    done
fi

if [[ $# -eq 1 ]]; then
    echo "Running one test file: $1"
    emacs -batch -l ert -l "$1" --eval '(ert-run-tests-batch-and-exit)'
fi

if [[ $# -eq 2 ]]; then
    echo "Running test case $2 in test file: $1"
    emacs -batch -l ert -l "$1" --eval "(ert-run-tests-batch-and-exit \"$2\")"
fi

if [[ $# -gt 2 ]]; then
    echo "Wrong number of arguments"
fi

