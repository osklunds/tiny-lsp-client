#!/bin/bash

test_dir=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )
cd "$test_dir"
cd .. # now in repo dir

cargo build

emacs -Q --eval "(load-file \"test/interactive.el\")"
