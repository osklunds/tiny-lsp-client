#!/bin/bash

cargo build && RUST_BACKTRACE=1 emacs -Q --eval '(load-file "tiny-lsp-client-test.el")'
