#!/bin/bash

cargo build && emacs -Q --eval '(load-file "tiny-lsp-client-test.el")'
