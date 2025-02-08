#!/bin/bash

cargo build && RUST_BACKTRACE=1 emacs \
    -Q \
    --eval '(load-file "tiny-lsp-client-interactive-test.el")'

code=$?

echo ""
echo ""
echo ""

if [ $code -eq 0 ]; then
    echo "OK"
else
    echo "FAILED"
fi
