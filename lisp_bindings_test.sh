#!/bin/bash

cargo build --release && RUST_BACKTRACE=1 emacs \
    -Q \
    --batch \
    --eval '(load-file "tiny-lsp-client-lisp-bindings-test.el")'

code=$?

echo ""
echo ""
echo ""

if [ $code -eq 0 ]; then
    echo "OK"
else
    echo "FAILED"
fi
