#!/bin/bash

send() {
    local msg="$1"
    local len
    len=$(printf '%s' "$msg" | wc -c)

    printf 'Content-Length: %d\r\n\r\n%s' "$len" "$msg"
}

send '{"id":0,"jsonrpc": "2.0","result":{}}'

sleep 1000

