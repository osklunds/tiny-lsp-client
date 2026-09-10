#!/bin/bash

send() {
    local msg="$1"
    local len
    len=$(printf '%s' "$msg" | wc -c)

    printf 'Content-Length: %d\r\n\r\n%s' "$len" "$msg"
}

send '{"id":0,"jsonrpc": "2.0","result":{}}'

send '{"id":1,"jsonrpc": "2.0","result":[
    {
      "range": {
        "end": {
          "character": 10,
          "line": 4
        },
        "start": {
          "character": 10,
          "line": 4
        }
      },
      "uri": "file:///tiny-lsp-client/test/clangd/main.cpp"
    }
  ]}'

send '{
  "error": {
    "code": -32801,
    "message": "some error"
  },
  "id": 2,
  "jsonrpc": "2.0"
}'

send '{"id":3,"jsonrpc": "2.0","result":[
    {
      "range": {
        "end": {
          "character": 5,
          "line": 2
        },
        "start": {
          "character": 5,
          "line": 2
        }
      },
      "uri": "file:///tiny-lsp-client/test/clangd/main.cpp"
    }
  ]}'

sleep 1000

