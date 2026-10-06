#!/bin/bash

build() {
    docker compose up --build --force-recreate --detach
}

attach() {
    docker exec -it tiny-lsp-client-test nix develop
}

"$1"

