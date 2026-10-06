#!/bin/bash

build() {
    docker compose up --build --force-recreate --detach
}

attach() {
    docker exec -it tiny-lsp-client-test nix develop \
           --command bash -c 'cd /tiny-lsp-client && exec bash'
}

"$1"

