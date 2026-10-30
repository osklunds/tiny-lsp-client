#!/bin/bash

build() {
    docker compose up --build --force-recreate --detach
}

attach() {
    # Use bash -c as outer so that arrows and tab completion work inside
    # the container.
    # Use path:. to avoid complaints about missing git
    docker exec -it tiny-lsp-client-test bash -c \
           "cd /tiny-lsp-client/test && exec nix develop path:."
}

update_flake() {
    docker exec -it tiny-lsp-client-test bash -c \
           "cd /tiny-lsp-client/test && exec nix flake update --flake path:."
}

"$1"

