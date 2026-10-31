#!/bin/bash

# If you update flake.nix and run attach without build, the "nix develop"
# step is redone each time, which is time consuming, so keep that in mind.
# Also after updating flake.nix need to run update_flake to update flake.lock

build() {
    docker compose up --build --force-recreate --detach
}

stop() {
    docker compose stop
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

