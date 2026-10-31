#!/bin/bash

# If you update flake.nix and run attach without build, the "nix develop"
# step is redone each time, which is time consuming, so keep that in mind.

build() {
    docker compose up --build --force-recreate --detach
}

start() {
    docker compose up --force-recreate --detach
}

# As is visible, this cleans more than just tiny-lsp-client, so it's crude.
clean() {
    docker builder prune -a -f
    docker image prune -a -f
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

