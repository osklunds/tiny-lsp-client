
FROM nixos/nix@sha256:7a007c766426c1877758ddc5cb87a965ac131fc78c582ce0083d922d51ae945c

RUN mkdir -p /etc/nix && \
    echo "experimental-features = nix-command flakes" \
    >> /etc/nix/nix.conf

COPY flake.nix flake.lock /tmp

WORKDIR "/tmp"

RUN nix develop

RUN wget "https://github.com/erlang-ls/erlang_ls/releases/download/1.1.0/erlang_ls-linux-27.tar.gz" \
    && tar -xzvf erlang_ls-linux-27.tar.gz \
    && mv erlang_ls /root/.nix-profile/bin/

ENV CARGO_TARGET_DIR=/tiny-lsp-client/target_docker

