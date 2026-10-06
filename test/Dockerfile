
FROM nixos/nix@sha256:7a007c766426c1877758ddc5cb87a965ac131fc78c582ce0083d922d51ae945c

RUN mkdir -p /etc/nix && \
    echo "experimental-features = nix-command flakes" \
    >> /etc/nix/nix.conf

WORKDIR "/test"

# RUN nix develop

