
FROM opensuse/tumbleweed:latest

ENV CARGO_TARGET_DIR=/tiny-lsp-client/target_docker

# Common
RUN zypper --non-interactive in rustup emacs clang git cmake libstdc++-devel \
    erlang wget java-25-openjdk-devel

# Rust
RUN rustup toolchain install 1.86 && rustup component add rust-analyzer

# Erlang
RUN wget "https://github.com/erlang-ls/erlang_ls/releases/download/1.1.0/erlang_ls-linux-27.tar.gz" \
    && tar -xzvf erlang_ls-linux-27.tar.gz \
    && mv erlang_ls /usr/local/bin/

# Java
RUN wget "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.56.0/jdt-language-server-1.56.0-202601291528.tar.gz" -O jdtls.tar.gz \
    && mkdir /jdtls \
    && tar -xzvf jdtls.tar.gz -C /jdtls || true \
    && ln -s /jdtls/bin/jdtls /usr/local/bin/jdtls

# Haskell
RUN wget "https://downloads.haskell.org/~ghcup/0.1.50.2/x86_64-linux-ghcup-0.1.50.2" \
    && mv x86_64-linux-ghcup-0.1.50.2 ghcup \
    && chmod +x ghcup \
    && ./ghcup install ghc 9.12.2 \
    && ./ghcup install cabal 3.14.2.0 \
    && ./ghcup install hls 2.10.0.0
# todo: fix path the proper way so that e.g. ghc version isn't hardcoded
ENV PATH="$PATH:/root/.ghcup/bin:/root/.ghcup/ghc/9.12.2/bin"

WORKDIR "/tiny-lsp-client/test"
