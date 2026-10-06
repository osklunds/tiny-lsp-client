
# Latest as of 2026-08-28 17:30 CET
FROM opensuse/tumbleweed@sha256:e395f958f9fd2b11eb4a113a108c3f8b6bf66c1d452308c196c529c6d56007e8

ENV CARGO_TARGET_DIR=/tiny-lsp-client/target_docker

# Common
# Need gcc-c++ for g++ command, needed for ghcup
RUN zypper --non-interactive in rustup emacs clang git cmake gcc-c++ \
    erlang wget

# Rust
RUN rustup toolchain install 1.93 && rustup component add rust-analyzer

# Erlang
RUN wget "https://github.com/erlang-ls/erlang_ls/releases/download/1.1.0/erlang_ls-linux-27.tar.gz" \
    && tar -xzvf erlang_ls-linux-27.tar.gz \
    && mv erlang_ls /usr/local/bin/

# Java
# jdtls doesn't start. Quits immediately.
# RUN wget "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.56.0/jdt-language-server-1.56.0-202601291528.tar.gz" -O jdtls.tar.gz \
#     && mkdir /jdtls \
#     && tar -xzvf jdtls.tar.gz -C /jdtls || true \
#     && ln -s /jdtls/bin/jdtls /usr/local/bin/jdtls

# Haskell
RUN wget "https://downloads.haskell.org/~ghcup/0.1.50.2/x86_64-linux-ghcup-0.1.50.2" \
    && mv x86_64-linux-ghcup-0.1.50.2 ghcup \
    && chmod +x ghcup \
    && ./ghcup install ghc 9.12.2 \
    && ./ghcup install cabal 3.14.2.0 \
    && ./ghcup install hls 2.10.0.0

# Erlang - elp
RUN wget "https://github.com/WhatsApp/erlang-language-platform/releases/download/2026-08-10/elp-linux-x86_64-unknown-linux-gnu-otp-28.5.tar.gz" \
    && tar -xzvf elp-linux-x86_64-unknown-linux-gnu-otp-28.5.tar.gz \
    && mv elp /usr/local/bin/

ENV PATH="$PATH:/root/.ghcup/bin:/root/.ghcup/ghc/9.12.2/bin"

WORKDIR "/tiny-lsp-client/test"
