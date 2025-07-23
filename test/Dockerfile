
FROM opensuse/tumbleweed:latest

ENV CARGO_TARGET_DIR=/tiny-lsp-client/target_docker

RUN zypper --non-interactive in rustup emacs clang git cmake libstdc++-devel \
    erlang wget
RUN rustup toolchain install 1.86 && rustup component add rust-analyzer
RUN wget https://github.com/erlang-ls/erlang_ls/releases/download/1.1.0/erlang_ls-linux-27.tar.gz \
    && tar -xzvf erlang_ls-linux-27.tar.gz \
    && mv erlang_ls /usr/local/bin/

WORKDIR "/tiny-lsp-client/test"
