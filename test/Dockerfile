
FROM opensuse/tumbleweed:latest

ENV CARGO_TARGET_DIR=/tiny-lsp-client/target_docker

RUN zypper --non-interactive in rustup emacs clang git 
RUN rustup toolchain install 1.86 && rustup component add rust-analyzer

WORKDIR "/tiny-lsp-client/test"
