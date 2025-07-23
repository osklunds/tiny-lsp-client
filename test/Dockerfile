
FROM opensuse/tumbleweed:latest

ENV CARGO_TARGET_DIR=/tiny-lsp-client/target_docker

RUN zypper --non-interactive in rustup emacs clang git cmake libstdc++-devel \
    erlang wget java-24-openjdk-devel
RUN rustup toolchain install 1.86 && rustup component add rust-analyzer
RUN wget "https://github.com/erlang-ls/erlang_ls/releases/download/1.1.0/erlang_ls-linux-27.tar.gz" \
    && tar -xzvf erlang_ls-linux-27.tar.gz \
    && mv erlang_ls /usr/local/bin/
RUN wget "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.46.0/jdt-language-server-1.46.0-202503271314.tar.gz" -O jdtls.tar.gz \
    && mkdir /jdtls \
    && tar -xzvf jdtls.tar.gz -C /jdtls || true \
    && ln -s /jdtls/bin/jdtls /usr/local/bin/jdtls

WORKDIR "/tiny-lsp-client/test"
