{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      in {
          devShells.${system}.default =
      (pkgs.mkShell.override {
          stdenv = pkgs.llvmPackages.stdenv;
        }) {
        packages = with pkgs; [
          emacs
          rustc
          cargo
          rust-analyzer

          # Clang compiler
          llvmPackages.clang

          # libclang.so, seemed to be needed
          llvmPackages.libclang

          # clangd
          clang-tools

          # jdk
          # jdt-language-server

          # beam29Packages.erlang
          # erlang-language-platform

          # ghc
          # haskell-language-server
        ];

        LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
        TLC_EMACS_MODULE_HEADER_PATH = "${pkgs.emacs}/include/emacs-module.h";
      };
    };
}
