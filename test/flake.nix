{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
      in {
          devShells.${system}.default =
      (pkgs.mkShell.override {
          stdenv = pkgs.llvmPackages_23.stdenv;
        }) {
        packages = with pkgs; [
          emacs
          rustc
          cargo
          rust-analyzer

          # utf-8 file names (system-wide)
          glibcLocales

          cmake

          # Clang compiler
          llvmPackages_23.clang

          # libclang.so, seemed to be needed
          llvmPackages_23.libclang

          # clangd
          llvmPackages_23.clang-tools

          jdk
          jdt-language-server

          beam29Packages.erlang
          erlang-language-platform

          # ghc
          # haskell-language-server
        ];

        LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
        TLC_EMACS_MODULE_HEADER_PATH = "${pkgs.emacs}/include/emacs-module.h";

        LANG = "en_US.UTF-8";
        LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
      };
    };
}
