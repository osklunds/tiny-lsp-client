{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in {
      devShells.${system}.default = pkgs.mkShell {
        packages = with pkgs; [
          emacs
          rustc
          cargo
          rust-analyzer

          clang-tools
          llvmPackages.libclang

          jdk
          jdt-language-server

          beam29Packages.erlang
          erlang-language-platform

          ghc
          haskell-language-server
        ];

        LIBCLANG_PATH = "${pkgs.llvmPackages.libclang.lib}/lib";
      };
    };
}
