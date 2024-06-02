{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat.url = "https://flakehub.com/f/edolstra/flake-compat/1.tar.gz";
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hsPkgs = pkgs.haskell.packages."ghc965";
        hsTools = with hsPkgs; [
          ghc
          ghcid
          ormolu
          hlint
          hoogle
          haskell-language-server
          implicit-hie
          retrie
          hpack
          cabal-install
        ];
      in
      {
        formatter = pkgs.nixpkgs-fmt;
        devShells.default = pkgs.mkShell {
          shellHook = "exec zsh";
          buildInputs = [ pkgs.nil pkgs.zlib ] ++ hsTools;
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath hsTools;
        };
      });
}
