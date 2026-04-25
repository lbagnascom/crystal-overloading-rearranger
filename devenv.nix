{ pkgs, ... }:

{
  cachix.enable = false;

  packages = with pkgs; [
    crystal
    ghc
    ghcid
    haskell-language-server
    cabal-install
  ];
}
