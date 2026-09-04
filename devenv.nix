{ pkgs, ... }:

{
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc9123;
    cabal.enable = true;
    lsp.enable = true;
  };

  languages.crystal = {
    enable = true;
    lsp.enable = true;
  };

  packages = [
    pkgs.ghcid
    pkgs.ormolu
    pkgs.haskellPackages.cabal-gild
  ];
}
