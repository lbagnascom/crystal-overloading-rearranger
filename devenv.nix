{ pkgs, ... }:

{
  languages.haskell = {
    enable = true;
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
  ];
}
