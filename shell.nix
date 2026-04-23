{
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-25.11.tar.gz") { },
}:

pkgs.mkShell {
  name = "crystal-rearranger";

  buildInputs = with pkgs; [
    crystal
    ghc
    ghcid
    haskell-language-server
    cabal-install
  ];

  shellHook = ''
    echo "dev shell ready"
  '';
}
