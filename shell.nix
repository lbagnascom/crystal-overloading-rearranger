# Pin nixpkgs to ensure all developers use the same version of devenv.
#
# To update: change the rev below and run:
#   nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/<NEW_REV>.tar.gz
# Then replace the sha256 with the output.
{
  pkgs ? import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/a3116115851d68b8952a2a4221cc25a84e56b532.tar.gz
";
    sha256 = "0qbzjra0z5v7fwzji7j2h7g632a5zg2934v3j4555n6xnwsx9sv7";
  }) { },
}:

pkgs.mkShell {
  buildInputs = with pkgs; [
    devenv
  ];

  shellHook = ''
    echo "devenv environment loaded"
  '';
}
