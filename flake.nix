{
  description = "Crystal rearranger dev shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  };

  outputs =
    { nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        name = "crystal-rearranger";

        buildInputs = with pkgs; [
          crystal
          crystalline
          shards
          ameba-ls

          ghc
          ghcid
          haskell-language-server
          cabal-install
        ];

        shellHook = ''
          echo "dev shell ready"
        '';
      };
    };
}
