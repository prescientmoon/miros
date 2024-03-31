{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem
      (system:
        let pkgs = nixpkgs.legacyPackages.${system}.extend inputs.purescript-overlay.overlays.default;
        in
        rec {
          devShells.default = with pkgs;
            mkShell {
              buildInputs = [
                nodePackages_latest.typescript
                nodePackages_latest.ts-node
                nodejs
                purs
                spago-unstable
                purs-tidy-bin.purs-tidy-0_10_0
                purs-backend-es
                purescript-language-server
              ];
            };
        }
      );
}
