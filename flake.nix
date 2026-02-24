{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    mkSpagoDerivation.url = "github:jeslie0/mkSpagoDerivation";
    purescript-overlay = {
      url = "github:thomashoneyman/purescript-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        base = nixpkgs.legacyPackages.${system};
        spagoOverlay = inputs.mkSpagoDerivation.overlays.default;
        pursOverlay = inputs.purescript-overlay.overlays.default;
        pkgs = (base.extend spagoOverlay).extend pursOverlay;

        binDeps = [
          pkgs.stylua
          pkgs.nodejs
        ];

        miros = pkgs.mkSpagoDerivation {
          spagoYaml = ./spago.yaml;
          spagoLock = ./spago.lock;
          src = ./.;
          version = "unstable-2026-02-24";
          nativeBuildInputs = [
            pkgs.purs-unstable
            pkgs.spago-unstable
            pkgs.esbuild
            pkgs.makeWrapper
            pkgs.purs-backend-es
            pkgs.nodejs
          ];

          buildPhase = ''
            spago bundle --platform node
          '';

          doCheck = true;
          checkPhase = ''
            runHook preCheck
            spago test
            runHook postCheck
          '';

          installPhase = ''
            install -Dm755 index.js $out/bin/miros 
            wrapProgram $out/bin/miros \
              --prefix PATH : ${pkgs.lib.makeBinPath binDeps}
          '';
        };
      in
      {
        packages.miros = miros;
        packages.default = miros;
        packages.miros-nvim = pkgs.vimUtils.buildVimPlugin {
          name = "miros-nvim";
          src = ./vim;
        };

        devShells.default =
          with pkgs;
          mkShell {
            buildInputs = [
              nodejs
              purs
              spago-unstable
              purs-tidy-bin.purs-tidy-0_10_0
              purs-backend-es
              purescript-language-server
              esbuild
            ];
          };
      }
    );
}
