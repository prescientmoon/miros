{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    purifix.url = "github:purifix/purifix";
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
        pkgs =
          (nixpkgs.legacyPackages.${system}.extend inputs.purifix.overlay).extend
            inputs.purescript-overlay.overlays.default;

        mirosProject = pkgs.purifix { src = ./.; };

        deps = [
          pkgs.stylua
          pkgs.nodejs
        ];

        evaluate = "import {main} from 'file://$out/output/Main/index.js'; main();";
      in
      rec {
        packages.miros = pkgs.symlinkJoin {
          inherit (mirosProject.run) name meta;
          paths = [ mirosProject ];
          nativeBuildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            mkdir -p $out/bin
            echo "#!${pkgs.runtimeShell}" >> $out/bin/miros
            echo "node \
              --input-type=module \
              --abort-on-uncaught-exception \
              --trace-sigint \
              --trace-uncaught \
              --eval=\"${evaluate}\" -- \"\$@"\" >> $out/bin/miros
            chmod +x $out/bin/miros
            wrapProgram $out/bin/miros \
              --prefix PATH : ${pkgs.lib.makeBinPath deps}
          '';
        };

        packages.default = packages.miros;
        packages.miros-nvim = pkgs.vimUtils.buildVimPlugin {
          name = "miros-nvim";
          src = ./vim;
        };

        devShells.default =
          with pkgs;
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
