{
  sources ? import ./npins,
  pkgs ? import sources.nixpkgs { },
  pursPkgs ? import ./nix/purs-pkgs.nix { inherit pkgs sources; },
  mkSpagoDerivation ? import ./nix/mk-spago-derivation.nix { inherit pkgs sources; },
}:
{
  miros = pkgs.callPackage (import ./nix/miros.nix) {
    inherit mkSpagoDerivation;
    inherit (pursPkgs)
      purs-unstable
      spago-unstable
      purs-backend-es
      ;
  };

  miros-nvim = pkgs.vimUtils.buildVimPlugin {
    name = "miros-nvim";
    src = ./vim;
  };
}
