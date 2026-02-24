{
  sources ? import ./npins,
  pkgs ? import sources.nixpkgs { },
  pursPkgs ? import ./purs-pkgs.nix { inherit pkgs sources; },
  mkSpagoDerivation ? import ./mk-spago-derivation.nix { inherit pkgs sources; },
}:
{
  miros = pkgs.callPackage (import ./miros.nix) {
    inherit mkSpagoDerivation;
    inherit (pursPkgs)
      purs-unstable
      spago-unstable
      purs-backend-es
      ;
  };
}
