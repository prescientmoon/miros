{
  sources ? import ./npins,
  pkgs ? import sources.nixpkgs { },
}:
let
  mkSpagoDerivationFlake = import "${sources.mkSpagoDerivation}/flake.nix";
  mkSpagoDerivationOverlay =
    (pkgs.lib.fix (
      self:
      mkSpagoDerivationFlake.outputs {
        inherit self;
        inherit (sources) registry registry-index;
        nixpkgs = pkgs;
        ps-overlay = null;
      }
    )).overlays.default;
in
# These don't actually need to be overlays, so we can simply call them with
# our packages.
(mkSpagoDerivationOverlay pkgs pkgs).mkSpagoDerivation
