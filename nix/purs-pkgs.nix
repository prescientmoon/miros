{
  sources ? import ../npins,
  pkgs ? import sources.nixpkgs { },
}:
# These don't actually need to be overlays, so we can simply call them with
# our packages.
import "${sources.purescript-overlay}/overlay.nix" pkgs pkgs
