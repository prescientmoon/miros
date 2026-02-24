{
  sources ? import ./npins,
  pkgs ? import sources.nixpkgs { },
  pursPkgs ? import ./nix/purs-pkgs.nix { inherit pkgs; },
}:
pkgs.mkShell {
  packages = [
    pkgs.nodejs
    pkgs.esbuild
    pkgs.npins
    pursPkgs.purs
    pursPkgs.spago-unstable
    pursPkgs.purs-tidy-bin.purs-tidy-0_10_0
    pursPkgs.purs-backend-es
    pursPkgs.purescript-language-server
  ];
}
