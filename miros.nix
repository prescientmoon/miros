{
  mkSpagoDerivation,
  purs-unstable,
  spago-unstable,
  purs-backend-es,
  nodejs,
  esbuild,
  makeWrapper,
  stylua,
  lib,
}:
mkSpagoDerivation {
  version = "unstable-2026-02-24";
  pname = "miros";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [
      ./src
      ./test
      ./spago.lock
      ./spago.yaml
    ];
  };

  spagoYaml = ./spago.yaml;
  spagoLock = ./spago.lock;
  nativeBuildInputs = [
    purs-unstable
    spago-unstable
    purs-backend-es
    nodejs
    esbuild
    makeWrapper
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
      --prefix PATH : ${
        lib.makeBinPath [
          nodejs
          stylua
        ]
      }
  '';
}
