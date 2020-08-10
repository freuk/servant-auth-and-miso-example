{ # this uses miso's pinning and caching for non-tooling dependencies
nixpkgs-miso ?
  builtins.fetchTarball "https://github.com/dmjio/miso/archive/master.tar.gz",

nixpkgs ?
  builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz"

}:
let

  pkgs_20_03 = import nixpkgs { };
  misopkgs = import nixpkgs-miso { };
  pkgs = misopkgs.pkgs;
  src = pkgs.lib.cleanSource ./.;

in pkgs // rec {

  nixfmt = pkgs_20_03.nixfmt;
  ormolu = let
    source = builtins.fetchTarball
      "https://github.com/tweag/ormolu/archive/master.zip";
  in (import source { }).ormolu;

  app-ghc = pkgs.haskellPackages.callCabal2nix "app" src {
    miso = misopkgs.miso-jsaddle;
  };

  app-ghcjs = pkgs.haskell.packages.ghcjs.callCabal2nix "app" src { };

  app = pkgs.runCommand "app" { } ''
    mkdir -p $out/{bin,static,share}
    cp ${app-ghc}/bin/* $out/bin/
    ${pkgs.closurecompiler}/bin/closure-compiler \
      ${app-ghcjs}/bin/client.jsexe/all.js > $out/static/all.js
    cd $out/share
    mkdir $out/share/docs
    ${app-ghc}/bin/docs
  '';
}
