{ withHoogle ? false, ghcjs ? false }:
with import ./. { };

pkgs.haskellPackages.shellFor {
  packages = p: with p; [ (if ghcjs then app-ghcjs else app-ghc) ];
  buildInputs = [
    pkgs.haskellPackages.ghcid
    ormolu
    # pkgs.haskell.compiler.ghcjs
    pkgs.hlint
    haskellPackages.apply-refact
    pkgs.cabal-install
  ];
  inherit withHoogle;
}
