let
  pkgs = import <nixpkgs> {};
  zlib = pkgs.zlib;
  ncurses = pkgs.ncurses;
  haskellPackages = pkgs.haskellPackages;
  cabal = haskellPackages.cabal;
  cabalInstall = haskellPackages.cabalInstall;

in

cabal.mkDerivation (self: {
  pname = "kaleidoscope";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    zlib
    ncurses
    haskellPackages.haskeline 
    haskellPackages.llvmGeneral
    haskellPackages.llvmGeneralPure
    haskellPackages.mtl
    haskellPackages.transformers
  ];
  buildTools = [ "cabalInstall_1_18_1_3" ];
  meta = {
    homepage = "https://github.com/sdiehl/kaleidoscope";
    description = "Haskell Kaleidoscope tutorial";
    license = self.stdenv.lib.licenses.mit;
    platforms = self.ghc.meta.platforms;
  };
})
