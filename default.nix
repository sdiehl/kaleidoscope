{}:
with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "kaleidoscope";
  src = ./.;
  buildInputs = [
    ncurses
    zlib
    haskellPackages.ghc
    haskellPackages.llvmGeneral
    haskellPackages.parsec
    haskellPackages.haskeline
  ];
  buildPhase = ''
    make chapter1
    make chapter2
    make chapter3
    make chapter4
    make chapter5
    make chapter6
    make chapter7
  '';
  installPhase = ''
  '';
}
