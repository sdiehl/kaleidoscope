{}:
with import <nixpkgs> {};
stdenv.mkDerivation rec {
  name = "kaleidoscope";
  src = ./.;
  buildInputs = [
    ncurses
    zlib
    haskellPackages.ghc
    "haskellPackages.haskell-llvm-general-ghc7.8.3-3.4.2.2-shared"
    "haskell-haskeline-ghc7.8.3-0.7.1.3-shared"
    "haskell-parsec-ghc7.8.3-3.1.3-shared"
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
