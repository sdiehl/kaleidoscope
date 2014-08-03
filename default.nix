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
    ghc --make src/chapter2/Main.hs -o chapter2
    ghc --make src/chapter3/Main.hs -o chapter3
    ghc --make src/chapter4/Main.hs -o chapter4
    ghc --make src/chapter5/Main.hs -o chapter5
    ghc --make src/chapter6/Main.hs -o chapter6
    ghc --make src/chapter7/Main.hs -o chapter7
  '';
  installPhase = ''
  '';
}
