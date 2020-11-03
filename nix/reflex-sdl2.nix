{ mkDerivation, async, base, containers, dependent-sum
, exception-transformers, fetchgit, mtl, ref-tf, reflex, sdl2
, stdenv, stm
}:
mkDerivation {
  pname = "reflex-sdl2";
  version = "0.3.0.1";
  src = fetchgit {
    url = "https://github.com/schell/reflex-sdl2";
    sha256 = "0nli9p5yrhi7aiapcmh0ykmxdffsrzjfwnvcv7pbbd2ncdcfvh2v";
    rev = "e2682a9794d3939525eee34c9fe7142b86e4e98f";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base containers dependent-sum exception-transformers mtl
    ref-tf reflex sdl2 stm
  ];
  executableHaskellDepends = [ base mtl reflex ];
  homepage = "https://github.com/schell/reflex-sdl2#readme";
  description = "SDL2 and reflex FRP";
  license = stdenv.lib.licenses.mit;
}
