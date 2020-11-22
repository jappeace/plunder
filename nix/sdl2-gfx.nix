{ mkDerivation, base, bytestring, fetchgit, lifted-base
, monad-control, SDL2, sdl2, SDL2_gfx, stdenv, template-haskell
, text, vector
}:
mkDerivation {
  pname = "sdl2-gfx";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/jappeace/sdl2-gfx";
    sha256 = "0q8y6l2p1476p9ylxj9ai0l0vdgl6q9kqnkhka8kbv625jgzmrir";
    rev = "8d6cd3cf89309f7dea5fe9aa634f685716a8ee9a";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring lifted-base monad-control sdl2 template-haskell
    text vector
  ];
  libraryPkgconfigDepends = [ SDL2 SDL2_gfx ];
  description = "Bindings to SDL2_gfx";
  license = stdenv.lib.licenses.mit;
}
