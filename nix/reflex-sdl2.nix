{ mkDerivation, async, base, containers, dependent-sum
, exception-transformers, fetchgit, lib, mtl, ref-tf, reflex, sdl2
, stm
}:
mkDerivation {
  pname = "reflex-sdl2";
  version = "0.3.0.2";
  src = fetchgit {
    url = "https://github.com/schell/reflex-sdl2";
    sha256 = "06lxfgp18l1car6wd07mbjn4yblnp89acf1i67nd815p2hx0ihbz";
    rev = "6dadf2c4f383b8a58fcd73616996b219c4f93972";
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
  license = lib.licenses.mit;
}
