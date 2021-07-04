{ mkDerivation, base, bytestring, fetchgit, lib, lifted-base
, monad-control, SDL2, sdl2, SDL2_gfx, template-haskell, text
, vector
}:
mkDerivation {
  pname = "sdl2-gfx";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/haskell-game/sdl2-gfx";
    sha256 = "0f9yczmmfxvv24f8mplq90rh97ip21ik0w29iqpvlwcr446ysc0k";
    rev = "8532ccf52602e983aef4cb1bf5af3dbd1e76256e";
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
  license = lib.licenses.mit;
}
