{ mkDerivation, base, bytestring, hpack, lens, sdl2, stdenv, vector
}:
mkDerivation {
  pname = "template";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring lens sdl2 vector ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base bytestring lens sdl2 vector ];
  preConfigure = "hpack";
  license = stdenv.lib.licenses.mit;
}
