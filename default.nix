{ mkDerivation, base, hpack, sdl2, stdenv }:
mkDerivation {
  pname = "template";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base sdl2 ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base sdl2 ];
  preConfigure = "hpack";
  license = stdenv.lib.licenses.mit;
}
