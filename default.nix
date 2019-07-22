{ mkDerivation, base, bytestring, hpack, sdl2, stdenv }:
mkDerivation {
  pname = "template";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring sdl2 ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base bytestring sdl2 ];
  preConfigure = "hpack";
  license = stdenv.lib.licenses.mit;
}
