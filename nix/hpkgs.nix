{ sources ? import ../npins
, pkgs ? import ./pkgs.nix { inherit sources; }
}:
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    game13 = hnew.callCabal2nix "game13" ../. { };
    reflex-sdl2 = hnew.callCabal2nix "reflex-sdl2" sources.reflex-sdl2 { };
  };
}
