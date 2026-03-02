{ sources ? import ../npins
, pkgs ? import ./pkgs.nix { inherit sources; }
}:
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    game13 = hnew.callCabal2nix "game13" ../. { };
    reflex-sdl2 = hnew.callCabal2nix "reflex-sdl2" sources.reflex-sdl2 { };
    sdl2-image = pkgs.haskell.lib.overrideCabal
      (pkgs.haskell.lib.markUnbroken hold.sdl2-image)
      { __onlyPropagateKnownPkgConfigModules = true; };
  };
}
