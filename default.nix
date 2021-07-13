{ pkgs ? import ./nix/pin.nix { config.allowBroken = true; },
  # I find compiler verions by goin in repel, pkgs=import <nixpkgs>{},
  # tab complete the path.
  compiler ? "ghc8104",
  ... }:

let
  hpkgs = pkgs.haskell.packages.${compiler}.override {
    overrides = hpNew: hpOld: {

        reflex-sdl2 = pkgs.haskell.lib.doJailbreak (hpkgs.callPackage ./nix/reflex-sdl2.nix { });
        sdl2-gfx = pkgs.haskell.lib.doJailbreak (hpkgs.callPackage ./nix/sdl2-gfx.nix { });
        # singletons = hpNew.callHackageDirect {
        #         pkg = "singletons";
        #         ver = "2.7";
        #         sha256 = "0ssbswl72fr3wx8br2c4snzi4qnic821wq57s042cjw61kzrrg5b";
        #     } {};
    };
  };
  ignore = import ./nix/gitignoreSource.nix { inherit (pkgs) lib; };
  # https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/make-package-set.nix#L216
  src = ignore.gitignoreSource ./.;
  cabal2nix =
    hpkgs.callCabal2nix "game13" src {
    };
in
# https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/generic-builder.nix#L13

pkgs.haskell.lib.overrideCabal cabal2nix (drv: {
  inherit src;
  isExecutable = true;
})
