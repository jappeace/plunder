{ pkgs ? import ./nix/pin.nix { config.allowBroken = true; },
  # should be default ghc
  # https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/all-packages.nix#L9029
  compiler ? "ghc884",
  ... }:

let
  hpkgs = pkgs.haskell.packages.${compiler};
  ignore = import ./nix/gitignoreSource.nix { inherit (pkgs) lib; };
  # https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/make-package-set.nix#L216
  src = ignore.gitignoreSource ./.;
  cabal2nix =
    hpkgs.callCabal2nix "game13" src {
      reflex-sdl2 = pkgs.haskell.lib.doJailbreak (hpkgs.callPackage ./nix/reflex-sdl2.nix { });
      sdl2-gfx = pkgs.haskell.lib.doJailbreak (hpkgs.callPackage ./nix/sdl2-gfx.nix { });
    };
in
# https://github.com/NixOS/nixpkgs/blob/dbacb52ad8/pkgs/development/haskell-modules/generic-builder.nix#L13

pkgs.haskell.lib.overrideCabal cabal2nix (drv: {
  inherit src;
  isExecutable = true;
})
