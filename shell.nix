{ hpkgs ? import ./nix/hpkgs.nix { }
, pkgs ? import ./nix/pkgs.nix { }
}:
hpkgs.shellFor {
  packages = ps: [ ps."game13" ];
  withHoogle = true;

  buildInputs = [
    hpkgs.haskell-language-server
    pkgs.ghcid
    pkgs.cabal-install
    pkgs.haskellPackages.hasktags
  ];
}
