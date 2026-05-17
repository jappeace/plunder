{ hpkgs ? import ./nix/hpkgs.nix { }
, pkgs ? import ./nix/pkgs.nix { }
}:
hpkgs.shellFor {
  packages = ps: [ ps."game13" ];

  buildInputs = [
    pkgs.cabal-install
  ];
}
