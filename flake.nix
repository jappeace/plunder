# I used chatgpt to generate this template and then just
# modified to how I normally use these things.
{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskellPackages.override {
        overrides = hnew: hold: {
          game13 = hnew.callCabal2nix "game13" ./. { };
        };
      };
    in
    {
      defaultPackage.x86_64-linux =  hpkgs.game13;
      inherit pkgs;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."game13" ];
        withHoogle = true;

        buildInputs = [
          # hpkgs.haskell-language-server
          pkgs.ghcid
          pkgs.cabal-install
          pkgs.haskellPackages.hasktags
        ];
      };
    };
}
