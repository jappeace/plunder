{ program ? import ../default.nix { }, run ? "/bin/game13" }:
let
nix-bundle-src = builtins.fetchGit {
    url = "https://github.com/matthewbauer/nix-bundle";
    rev = "83eadede9087c2c94a018ea6e240da552355dcc1";
};
nix-bundle = (import ("${nix-bundle-src}/appimage-top.nix") {}) // (import "${nix-bundle-src}/default.nix" {});
in
   nix-bundle.nix-bootstrap {
      extraTargets = [];
      target = program;
      inherit run;
    }
