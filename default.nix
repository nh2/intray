let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay = import (
    (pkgs.fetchFromGitHub (import ./validity-version.nix)
    + "/overlay.nix")
  );
in pkgsv {
  overlays = [ validity-overlay (import ./overlay.nix) ];
  config.allowUnfree = true;
}
