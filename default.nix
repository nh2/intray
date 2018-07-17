let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  validity-overlay = import (
    (pkgs.fetchFromGitHub (import ./validity-version.nix)
    + "/overlay.nix")
  );
  # TODO: Remove when https://github.com/NixOS/cabal2nix/pull/360 is merged and available
  cabal2nix-overlay = final: previous:
    with final.haskell.lib; {
      haskellPackages = previous.haskellPackages.override (old: {
        overrides = final.lib.composeExtensions (old.overrides or (_: _: {})) (

          self: super: {
            cabal2nix = overrideCabal super.cabal2nix (old: {
              src = pkgs.fetchFromGitHub {
                owner = "nh2";
                repo = "cabal2nix";
                rev = "5721bed2a598a018119413bfe868bd286735cb15";
                sha256 = "1436ri6nlfcgd263byb596dcx6g4l9fx47hm11vfh34x849r2kcy";
              };
            });

          }
        );
      });
    };
in pkgsv {
  overlays = [ cabal2nix-overlay validity-overlay (import ./overlay.nix) ];
  config.allowUnfree = true;
}
