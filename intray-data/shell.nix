let pkgs = (import ../.);
    drv = pkgs.haskellPackages.intray-data;
    drv' = pkgs.haskell.lib.addBuildDepend drv pkgs.haskellPackages.cabal-install;
in drv'.env
