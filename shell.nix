{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "intray";
  buildInputs = [ zlib unzip glibc ];
}
