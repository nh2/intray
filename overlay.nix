final:
  previous:
    with final.haskell.lib;
    {
      haskellPackages = previous.haskellPackages.override {
        overrides = self:
          super:
            let
              typedUuidRepo = final.fetchFromGitHub {
                owner = "NorfairKing";
                repo = "typed-uuid";
                rev = "155c9ec880ca1c12f7dd8a8468b3626de8164823";
                sha256 = "0wvdj07vhd7q93f7sdg4mq8f9nk4w3fjsq3z7nx7zm5dv0j78iwb";
              };
              validityRepo = final.fetchFromGitHub {
                owner = "NorfairKing";
                repo = "validity";
                rev = "a86dd3e4830c14c056a0d5d8712864d395d25b2a";
                sha256 = "1sg5bbrqv723c8jlznfnxk6x748082bprw9z2zmm8m0ycskw6ds7";
              };
              mergelessRepo = final.fetchFromGitHub {
                owner = "NorfairKing";
                repo = "mergeless";
                rev = "f0dc884343a682afdc8f9a39bee419d99d85c4c3";
                sha256 = "0adsv9sq9q5sxalagn7a7g8h0wlbby4423cajvgilfl4j9h88v9d";
              };
              prettyRelativeTimeRepo = final.fetchFromGitHub {
                owner = "NorfairKing";
                repo = "pretty-relative-time";
                rev = "9a670db25ac68974045c75da364486c10970824f";
                sha256 = "125p6m7p8kqcndfa248k3xylmk9n2smchknplx2qqlqyxa909zdn";
              };
              typedUuidPkg = name:
                super.callCabal2nix name (typedUuidRepo + "/${name}") {};
              validityPkg = name:
                super.callCabal2nix name (validityRepo + "/${name}") {};
              intrayPkg = name:
                disableLibraryProfiling (super.callCabal2nix name (./. + "/${name}") {});
              mergelessPkg = name:
                super.callCabal2nix name (mergelessRepo + "/${name}") {};
            in {
              pretty-relative-time = super.callCabal2nix "pretty-relative-time" prettyRelativeTimeRepo {};
              servant-auth-server = doJailbreak (super.servant-auth-server);
            } // final.lib.genAttrs [
              "typed-uuid"
              "genvalidity-typed-uuid"
            ] typedUuidPkg // final.lib.genAttrs [
              "mergeless"
              "genvalidity-mergeless"
            ] mergelessPkg // final.lib.genAttrs [
              "intray-data"
              "intray-data-gen"
              "intray-api"
              "intray-api-gen"
              "intray-cli"
              "intray-client"
              "intray-data"
              "intray-data-gen"
              "intray-server"
              "intray-server-test-utils"
              "intray-web-server"
            ] intrayPkg // final.lib.genAttrs [
              "genvalidity"
              "genvalidity-aeson"
              "genvalidity-bytestring"
              "genvalidity-containers"
              "genvalidity-hspec"
              "genvalidity-hspec-aeson"
              "genvalidity-hspec-binary"
              "genvalidity-hspec-cereal"
              "genvalidity-hspec-hashable"
              "genvalidity-path"
              "genvalidity-property"
              "genvalidity-scientific"
              "genvalidity-text"
              "genvalidity-time"
              "genvalidity-unordered-containers"
              "genvalidity-uuid"
              "genvalidity-vector"
              "validity"
              "validity-aeson"
              "validity-bytestring"
              "validity-containers"
              "validity-path"
              "validity-scientific"
              "validity-text"
              "validity-time"
              "validity-unordered-containers"
              "validity-uuid"
              "validity-vector"
            ] validityPkg;
      };
    }
