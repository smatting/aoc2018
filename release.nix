let
  nixpkgSrc = import ./nixpkgs-source.nix {};

  config = rec {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = self: super: rec {

          myHaskellPackage = pkgs.haskell.lib.overrideCabal
            (pkgs.haskell.lib.justStaticExecutables (self.callPackage ./default.nix { }))
            ( oldDerivation: {
                buildTools = [ super.hpack ];
                preConfigure = ''hpack'';
                testToolDepends = [];
                enableSharedExecutables = false;
              }
            );

        };
      };
    };
    allowUnfree = true;
  };

  pkgs = import nixpkgSrc { inherit config; };

in
  pkgs.haskellPackages.myHaskellPackage
