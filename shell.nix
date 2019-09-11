let
  sources = import ./nix/sources.nix;
in
  with (import sources."nixpkgs" {});
  pkgs.mkShell {
    buildInputs = [
      (haskell.packages.ghc865.ghcWithPackages (hs: [ hs.cabal-install hs.zlib ]))
      pkgs.pkgconfig
      pkgs.zlib
    ];
  }
