let
  sources = import ./nix/sources.nix;
in
  with (import sources."nixpkgs" {});
  let
    ghc = (haskell.packages.ghc865.ghcWithPackages (hs: [ hs.cabal-install hs.zlib ]));
  in
  pkgs.mkShell {
    buildInputs = [
      ghc
      pkgs.pkgconfig
      pkgs.zlib
    ];
# TODO: set NIX
 # export NIX_GHC_LIBDIR="/nix/store/p9r7vp3ifinihaa2v7k1qlg1wfkgjdfk-ghc-8.6.5-with-packages/lib/ghc-8.6.5/"
    shellHook = ''
    export NIX_GHC_LIBDIR="${ghc}/lib/ghc-8.6.5";
    '';
  }
