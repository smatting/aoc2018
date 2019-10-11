# shell-hoogle.nix
let
  hsPkgs = import ./haskell.nix/default-pinned.nix {};
in
  hsPkgs.shellFor {
      packages = ps: [ps.aoc2018];
      withHoogle = true;
  }
