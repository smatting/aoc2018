let
  hsPkgs = import ./haskell.nix/default-pinned.nix {};
in
  hsPkgs.aoc2018.components.all
