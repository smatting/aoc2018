# shell.nix
# let
#   hsPkgs = import ./nix/default.nix {};
# in
#   hsPkgs.aoc2018.components.all

let
  hsPkgs = import ./haskell.nix/default.nix {};
in
  hsPkgs.aoc2018.components.all
