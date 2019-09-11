# shell.nix
# let
#   hsPkgs = import ./nix/default.nix {};
# in
#   hsPkgs.aoc2018.components.all

let
  hsPkgs = import ./haskell.nix/default.nix {};
in
  hsPkgs.shellFor {
    packages = ps: [ hsPkgs.aoc2018 ];
    withHoogle = true;
  }
