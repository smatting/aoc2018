{ nixpkgs ?
    import
      (import ./nixpkgs-source.nix {})
      {config = {allowUnfree = true;};}
}:

with nixpkgs;

let
  # mkFun generate a package selection function (ps: [base ..]) from a package.yaml
  mkFun = fn:
    let
      packageYAML = writeTextFile { name = "package.yaml"; text = builtins.readFile fn; };
      packageJSON = runCommand "package.json" { yq = pkgs.yq; } "$yq/bin/yq . ${packageYAML} > $out";
      package = builtins.fromJSON (builtins.readFile packageJSON);
      text = "ps: with ps; [" + lib.concatStringsSep " " package.dependencies + "]";
      defaultNix = pkgs.writeTextFile { name = "default.nix"; text = text; };
      fun = import defaultNix;
    in fun;

  ghc = haskellPackages.ghcWithHoogle (mkFun ./package.yaml);

in

pkgs.stdenv.mkDerivation {
  name = "my-haskell-package";
  buildInputs = [ ghc ];
  shellHook = "eval $(egrep ^export ${ghc}/bin/ghc)";
}
