{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "aoc2018"; version = "0.1.0.0"; };
      license = "NONE";
      copyright = "";
      maintainer = "";
      author = "";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.array)
          (hsPkgs.base)
          (hsPkgs.bytestring)
          (hsPkgs.case-insensitive)
          (hsPkgs.aeson)
          (hsPkgs.containers)
          (hsPkgs.data-default)
          (hsPkgs.directory)
          (hsPkgs.extra)
          (hsPkgs.filepath)
          (hsPkgs.lens)
          (hsPkgs.megaparsec)
          (hsPkgs.mtl)
          (hsPkgs.optparse-applicative)
          (hsPkgs.parser-combinators)
          (hsPkgs.process)
          (hsPkgs.random)
          (hsPkgs.split)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.vector)
          (hsPkgs.yaml)
          (hsPkgs.raw-strings-qq)
          (hsPkgs.optics)
          (hsPkgs.monad-loops)
          ];
        };
      exes = {
        "primes" = {
          depends = [ (hsPkgs.base) ];
          libs = (pkgs.lib).optional (system.isOsx) (pkgs."iconv");
          };
        "aoc" = {
          depends = [
            (hsPkgs.aoc2018)
            (hsPkgs.base)
            (hsPkgs.filepath)
            (hsPkgs.optparse-applicative)
            (hsPkgs.text)
            ];
          libs = (pkgs.lib).optional (system.isOsx) (pkgs."iconv");
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault .././../.; }