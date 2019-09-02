{ mkDerivation, aeson, array, attoparsec, base, bytestring
, containers, data-default, directory, extra, filepath, hpack
, http-client, iconv, lens, megaparsec, mtl, optparse-applicative
, parser-combinators, process, product-profunctors, profunctors
, random, repa, split, stdenv, text, time, vector, yaml
}:
mkDerivation {
  pname = "aoc2018";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array attoparsec base bytestring containers data-default
    directory extra filepath http-client lens megaparsec mtl
    optparse-applicative parser-combinators process product-profunctors
    profunctors random repa split text time vector yaml
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    aeson array attoparsec base bytestring containers data-default
    directory extra filepath http-client lens megaparsec mtl
    optparse-applicative parser-combinators process product-profunctors
    profunctors random repa split text time vector yaml
  ];
  executableSystemDepends = [ iconv ];
  preConfigure = "hpack";
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
