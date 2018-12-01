{ mkDerivation, aeson, array, attoparsec, base, base64-bytestring
, blaze-html, blaze-markup, bloodhound, bytestring, Chart
, Chart-cairo, Chart-diagrams, containers, data-default, diagrams
, directory, extra, filepath, http-client, http-conduit, megaparsec
, mtl, opaleye, optparse-applicative, parser-combinators, process
, product-profunctors, profunctors, random, regex-pcre, split
, stdenv, text, time, vector, yaml
}:
mkDerivation {
  pname = "haskell-ask-tools";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson array attoparsec base base64-bytestring blaze-html
    blaze-markup bloodhound bytestring Chart Chart-cairo Chart-diagrams
    containers data-default diagrams directory extra filepath
    http-client http-conduit megaparsec mtl opaleye
    optparse-applicative parser-combinators process product-profunctors
    profunctors random regex-pcre split text time vector yaml
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
