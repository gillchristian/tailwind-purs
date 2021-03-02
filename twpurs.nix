{ mkDerivation, base, casing, containers, data-default, directory
, fetchgit, filepath, hpack, hspec, html-parse, MissingH, mtl
, optparse-applicative, parsec, stdenv, stm, text, transformers
, twitch
}:
mkDerivation {
  pname = "tailwind-purs";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/gillchristian/tailwind-purs.git";
    sha256 = "0wcy4hsr7apbdmz9pgczhlajp47cawwvswz19y9xp6ggp919pmpc";
    rev = "f0d5cd96e8a658a8a69bdbf7e098812de9049211";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base casing containers data-default directory filepath html-parse
    MissingH mtl optparse-applicative parsec stm text transformers
    twitch
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base casing containers data-default directory filepath html-parse
    MissingH mtl optparse-applicative parsec stm text transformers
    twitch
  ];
  testHaskellDepends = [
    base casing containers data-default directory filepath hspec
    html-parse MissingH mtl optparse-applicative parsec stm text
    transformers twitch
  ];
  prePatch = "hpack";
  homepage = "https://github.com/gillchristian/tailwind-purs#readme";
  license = stdenv.lib.licenses.mit;
}
