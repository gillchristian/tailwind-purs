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
    sha256 = "0ghraxnfb93jlgwjw66vpkmxlxqjsz7h08wqdm1fl3h6ikxr5cz0";
    rev = "5db77c8755f1d8bdc8efd1217df673e8b9cc99b0";
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
