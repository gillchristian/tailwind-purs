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
    sha256 = "187lnpxirhsf266b1cmbm6pkdyln4nyssyzdn8qgwgnyg1b5jpir";
    rev = "c83e939f86bd0dd4f5a7ab9afaf991621aeee852";
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
