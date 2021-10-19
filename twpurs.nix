{ mkDerivation, base, casing, containers, data-default, directory
, filepath, hpack, hspec, html-parse, lib, MissingH, mtl
, optparse-applicative, parsec, stm, text, transformers, twitch
, with-utf8
}:
mkDerivation {
  pname = "tailwind-purs";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base casing containers data-default directory filepath html-parse
    MissingH mtl optparse-applicative parsec stm text transformers
    twitch with-utf8
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base casing containers data-default directory filepath html-parse
    MissingH mtl optparse-applicative parsec stm text transformers
    twitch with-utf8
  ];
  testHaskellDepends = [
    base casing containers data-default directory filepath hspec
    html-parse MissingH mtl optparse-applicative parsec stm text
    transformers twitch with-utf8
  ];
  prePatch = "hpack";
  homepage = "https://github.com/gillchristian/tailwind-purs#readme";
  license = lib.licenses.mit;
}
