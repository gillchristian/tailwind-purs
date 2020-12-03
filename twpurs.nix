{ mkDerivation, base, casing, containers, directory, filepath
, hpack, MissingH, optparse-applicative, parsec, stdenv, text
}:
mkDerivation {
  pname = "tailwind-purs";
  version = "0.0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base casing containers directory filepath MissingH
    optparse-applicative parsec text
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base casing containers directory filepath MissingH
    optparse-applicative parsec text
  ];
  testHaskellDepends = [
    base casing containers directory filepath MissingH
    optparse-applicative parsec text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/gillchristian/tailwind-purs#readme";
  license = stdenv.lib.licenses.mit;
}
