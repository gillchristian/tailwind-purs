{ mkDerivation, base, casing, containers, directory, fetchgit
, filepath, hpack, hspec, MissingH, optparse-applicative, parsec
, stdenv, text
}:
mkDerivation {
  pname = "tailwind-purs";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/gillchristian/tailwind-purs.git";
    sha256 = "0z48wsy4xqrd7pgw5wnqy36xwdzrmi6xbp4cwh37iagjkavqkgqs";
    rev = "5d6ab2bf609c83d9baee64740ecd62270bf4c5f1";
    fetchSubmodules = true;
  };
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
    base casing containers directory filepath hspec MissingH
    optparse-applicative parsec text
  ];
  prePatch = "hpack";
  homepage = "https://github.com/gillchristian/tailwind-purs#readme";
  license = stdenv.lib.licenses.mit;
}
