{ mkDerivation, base, casing, containers, data-default, directory
, fetchgit, filepath, hpack, hspec, MissingH, optparse-applicative
, parsec, stdenv, stm, text, twitch
}:
mkDerivation {
  pname = "tailwind-purs";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/gillchristian/tailwind-purs.git";
    sha256 = "0b0s152ji3p9xa2nq3dn2ryj6kg4pjac4yvi7mznl7kzvnbjwymv";
    rev = "343656f4c358acc7f178691b052f8b9aade90c52";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base casing containers data-default directory filepath MissingH
    optparse-applicative parsec stm text twitch
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base casing containers data-default directory filepath MissingH
    optparse-applicative parsec stm text twitch
  ];
  testHaskellDepends = [
    base casing containers data-default directory filepath hspec
    MissingH optparse-applicative parsec stm text twitch
  ];
  prePatch = "hpack";
  homepage = "https://github.com/gillchristian/tailwind-purs#readme";
  license = stdenv.lib.licenses.mit;
}
