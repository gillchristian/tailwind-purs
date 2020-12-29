{ mkDerivation, base, casing, containers, data-default, directory
, fetchgit, filepath, hpack, hspec, MissingH, optparse-applicative
, parsec, stdenv, stm, text, twitch
}:
mkDerivation {
  pname = "tailwind-purs";
  version = "0.0.1.0";
  src = fetchgit {
    url = "https://github.com/gillchristian/tailwind-purs.git";
    sha256 = "027x2b8kxc3q3xf4i4jhk94x5mh5ri02386xh62sqf74zgmpdgbq";
    rev = "3b5bcfebf891c7c0eae1ae4991af29e411b63fdd";
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
