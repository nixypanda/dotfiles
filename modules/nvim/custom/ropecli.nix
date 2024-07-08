{ fetchFromGitHub, python310Packages, ... }:
let
  old_setuptools = python310Packages.setuptools.overrideAttrs (o: rec {
    pname = "setuptools";
    version = "68.2.2";
    src = python310Packages.fetchPypi {
      inherit pname version;
      hash = "sha256-SsFHUnbS8cSGhIdAif782DvXFi3ar7gfrIZroNsoKoc=";
    };
  });
in
python310Packages.buildPythonPackage rec {
  pname = "ropify";
  version = "eb7658cf6d225c3c8964a2772b6f4d369322d18c";

  src = fetchFromGitHub {
    owner = "niqodea";
    repo = "${pname}";
    rev = "${version}";
    hash = "sha256-30gToLrKd+4fcYlEHsLDCIHjnlPXb+rrLyYydVDECec=";
  };

  pyproject = true;

  nativeBuildInputs = with python310Packages; [
    pip
    poetry-core
  ];

  propagatedBuildInputs = with python310Packages; [
    click
    rope
    old_setuptools
  ];
}
