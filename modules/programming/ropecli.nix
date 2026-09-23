{ fetchFromGitHub, python3Packages, ... }:
python3Packages.buildPythonPackage rec {
  pname = "ropify";
  version = "0-unstable-2026-03-03";

  src = fetchFromGitHub {
    owner = "niqodea";
    repo = "${pname}";
    rev = "03dd725ca100df092eb2297bfc698b2994fcce69";
    hash = "sha256-n4ou/5F0+ebzD0hUSvahdMluxL2Yguw2IfNiaGRmf+s=";
  };

  pyproject = true;

  dontCheckRuntimeDeps = true;

  nativeBuildInputs = with python3Packages; [
    setuptools
    wheel
  ];

  propagatedBuildInputs = with python3Packages; [
    click
    rope
    setuptools
  ];
}
