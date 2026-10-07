{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:

buildGoModule rec {
  pname = "hledger-lsp";
  version = "0.2.49";

  src = fetchFromGitHub {
    owner = "juev";
    repo = "hledger-lsp";
    tag = "v${version}";
    hash = "sha256-j+AC21EEZEz3kElX+P3GMvm13AYxNd87add8IZFS+7Q=";
  };

  vendorHash = "sha256-Oo/8LCX6svcH/0vCowzOiAhlif9LJfNrU3OgNiZDupo=";

  subPackages = [ "cmd/hledger-lsp" ];

  ldflags = [
    "-s"
    "-w"
    "-X=main.Version=${version}"
  ];

  meta = {
    description = "Language Server Protocol implementation for hledger journal files";
    homepage = "https://github.com/juev/hledger-lsp";
    license = lib.licenses.mit;
    mainProgram = "hledger-lsp";
  };
}
