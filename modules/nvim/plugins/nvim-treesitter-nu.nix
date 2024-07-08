{
  pkgs,
  buildGrammar,
  fetchgit,
  ...
}:
let
  commit = "7dd29f9616822e5fc259f5b4ae6c4ded9a71a132";
  src = fetchgit {
    url = "https://github.com/nushell/tree-sitter-nu";
    rev = commit;
    hash = "sha256-V6EZfba5e0NdOG4n3DNI25luNXfcCN3+/vNYuz9llUk=";
    fetchSubmodules = false;
  };
in
{
  grammar = buildGrammar {
    language = "nu";
    version = "0.0.0+rev=${commit}";
    inherit src;
    meta.homepage = "https://github.com/nushell/tree-sitter-nu";
  };
  highlights = builtins.readFile "${src}/queries/nu/highlights.scm";
  injections = builtins.readFile "${src}/queries/nu/injections.scm";
}
