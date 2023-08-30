{ pkgs, buildGrammar, fetchgit, ... }:
let
  commit = "786689b0562b9799ce53e824cb45a1a2a04dc673";
  src = fetchgit {
    url = "https://github.com/nushell/tree-sitter-nu";
    rev = commit;
    hash = "sha256-ENyK0l2KhhNfyuXCz0faLwVHMJjJxlRCqitJkJ8fD+w=";
    fetchSubmodules = false;
  };
in {
  grammar = buildGrammar {
    language = "nu";
    version = "0.0.0+rev=${commit}";
    inherit src;
    meta.homepage = "https://github.com/nushell/tree-sitter-nu";
  };
  highlights = builtins.readFile "${src}/queries/highlights.scm";
  injections = builtins.readFile "${src}/queries/injections.scm";
}
