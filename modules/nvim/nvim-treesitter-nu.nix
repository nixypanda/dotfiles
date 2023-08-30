{ pkgs, buildGrammar, fetchFromGitHub, ... }:
let
  highlights = builtins.fetchurl {
    url =
      "https://raw.githubusercontent.com/nushell/tree-sitter-nu/786689b0562b9799ce53e824cb45a1a2a04dc673/queries/highlights.scm";
    sha256 = "sha256:0v1nb5v07n3vad73lgr6xq4p0nd6msxhpjpiymwvbwik7xwx7kp3";
  };
  injections = builtins.fetchurl {

    url =
      "https://raw.githubusercontent.com/nushell/tree-sitter-nu/786689b0562b9799ce53e824cb45a1a2a04dc673/queries/injections.scm";
    sha256 = "sha256:15kh9nb2k2672yzxzydgv9v2aaa6znyq9vcpxkqs6qmzmxl2zhh8";
  };
in {
  grammar = buildGrammar {
    language = "nu";
    version = "0.0.0+rev=786689b";
    src = fetchFromGitHub {
      owner = "nushell";
      repo = "tree-sitter-nu";
      rev = "786689b0562b9799ce53e824cb45a1a2a04dc673";
      hash = "sha256-ENyK0l2KhhNfyuXCz0faLwVHMJjJxlRCqitJkJ8fD+w=";
    };
    meta.homepage = "https://github.com/nushell/tree-sitter-nu";
  };
  highlights = "${builtins.readFile highlights}";
  injections = "${builtins.readFile injections}";
}
