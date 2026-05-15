{ config, pkgs, ... }:

let
  inherit (config) xdg;
in
{
  home.packages = with pkgs; [
    # docker
    docker
    docker-compose

    # Haskell
    ghc

    # python
    (python3.withPackages (
      ps: with ps; [
        setuptools
        pip
      ]
    ))
  ];

  # Codex — HM module auto-manages CODEX_HOME via preferXdgDirectories
  programs.codex.enable = true;

  # Claude Code — HM module auto-manages CLAUDE_CONFIG_DIR when configDir ≠ ~/.claude
  programs.claude-code = {
    enable = true;
    configDir = "${xdg.configHome}/claude";
  };
}
