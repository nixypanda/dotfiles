{ config, lib, pkgs, ... }:

let
  inherit (config) xdg;
in
{
  # Tell HM modules to prefer XDG paths (codex → $XDG_CONFIG_HOME/codex, etc.)
  home.preferXdgDirectories = true;

  # All other env vars — HM's home.sessionVariables sets these in all shells
  home.sessionVariables = {
    CARGO_HOME = "${xdg.dataHome}/cargo";
    DOCKER_CONFIG = "${xdg.configHome}/docker";
    GOPATH = "${xdg.dataHome}/go";
    GRADLE_USER_HOME = "${xdg.dataHome}/gradle";
    LESSHISTFILE = "${xdg.stateHome}/lesshst";
    MPLCONFIGDIR = "${xdg.configHome}/matplotlib";
    NPM_CONFIG_CACHE = "${xdg.cacheHome}/npm";
    NPM_CONFIG_INIT_MODULE = "${xdg.configHome}/npm/config/npm-init.js";
    PYTHON_HISTORY = "${xdg.stateHome}/python/history";
    REDISCLI_HISTFILE = "${xdg.stateHome}/redis/rediscli_history";
    STACK_ROOT = "${xdg.dataHome}/stack";
    STACK_XDG = "1";
  };
}
