{ config, ... }:
let
  inherit (config) xdg;
in
{
  programs.claude-code = {
    enable = true;
    configDir = "${xdg.configHome}/claude";

    context = ./CLAUDE.md;

    # Written into a synthetic plugin dir passed as --plugin-dir, not into
    # .claude.json, so this coexists with the state file Claude Code mutates.
    mcpServers = {
      sentry = {
        type = "http";
        url = "https://mcp.sentry.dev/mcp";
      };
      atlassian = {
        type = "sse";
        url = "https://mcp.atlassian.com/v1/sse";
      };
    };

    # Whole-file store symlink, so Claude Code can no longer persist anything
    # here: /model, /fast and /config stop sticking. Change them by editing
    # this and rebuilding.
    settings = {
      includeCoAuthoredBy = false;
      permissions.allow = [ "Bash(*)" ];
      model = "opus[1m]";
      enabledPlugins = {
        "typescript-lsp@claude-plugins-official" = true;
      };
      effortLevel = "medium";
      # Ring kitty's bell on completion and permission prompts; kitty turns the
      # BEL into a tab indicator, dock badge, and banner (modules/home/kitty).
      preferredNotifChannel = "terminal_bell";
      tui = "fullscreen";
    };
  };
}
