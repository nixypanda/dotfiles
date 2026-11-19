{ config, pkgs, ... }:
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
        type = "http";
        url = "https://mcp.atlassian.com/v2/mcp";
      };
      figma = {
        type = "http";
        url = "https://mcp.figma.com/mcp";
      };
      playwright = {
        type = "stdio";
        command = pkgs.lib.getExe pkgs.playwright-mcp;
        args = [
          "--headless"
          "--isolated"
        ]
        ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
          "--browser"
          "chrome"
          "--executable-path"
          "${pkgs.google-chrome}/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"
        ];
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
      # Ring the terminal bell on completion and permission prompts.
      # The terminal's settings determine how the alert is presented.
      preferredNotifChannel = "terminal_bell";
      tui = "fullscreen";
      # The Nix wrapper pins CLAUDE_CODE_CERT_STORE=system, and its macOS
      # "system" store only surfaces a handful of anchors, so api.anthropic.com
      # (GTS Root R4 -> GlobalSign) fails to verify when the wrapper does not
      # inherit NIX_SSL_CERT_FILE. Appending the Nix CA bundle here fixes that
      # for every launch path, including ones that bypass the shell.
      env.NODE_EXTRA_CA_CERTS = "/etc/ssl/certs/ca-certificates.crt";
    };
  };
}
