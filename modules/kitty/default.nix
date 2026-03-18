{
  pkgs,
  colorscheme,
  config,
  lib,
  ...
}:
let
  kittySessionPicker = pkgs.writeShellScript "kitty-session-picker" (
    lib.replaceStrings
      [
        "@find@"
        "@sort@"
        "@basename@"
        "@cut@"
        "@fzf@"
        "@kitty@"
      ]
      [
        "${pkgs.findutils}/bin/find"
        "${pkgs.coreutils}/bin/sort"
        "${pkgs.coreutils}/bin/basename"
        "${pkgs.coreutils}/bin/cut"
        "${pkgs.fzf}/bin/fzf"
        "${pkgs.kitty}/bin/kitty"
      ]
      (builtins.readFile ./kitty-session-picker.sh)
  );

  kittyDevApp = pkgs.runCommand "kitty-dev-app" { } ''
    mkdir -p "$out/Applications"
    cp -R "${pkgs.kitty-dev}/Applications/kitty.app" "$out/Applications/kitty-dev.app"
    chmod -R u+w "$out/Applications/kitty-dev.app"

    "${pkgs.python3}/bin/python3" - <<PY
import plistlib
from pathlib import Path

plist_path = Path("$out/Applications/kitty-dev.app/Contents/Info.plist")
with plist_path.open("rb") as f:
    info = plistlib.load(f)

info["CFBundleIdentifier"] = "net.kovidgoyal.kitty.dev"
info["CFBundleName"] = "kitty-dev"
info["CFBundleDisplayName"] = "kitty-dev"

with plist_path.open("wb") as f:
    plistlib.dump(info, f)
PY
  '';

  kittyDevBin = pkgs.writeShellScriptBin "kitty-dev" ''
    exec "${pkgs.kitty-dev}/bin/kitty" "$@"
  '';
in
{
  programs.kitty = {
    enable = true;
    package = pkgs.kitty;
    settings = {
      font_size = 12;
      scrollback_lines = 10000;
      input_delay = 1;

      enabled_layouts = "Tall,Grid,Stack";
      shell = "${pkgs.nushell}/bin/nu";
      editor = "~/.nix-profile/bin/nvim";

      tab_bar_edge = "top";
      tab_bar_style = "powerline";
      tab_powerline_style = "slanted";
      tab_bar_min_tabs = 1;
      tab_bar_filter = "session:~ or session:^$";
      tab_title_template = " 󰆍 {index} → {title} ";
      active_tab_title_template = " {session_name} ┊ 󰆍 {index} → {title} ┊ 󰕮 ‹{layout_name}› ";

      macos_option_as_alt = "left";
      macos_show_window_title_in = "none";

      # opencode requires remote control
      allow_remote_control = "yes";
      listen_on = "unix:/tmp/kitty";
    };
    keybindings = {
      "cmd+enter" = "new_window_with_cwd";
      "cmd+t" = "new_tab";
      "cmd+shift+t" = "new_tab_with_cwd";
      "cmd+shift+r" = "set_tab_title";
      "cmd+g" = "swap_with_window";
      # closing
      "cmd+x" = "close_window";
      "cmd+shift+x" = "close_tab";
      # vim-like keybinds for navigation
      "cmd+j" = "next_tab";
      "cmd+k" = "previous_tab";
      "cmd+l" = "next_window";
      "cmd+h" = "previous_window";
      "cmd+shift+j" = "move_tab_forward";
      "cmd+shift+k" = "move_tab_backward";
      "cmd+shift+l" = "move_window_forward";
      "cmd+shift+h" = "move_window_backward";
      "cmd+d" = "detach_window ask";
      # font size
      "cmd+equal" = "change_font_size all +1.0";
      "cmd+minus" = "change_font_size all -1.0";
      # resizing (not-vim-like - broken)
      "cmd+shift+equal" = "resize_window wider";
      "cmd+shift+minus" = "resize_window narrower";
      # layout
      "cmd+n" = "next_layout";
      "cmd+f" = "toggle_layout stack";
      # sessions
      "cmd+shift+s" = "goto_session ~/.local/state/kitty/sessions";
      "cmd+s" = "launch --type=overlay --cwd=current ~/.local/bin/kitty-session-picker";
      "cmd+1" = "goto_session ~/.local/state/kitty/sessions/reproducible-me.kitty-session";
      "cmd+2" = "goto_session ~/.local/state/kitty/sessions/monadic-trials.kitty-session";
      "cmd+w" =
        "combine : save_as_session --use-foreground-process --save-only --base-dir ~/.local/state/kitty/sessions : quit";
      # scrollback
      "cmd+/" = "show_scrollback";
      # shell integration
      "cmd+p" = "show_last_command_output";
    };
  };

  home.activation.kittyCodesign = lib.hm.dag.entryAfter [ "copyApps" ] (
    lib.optionalString pkgs.stdenv.hostPlatform.isDarwin ''
      for app in "$HOME/Applications/Home Manager Apps/kitty.app" "$HOME/Applications/Home Manager Apps/kitty-dev.app"; do
        if [ -d "$app" ]; then
          /usr/bin/codesign --force --deep --sign - "$app"
          /usr/bin/codesign --verify --deep --strict --verbose=2 "$app"
        fi
      done
    ''
  );

  home.packages = [
    kittyDevApp
    kittyDevBin
  ];

  home.file = {
    "${config.xdg.configHome}/kitty/quick-access-terminal.conf".text = ''
      lines 40
      columns 200
      background_opacity 0.95
      edge top
    '';
    "${config.xdg.configHome}/kitty/open-actions.conf".text = ''
      protocol file
      mime image/*
      action launch --type=overlay kitten icat --hold -- $FILE_PATH

      protocol file
      mime text/*
      action launch --type=tab $EDITOR $FILE_PATH
    '';

    "${config.home.homeDirectory}/.local/bin/kitty-session-picker" = {
      executable = true;
      source = kittySessionPicker;
    };

    "${config.xdg.configHome}/kitty/dark-theme.auto.conf" = {
      text = "include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-dark}.conf";
    };
    "${config.xdg.configHome}/kitty/light-theme.auto.conf" = {
      text = "include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-light}.conf";
    };
    "${config.xdg.configHome}/kitty/no-preference-theme.auto.conf" = {
      text = "include ${pkgs.kitty-themes}/share/kitty-themes/themes/${colorscheme.kitty-name-dark}.conf";
    };
  };
}
