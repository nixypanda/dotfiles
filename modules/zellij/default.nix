{ pkgs, ... }:
let

  zellij-forgot = pkgs.stdenv.mkDerivation rec {
    pname = "zellij-forgot";
    version = "0.4.0";

    src = builtins.fetchurl {
      url = "https://github.com/karimould/zellij-forgot/releases/download/${version}/zellij_forgot.wasm";
      sha256 = "sha256:1hzdvyswi6gh4ngxnplay69w1n8wlk17yflfpwfhv6mdn0gcmlsr";
    };
    phases = [ "installPhase" ];

    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/zellij_forgot.wasm
    '';
  };

in
{
  programs.zellij = {
    enable = true;
    enableZshIntegration = true;
  };
  xdg.configFile."zellij/config.kdl".text = # kdl
    ''
      default_shell "nu"
      pane_frames false
      theme "catppuccin-macchiato"
      default_layout "compact"
      simplified_ui true

      keybinds clear-defaults=true {
        ${builtins.readFile ./keybinds.kdl}

        shared_except "locked" {
            bind "Ctrl y" {
                LaunchOrFocusPlugin "file:${zellij-forgot}/bin/zellij_forgot.wasm" {
                    "LOAD_ZELLIJ_BINDINGS" "true"
                    floating true
                }
            }
        }
      }

    '';
}
