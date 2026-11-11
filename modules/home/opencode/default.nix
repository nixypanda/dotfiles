{ pkgs, opencode-rex-activity, ... }:
let
  # OpenCode accepts sound paths, not per-event mute flags. A valid silent WAV
  # suppresses an event without triggering its built-in fallback sound.
  silence = pkgs.runCommand "opencode-silence.wav" { nativeBuildInputs = [ pkgs.python3 ]; } ''
    python3 - <<'PY'
    import os
    import wave

    with wave.open(os.environ["out"], "wb") as sound:
        sound.setnchannels(1)
        sound.setsampwidth(2)
        sound.setframerate(44100)
        sound.writeframes(b"\x00\x00" * 8820)
    PY
  '';

  # Paths outside the active project prompt for approval by default (the
  # `external_directory` base policy asks). Allow /tmp read+write and
  # /nix/store read-only. Global `permissions` are inherited by every agent,
  # but shipped agents append their own rules after them, so mirror the same
  # set into each tool-running agent. `title` and `summary` deny all actions
  # and never touch the filesystem, so they are left alone.
  permissions = [
    # macOS /tmp is a symlink to /private/tmp; explicit paths are canonicalized
    # before matching, so cover both spellings.
    {
      action = "external_directory";
      resource = "/tmp/*";
      effect = "allow";
    }
    {
      action = "external_directory";
      resource = "/private/tmp/*";
      effect = "allow";
    }
    {
      action = "read";
      resource = "/tmp/*";
      effect = "allow";
    }
    {
      action = "edit";
      resource = "/tmp/*";
      effect = "allow";
    }
    {
      action = "read";
      resource = "/private/tmp/*";
      effect = "allow";
    }
    {
      action = "edit";
      resource = "/private/tmp/*";
      effect = "allow";
    }
    # The Nix store stays read-only.
    {
      action = "external_directory";
      resource = "/nix/store/*";
      effect = "allow";
    }
    {
      action = "read";
      resource = "/nix/store/*";
      effect = "allow";
    }
    {
      action = "edit";
      resource = "/nix/store/*";
      effect = "deny";
    }
  ];

  opencodeConfig = {
    "$schema" = "https://opencode.ai/config.json";
    inherit permissions;
    plugins = [ "./plugins/rex-activity" ];
    agents = {
      build.permissions = permissions;
      plan.permissions = permissions;
      general.permissions = permissions;
      explore.permissions = permissions;
    };
  };
in
{
  home.file = {
    ".config/opencode/opencode.jsonc".text = builtins.toJSON opencodeConfig;
    ".config/opencode/sounds/silence.wav".source = silence;
    ".config/opencode/plugins/rex-activity" = {
      # Production sources only, from the pinned external input; tests and repo
      # metadata stay out of the install.
      source = builtins.path {
        path = opencode-rex-activity;
        filter =
          path: _type:
          builtins.elem (baseNameOf path) [
            "index.ts"
            "runtime.ts"
            "state.ts"
            "tui.ts"
          ];
      };
      recursive = true;
    };
  };
}
