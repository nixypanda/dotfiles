_:
let
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
    agents = {
      build.permissions = permissions;
      plan.permissions = permissions;
      general.permissions = permissions;
      explore.permissions = permissions;
    };
  };
in
{
  home.file.".config/opencode/opencode.jsonc".text = builtins.toJSON opencodeConfig;
}
