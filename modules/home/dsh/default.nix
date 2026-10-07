{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.dsh;

  packages = import ./packages.nix { inherit pkgs lib cfg; };
in
{
  imports = [ ./home.nix ];

  options.programs.dsh = {
    enable = lib.mkEnableOption "DeepSeek Harness (dsh)";

    cli = {
      version = lib.mkOption {
        type = lib.types.str;
        default = "0.2.0-rc.2";
        description = "npm version of `@deepseek-ai/dsh` the `dsh` wrapper runs.";
      };

      nodejsVersion = lib.mkOption {
        type = lib.types.str;
        default = "24.21.0";
        description = ''
          Official nodejs.org build DSH runs under. DSH's native addon probe
          rejects nixpkgs Node, so this must be an official build.
        '';
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = packages.dshCli;
        defaultText = lib.literalExpression "the `dsh` npm-launcher wrapper";
        description = "Package providing the `dsh` command.";
      };
    };

    desktop = {
      enable = lib.mkEnableOption "DeepSeek Harness Desktop (macOS app)";

      profileName = lib.mkOption {
        type = lib.types.str;
        default = "desktop";
        description = "DSH profile owned by the desktop app.";
      };

      version = lib.mkOption {
        type = lib.types.str;
        default = "0.2.0-rc.2";
        description = ''
          Desktop release to install. Its artifact URL and sha512 are read from
          the Nightly update feed; see ../../../pkgs/deepseek-harness-desktop.nix for how to bump.
        '';
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = packages.dshDesktop;
        defaultText = lib.literalExpression "pinned DeepSeek Harness Desktop app";
        description = "The DeepSeek Harness Desktop application bundle.";
      };
    };

    profiles = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [ "web" ];
      description = "DSH profiles that plugins install into unless they override `profiles`.";
    };

    patch = lib.mkOption {
      type = lib.types.listOf lib.types.attrs;
      default = [ ];
      example = lib.literalExpression ''
        [
          {
            id = "llm-pi-ai";
            config.providers.example = {
              apiKeyEnv = "EXAMPLE_API_KEY";
            };
          }
        ]
      '';
      description = ''
        Entries for the home-level `$DSH_HOME/cordis.patch.yml`. This is the
        highest-priority DSH layer: a later layer replaces an entry's whole
        `config`, so an id-targeted patch restates the fields it keeps. Values
        must be plain data (no `!!js` expressions).
      '';
    };

    plugins = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.submodule (
          { name, ... }: {
            options = {
              enable = lib.mkOption {
                type = lib.types.bool;
                default = true;
                description = "Whether this plugin declaration is active.";
              };

              install = lib.mkOption {
                type = lib.types.bool;
                default = true;
                description = ''
                  Install the package into the target profiles with
                  `dsh plugin add` and select it as a bundle. Set false for a
                  config-only declaration over a built-in entry.
                '';
              };

              spec = lib.mkOption {
                type = lib.types.str;
                default = name;
                defaultText = lib.literalExpression "the attribute name";
                description = "pnpm spec passed to `dsh plugin add` (name, range, tag, git or file URL).";
              };

              bundle = lib.mkOption {
                type = lib.types.bool;
                default = true;
                description = "Select the package in the profile's `dsh.profile.bundles`.";
              };

              id = lib.mkOption {
                type = lib.types.nullOr lib.types.str;
                default = null;
                defaultText = lib.literalExpression "the attribute name";
                description = "Cordis entry id to patch when `config` is set.";
              };

              config = lib.mkOption {
                type = lib.types.attrs;
                default = { };
                description = "Config for the entry `id`, written into the home patch.";
              };

              profiles = lib.mkOption {
                type = lib.types.listOf lib.types.str;
                default = config.programs.dsh.profiles;
                description = "Profiles to install into. `desktop` needs `desktop.enable`.";
              };
            };
          }
        )
      );
      default = { };
      description = "Declarative DSH plugins: package installation plus home-patch config.";
    };
  };
}
