{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.dsh;

  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;

  # DeepSeek Harness (dsh) is a Node CLI whose profiles are mutable pnpm
  # projects under $DSH_HOME: the LLM adapters (including
  # @deepseek-ai/dsh-llm-pi-ai) and out-of-tree plugins are installed into a
  # profile at runtime, not bundled with the CLI. That is DSH's design, so a
  # fully hermetic Nix package would fight it. The `dsh` wrapper pins the CLI
  # version and runs it through the npm launcher DSH documents
  # (`npx @deepseek-ai/dsh web`).
  #
  # DSH boots through node-addon-require-builtin, which pattern-matches the
  # running Node binary's machine code. That probe only recognises official
  # nodejs.org builds, so the wrapper runs under nodejs-official rather than
  # pkgs.nodejs; see ../../pkgs/nodejs-official.nix.
  #
  # pnpm is installed because profile/plugin operations forward to it.
  nodejsOfficial = pkgs.callPackage ../../pkgs/nodejs-official.nix {
    version = cfg.cli.nodejsVersion;
  };

  dshCli = import ./cli.nix {
    inherit pkgs;
    nodejs = nodejsOfficial;
    version = cfg.cli.version;
  };

  dshDesktop = pkgs.callPackage ../../pkgs/deepseek-harness-desktop.nix {
    version = cfg.desktop.version;
  };

  yaml = pkgs.formats.yaml { };

  # Plugins the user turned on. `install` is separate from `enable` so a
  # declaration can be config-only (e.g. patching a built-in entry) without
  # asking DSH to install a package.
  enabledPlugins = lib.filterAttrs (_: plugin: plugin.enable) cfg.plugins;
  installingPlugins = lib.filterAttrs (_: plugin: plugin.enable && plugin.install) cfg.plugins;

  # Config overrides for declared plugins. A home-level patch replaces the
  # whole config of the entry it names, so `id` must be the Cordis entry id
  # (defaults to the package name, which is right for a package that exports
  # its plugin under its own name).
  pluginConfigPatches = lib.mapAttrsToList (name: plugin: {
    id = if plugin.id != null then plugin.id else name;
    inherit (plugin) config;
  }) (lib.filterAttrs (_: plugin: plugin.config != { }) enabledPlugins);

  patchEntries = cfg.patch ++ pluginConfigPatches;

  # `dsh plugin add` installs a package into a profile and selects it as a
  # bundle. It writes the mutable profile manifest, so this runs at activation
  # and is idempotent: a plugin already present and selected is skipped, and a
  # failed run leaves the profile untouched (DSH rolls pnpm back).
  pluginInstallLines = lib.concatStrings (
    lib.mapAttrsToList (
      name: plugin:
      lib.concatMapStrings (profile: ''
        maybe_install ${lib.escapeShellArg profile} ${lib.escapeShellArg plugin.spec} ${lib.escapeShellArg name} ${
          if plugin.bundle then "1" else "0"
        } ${if profile == cfg.desktop.profileName then "1" else "0"}
      '') plugin.profiles
    ) installingPlugins
  );

  desktopRoot = lib.optionalString cfg.desktop.enable (toString cfg.desktop.package);

  dshPluginsSync = import ./plugins-sync.nix {
    inherit pkgs;
    nodejs = nodejsOfficial;
    dsh = dshCli;
    inherit desktopRoot;
    installLines = pluginInstallLines;
  };
in
{
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
        default = dshCli;
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
          the Nightly update feed; see ../../pkgs/deepseek-harness-desktop.nix for how to bump.
        '';
      };

      package = lib.mkOption {
        type = lib.types.package;
        default = dshDesktop;
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

  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !cfg.desktop.enable || isDarwin;
        message = "programs.dsh.desktop is only available on macOS.";
      }
      {
        assertion = lib.all (
          plugin: lib.all (profile: profile != cfg.desktop.profileName || cfg.desktop.enable) plugin.profiles
        ) (lib.attrValues installingPlugins);
        message = "programs.dsh: a plugin targets the desktop profile but programs.dsh.desktop.enable is false.";
      }
    ];

    home = {
      packages = [
        nodejsOfficial
        pkgs.pnpm
        cfg.cli.package
      ]
      ++ lib.optional (cfg.desktop.enable && isDarwin) cfg.desktop.package
      ++ lib.optional (installingPlugins != { }) dshPluginsSync;

      file.".dsh/cordis.patch.yml".source = yaml.generate "cordis.patch.yml" patchEntries;

      activation.dshPlugins = lib.mkIf (installingPlugins != { }) (
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          run ${dshPluginsSync}/bin/dsh-plugins-sync
        ''
      );
    };
  };
}
