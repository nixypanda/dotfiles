{
  pkgs,
  lib,
  cfg,
}:
rec {
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
  # pkgs.nodejs; see ../../../pkgs/nodejs-official.nix.
  #
  # pnpm is installed because profile/plugin operations forward to it.
  nodejsOfficial = pkgs.callPackage ../../../pkgs/nodejs-official.nix {
    version = cfg.cli.nodejsVersion;
  };

  dshCli = import ./cli.nix {
    inherit pkgs;
    nodejs = nodejsOfficial;
    version = cfg.cli.version;
  };

  dshDesktop = pkgs.callPackage ../../../pkgs/deepseek-harness-desktop.nix {
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
}
