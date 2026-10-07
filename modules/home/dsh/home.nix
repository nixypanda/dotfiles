{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.dsh;

  packages = import ./packages.nix { inherit pkgs lib cfg; };

  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;
in
{
  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = !cfg.desktop.enable || isDarwin;
        message = "programs.dsh.desktop is only available on macOS.";
      }
      {
        assertion = lib.all (
          plugin: lib.all (profile: profile != cfg.desktop.profileName || cfg.desktop.enable) plugin.profiles
        ) (lib.attrValues packages.installingPlugins);
        message = "programs.dsh: a plugin targets the desktop profile but programs.dsh.desktop.enable is false.";
      }
    ];

    home = {
      packages = [
        packages.nodejsOfficial
        pkgs.pnpm
        cfg.cli.package
      ]
      ++ lib.optional (cfg.desktop.enable && isDarwin) cfg.desktop.package
      ++ lib.optional (packages.installingPlugins != { }) packages.dshPluginsSync;

      file.".dsh/cordis.patch.yml".source =
        packages.yaml.generate "cordis.patch.yml" packages.patchEntries;

      activation.dshPlugins = lib.mkIf (packages.installingPlugins != { }) (
        lib.hm.dag.entryAfter [ "writeBoundary" ] ''
          run ${packages.dshPluginsSync}/bin/dsh-plugins-sync
        ''
      );
    };
  };
}
