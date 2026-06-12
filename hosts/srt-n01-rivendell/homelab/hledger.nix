{ lib, pkgs, ... }:

let
  dataDir = "/srv/hledger";
  journalsDir = "${dataDir}/journals";
  paisaDir = "${dataDir}/paisa";

  instances = {
    mine = {
      hledgerPort = 5001;
      paisaPort = 5101;
      journal = "main-mine.journal";
      paisaConfig = "paisa-mine.yaml";
    };
    wife = {
      hledgerPort = 5002;
      paisaPort = 5102;
      journal = "main-wife.journal";
      paisaConfig = "paisa-wife.yaml";
    };
    combined = {
      hledgerPort = 5003;
      paisaPort = 5103;
      journal = "main-combined.journal";
      paisaConfig = "paisa-combined.yaml";
    };
    dummy = {
      hledgerPort = 5004;
      paisaPort = 5104;
      journal = "main-dummy.journal";
      paisaConfig = "paisa-dummy.yaml";
    };
  };

  mkHledgerService =
    name:
    {
      hledgerPort,
      journal,
      ...
    }:
    let
      args = lib.escapeShellArgs [
        "--serve-api"
        "--allow=view"
        "--host=127.0.0.1"
        "--cors=*"
        "--port=${toString hledgerPort}"
        "--file=${journalsDir}/${journal}"
      ];
    in
    {
      name = "hledger-web-${name}";
      value = {
        description = "hledger-web JSON API for ${name}";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        serviceConfig = {
          ExecStart = "${pkgs.hledger-web}/bin/hledger-web ${args}";
          Restart = "always";
          WorkingDirectory = dataDir;
          User = "hledger";
          Group = "hledger";
          PrivateTmp = true;
          NoNewPrivileges = true;
        };
      };
    };

  mkPaisaService =
    name:
    {
      paisaPort,
      paisaConfig,
      ...
    }:
    let
      args = lib.escapeShellArgs [
        "--config"
        "${paisaDir}/${paisaConfig}"
        "serve"
        "--port"
        (toString paisaPort)
      ];
    in
    {
      name = "paisa-${name}";
      value = {
        description = "Paisa web UI for ${name}";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        path = [ pkgs.hledger ];

        serviceConfig = {
          ExecStart = "${pkgs.paisa}/bin/paisa ${args}";
          Restart = "always";
          WorkingDirectory = paisaDir;
          User = "nixypanda";
          Group = "hledger";
          PrivateTmp = true;
          NoNewPrivileges = true;
          IPAddressDeny = "any";
          IPAddressAllow = [
            "127.0.0.0/8"
            "::1"
          ];
        };
      };
    };
in
{
  environment.systemPackages = [
    pkgs.hledger
    pkgs.paisa
  ];

  users = {
    groups.hledger = { };
    users = {
      hledger = {
        isSystemUser = true;
        group = "hledger";
        home = dataDir;
        useDefaultShell = true;
      };

      nixypanda.extraGroups = [ "hledger" ];
    };
  };

  systemd = {
    services = lib.mapAttrs' mkHledgerService instances // lib.mapAttrs' mkPaisaService instances;

    tmpfiles.rules = [
      "d ${dataDir} 2775 hledger hledger - -"
      "d ${journalsDir} 2775 hledger hledger - -"
      "d ${journalsDir}/years 2775 hledger hledger - -"
      "d ${paisaDir} 2775 nixypanda hledger - -"
    ];
  };
}
