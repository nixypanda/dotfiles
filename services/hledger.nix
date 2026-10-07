{
  homelab,
  lib,
  pkgs,
  ...
}:

let
  dataDir = "/srv/hledger";
  journalsDir = "${dataDir}/journals";
  pricesDir = "${dataDir}/prices";
  paisaDir = "${dataDir}/paisa";
  hedgerDir = "${dataDir}/hedger";

  instances = {
    mine = {
      configFile = "${hedgerDir}/hedger-mine.yaml";
      hedgerPort = homelab.finance.mine.hedger;
      journal = "main-mine.journal";
    };
    wife = {
      hedgerPort = homelab.finance.wife.hedger;
      journal = "main-wife.journal";
    };
    combined = {
      hedgerPort = homelab.finance.combined.hedger;
      journal = "main-combined.journal";
    };
    dummy = {
      hedgerPort = homelab.finance.dummy.hedger;
      journal = "main-dummy.journal";
    };
  };

  paisaInstances = {
    mine = {
      paisaPort = homelab.finance.mine.paisa;
      paisaConfig = "paisa-mine.yaml";
    };
  };

  mkHedgerInstance =
    name:
    {
      configFile ? null,
      hedgerPort,
      journal,
      ...
    }:
    {
      enable = true;
      hostName = "${name}.hedger.internal";
      journalPath = "${journalsDir}/${journal}";
      port = hedgerPort;
      defaultCommodity = "INR";
      fiscalYearStartMonth = 4;
      supplementaryGroups = [ "hledger" ];
    }
    // lib.optionalAttrs (configFile != null) {
      inherit configFile;
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
  services = {
    hedger = {
      openFirewall = false;
      instances = lib.mapAttrs mkHedgerInstance instances;
    };

    nginx.defaultListen = [
      {
        addr = "127.0.0.1";
        port = homelab.services.hedger.local;
      }
    ];
  };

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
    services = lib.mapAttrs' mkPaisaService paisaInstances;

    tmpfiles.rules = [
      "d ${dataDir} 2775 hledger hledger - -"
      "d ${journalsDir} 2775 hledger hledger - -"
      "d ${journalsDir}/years 2775 hledger hledger - -"
      "d ${pricesDir} 2775 hledger hledger - -"
      "d ${paisaDir} 2775 nixypanda hledger - -"
      "d ${hedgerDir} 2775 nixypanda hledger - -"
    ];
  };
}
