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
      port = homelab.finance.mine.hedger;
      ledgers = {
        mine = {
          label = "Mine";
          journal = "main-mine.journal";
          config = "hedger-mine.yaml";
        };
        wife = {
          label = "Wife";
          journal = "main-wife.journal";
          config = "hedger-wife.yaml";
        };
        combined = {
          label = "Combined";
          journal = "main-combined.journal";
          config = "hedger-combined.yaml";
        };
      };
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
      port,
      ledgers,
      ...
    }:
    {
      enable = true;
      hostName = "${name}.hedger.internal";
      inherit port;
      defaultLedger = null;
      supplementaryGroups = [ "hledger" ];
      ledgers = lib.mapAttrs (_ledgerName: ledgerCfg: {
        inherit (ledgerCfg) label;
        journalPath = "${journalsDir}/${ledgerCfg.journal}";
        configPath = "${hedgerDir}/${ledgerCfg.config}";
      }) ledgers;
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
