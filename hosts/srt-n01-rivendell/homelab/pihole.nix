{
  config,
  homelab,
  lib,
  pkgs,
  ...
}:

let
  pihole = config.services.pihole-ftl.piholePackage;
  blocklists = [
    {
      url = "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts";
      description = "StevenBlack unified hosts";
    }

    # Firebog's green/ticked lists: https://firebog.net/
    # Suspicious
    {
      url = "https://raw.githubusercontent.com/PolishFiltersTeam/KADhosts/master/KADhosts.txt";
      description = "PolishFiltersTeam KADhosts";
    }
    {
      url = "https://raw.githubusercontent.com/FadeMind/hosts.extras/master/add.Spam/hosts";
      description = "Fademind's Spammers";
    }
    {
      url = "https://v.firebog.net/hosts/static/w3kbl.txt";
      description = "WaLLy3K's Blacklist";
    }

    # Advertising
    {
      url = "https://adaway.org/hosts.txt";
      description = "AdAway";
    }
    {
      url = "https://v.firebog.net/hosts/AdguardDNS.txt";
      description = "Adguard DNS";
    }
    {
      url = "https://v.firebog.net/hosts/Admiral.txt";
      description = "LanikSJ's Admiral Anti-Adblock";
    }
    {
      url = "https://raw.githubusercontent.com/anudeepND/blacklist/master/adservers.txt";
      description = "Anudeep ND's Blacklist";
    }
    {
      url = "https://v.firebog.net/hosts/Easylist.txt";
      description = "Easylist";
    }
    {
      url = "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&showintro=0&mimetype=plaintext";
      description = "Peter Lowe's Adservers";
    }
    {
      url = "https://raw.githubusercontent.com/FadeMind/hosts.extras/master/UncheckyAds/hosts";
      description = "Fademind's Unchecky Ads";
    }
    {
      url = "https://raw.githubusercontent.com/bigdargon/hostsVN/master/hosts";
      description = "hostsVN";
    }

    # Tracking & Telemetry
    {
      url = "https://v.firebog.net/hosts/Easyprivacy.txt";
      description = "Easyprivacy";
    }
    {
      url = "https://v.firebog.net/hosts/Prigent-Ads.txt";
      description = "Fabrice Prigent's Ads";
    }
    {
      url = "https://raw.githubusercontent.com/FadeMind/hosts.extras/master/add.2o7Net/hosts";
      description = "Fademind's 2o7 Network Trackers";
    }
    {
      url = "https://raw.githubusercontent.com/crazy-max/WindowsSpyBlocker/master/data/hosts/spy.txt";
      description = "Crazy Max's Microsoft Telemetry";
    }
    {
      url = "https://hostfiles.frogeye.fr/firstparty-trackers-hosts.txt";
      description = "Geoffrey Frogeye's First-Party Trackers";
    }

    # Malicious
    {
      url = "https://raw.githubusercontent.com/DandelionSprout/adfilt/master/Alternate%20versions%20Anti-Malware%20List/AntiMalwareHosts.txt";
      description = "DandelionSprout's Anti Malware Filter";
    }
    {
      url = "https://v.firebog.net/hosts/Prigent-Crypto.txt";
      description = "Fabrice Prigent's Cryptojacking";
    }
    {
      url = "https://raw.githubusercontent.com/FadeMind/hosts.extras/master/add.Risk/hosts";
      description = "Fademind's Risky Hosts";
    }
    {
      url = "https://phishing.army/download/phishing_army_blocklist_extended.txt";
      description = "Phishing Army's Extended Blocklist";
    }
    {
      url = "https://gitlab.com/quidsup/notrack-blocklists/raw/master/notrack-malware.txt";
      description = "Quidsup Malicious";
    }
    {
      url = "https://raw.githubusercontent.com/Spam404/lists/master/main-blacklist.txt";
      description = "Spam404";
    }
    {
      url = "https://raw.githubusercontent.com/AssoEchap/stalkerware-indicators/master/generated/hosts";
      description = "Echap's Stalkerware Indicators";
    }
    {
      url = "https://urlhaus.abuse.ch/downloads/hostfile/";
      description = "URLhaus Malicious URL blocklist";
    }
    {
      url = "https://lists.cyberhost.uk/malware.txt";
      description = "CyberHost.uk Malware Domains";
    }
  ];
  setupResolvConf = pkgs.writeText "pihole-ftl-setup-resolv.conf" ''
    nameserver 1.1.1.1
    nameserver 9.9.9.9
  '';
in
{
  services = {
    pihole-ftl = {
      enable = true;
      openFirewallDNS = true;
      openFirewallWebserver = true;
      queryLogDeleter.enable = true;
      lists = blocklists;
      settings = {
        misc.readOnly = false;
        webserver.api.cli_pw = true;
        dns = {
          domainNeeded = true;
          expandHosts = true;
          listeningMode = "ALL";
          upstreams = [
            "1.1.1.1"
            "1.0.0.1"
            "9.9.9.9"
          ];
          hosts = [
            "192.168.1.76 srt-n01-rivendell rivendell"
            "192.168.1.76 jellyfin"
            "192.168.1.76 radarr"
            "192.168.1.76 sonarr"
            "192.168.1.76 prowlarr"
            "192.168.1.76 qbittorrent"
            "192.168.1.76 jellyseerr seerr"
          ];
        };
      };
    };

    pihole-web = {
      enable = true;
      hostName = "pi.hole";
      ports = [ homelab.services.pihole.web ];
    };
  };

  # Remove this override once nixpkgs includes
  # 8f960c365db087e2712b4c383146802be339095d, which fixes the Pi-hole v6
  # lists API call to pass the list type as a query parameter.
  systemd.services.pihole-ftl-setup = {
    # Do not make Pi-hole's setup traffic depend on the local resolver while
    # Pi-hole itself is being restarted during activation.
    serviceConfig.BindReadOnlyPaths = [ "${setupResolvConf}:/etc/resolv.conf" ];

    script =
      lib.mkForce # bash
        ''
          set -eo pipefail

          pihole="${lib.getExe pihole}"
          jq="${lib.getExe pkgs.jq}"

          ${lib.getExe pkgs.curl} --retry 3 --retry-delay 5 \
            "${config.services.pihole-ftl.macvendorURL}" \
            -o "${config.services.pihole-ftl.settings.files.macvendor}" \
            || echo "Failed to download MAC database"

          if [ ! -f '${config.services.pihole-ftl.settings.files.gravity}' ]; then
            $pihole -g
            ${lib.getExe' pkgs.procps "kill"} -s SIGRTMIN "$(${lib.getExe' pkgs.systemd "systemctl"} show --property MainPID --value pihole-ftl.service)"
          fi

          source ${pihole}/share/pihole/advanced/Scripts/api.sh
          source ${pihole}/share/pihole/advanced/Scripts/utils.sh

          for i in 1 2 3; do
            (TestAPIAvailability) && break
            echo "Retrying API shortly..."
            ${lib.getExe' pkgs.coreutils "sleep"} .5s
          done

          LoginAPI

          add_blocklist() {
            description="$1"
            payload="$2"
            result=$(PostFTLData "lists?type=block" "$payload")
            error="$($jq '.error' <<< "$result")"
            if [[ "$error" != "null" ]]; then
              error_key="$($jq -r '.error.key // ""' <<< "$result")"
              error_message="$($jq -r '.error.message // ""' <<< "$result")"
              error_hint="$($jq -r '.error.hint // ""' <<< "$result")"
              duplicate_constraint="UNIQUE constraint failed: adlist.address, adlist.type"
              already_present="The item is already present"

              if [[ "$error_key" == "database_error" ]] \
                && [[ "$error_hint" == *"$duplicate_constraint"* || "$error_hint" == "$already_present" ]]; then
                echo "$description blocklist already exists"
              else
                echo "Error adding $description blocklist: key=$error_key message=$error_message hint=$error_hint"
                exit 1
              fi
            else
              echo "Added $description blocklist"
            fi
          }

          ${lib.concatMapStringsSep "\n" (list: ''
            add_blocklist ${lib.escapeShellArg list.description} ${
              lib.escapeShellArg (
                builtins.toJSON {
                  address = [ list.url ];
                  comment = list.description;
                  groups = [ 0 ];
                }
              )
            }
          '') blocklists}

          $pihole -g
        '';
  };
}
