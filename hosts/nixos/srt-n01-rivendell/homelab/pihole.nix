{
  config,
  lib,
  pkgs,
  ...
}:

let
  pihole = config.services.pihole-ftl.piholePackage;
in
{
  services = {
    pihole-ftl = {
      enable = true;
      openFirewallDNS = true;
      openFirewallWebserver = true;
      queryLogDeleter.enable = true;
      lists = [
        {
          url = "https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts";
          description = "StevenBlack unified hosts";
        }
      ];
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
      ports = [ 8081 ];
    };
  };

  # Remove this override once nixpkgs includes
  # 8f960c365db087e2712b4c383146802be339095d, which fixes the Pi-hole v6
  # lists API call to pass the list type as a query parameter.
  systemd.services.pihole-ftl-setup.script =
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

        payload='{"address":["https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts"],"comment":"StevenBlack unified hosts","groups":[0]}'
        result=$(PostFTLData "lists?type=block" "$payload")
        error="$($jq '.error' <<< "$result")"
        if [[ "$error" != "null" ]]; then
          error_key="$($jq -r '.error.key // ""' <<< "$result")"
          error_message="$($jq -r '.error.message // ""' <<< "$result")"
          error_hint="$($jq -r '.error.hint // ""' <<< "$result")"
          duplicate_hint="UNIQUE constraint failed: adlist.address, adlist.type"

          if [[ "$error_key" == "database_error" && "$error_hint" == *"$duplicate_hint"* ]]; then
            echo "StevenBlack blocklist already exists"
          else
            echo "Error adding list: key=$error_key message=$error_message hint=$error_hint"
            exit 1
          fi
        else
          echo "Added StevenBlack blocklist"
        fi

        $pihole -g
      '';
}
