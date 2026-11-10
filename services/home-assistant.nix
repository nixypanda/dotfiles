{
  lib,
  homelab,
  paths,
  pkgs,
  ...
}:

let
  stateDir = paths.state.homeAssistant;
  port = homelab.services.homeAssistant.local;
in
{
  services.home-assistant = {
    enable = true;
    configDir = stateDir;

    # Keep the base runtime configuration declarative. The included automation,
    # script, and scene files persist with the config directory and are editable
    # through Home Assistant's UI.
    config = {
      default_config = { };
      # HTTP settings were migrated to Settings > System > Network and persist
      # in Home Assistant's state; retaining YAML configuration raises a repair.
      automation = "!include automations.yaml";
      script = "!include scripts.yaml";
      scene = "!include scenes.yaml";
    };

    extraComponents = [
      "default_config"
      "met"
      "esphome"
      "broadlink"
      "xiaomi_miio"
      "yeelight"
    ];

    # `hass --script check_config` imports colorlog, but upstream keeps it in
    # nativeCheckInputs rather than the runtime package dependencies.
    extraPackages = python3Packages: [ python3Packages.colorlog ];

    customComponents = with pkgs.home-assistant-custom-components; [
      philips_airpurifier_coap
      localtuya
      smartir
    ];
  };

  # The instance is reachable directly from the LAN for onboarding and local
  # device control; the tailnet HTTPS proxy remains an additional access path.
  networking.firewall.allowedTCPPorts = [ port ];

  systemd.services.home-assistant = {
    unitConfig.RequiresMountsFor = stateDir;
    # The native module's preStart runs as the hass service user. Seed each
    # include only when absent so upgrades/restarts never replace UI edits.
    preStart = lib.mkAfter ''
      if [ ! -e "${stateDir}/automations.yaml" ] && [ ! -L "${stateDir}/automations.yaml" ]; then
        printf '%s\n' '[]' > "${stateDir}/automations.yaml"
      fi
      if [ ! -e "${stateDir}/scripts.yaml" ] && [ ! -L "${stateDir}/scripts.yaml" ]; then
        printf '%s\n' '{}' > "${stateDir}/scripts.yaml"
      fi
      if [ ! -e "${stateDir}/scenes.yaml" ] && [ ! -L "${stateDir}/scenes.yaml" ]; then
        printf '%s\n' '[]' > "${stateDir}/scenes.yaml"
      fi
    '';
  };
}
