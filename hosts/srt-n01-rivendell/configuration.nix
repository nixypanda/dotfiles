{ lib, pkgs, ... }:

let
  homelab = import ../../services/ports.nix;
  allRegisteredPorts = lib.collect builtins.isInt homelab;
in
{
  _module.args.homelab = homelab;

  assertions = [
    {
      assertion = builtins.length allRegisteredPorts == builtins.length (lib.unique allRegisteredPorts);
      message = "Rivendell's central homelab port registry contains a duplicate port.";
    }
  ];

  imports = [
    ./hardware-configuration.nix
    ./storage.nix
    ../../modules/system/nixos
    ../../services
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  # A swapfile gives systemd-oomd room to act and absorbs memory spikes from the
  # media stack instead of letting the machine thrash or hang under pressure.
  swapDevices = [
    {
      device = "/var/lib/swapfile";
      size = 8192;
    }
  ];

  # The Realtek USB Wi-Fi dongle initially presents as a virtual CD-ROM
  # device (0bda:1a2b); usb-modeswitch flips it into NIC mode at boot.
  hardware.usb-modeswitch.enable = true;

  networking = {
    hostName = "srt-n01-rivendell";
    networkmanager.enable = true;
    # Pi-hole provides the local resolver. Keep direct upstreams as fallbacks so
    # the host can still rebuild if Pi-hole is temporarily unavailable.
    nameservers = [
      "127.0.0.1"
      "1.1.1.1"
      "9.9.9.9"
    ];
    firewall = {
      enable = true;
      allowedTCPPorts = [ homelab.firewall.ssh ];
    };
  };

  nixpkgs.config.allowUnfree = true;

  services = {
    xserver = {
      enable = true;
      displayManager.lightdm.enable = true;
      desktopManager.xfce.enable = true;
      xkb = {
        layout = "us";
      };
    };

    printing.enable = true;

    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };

    logind.settings.Login = {
      HandleLidSwitch = "ignore";
      HandleLidSwitchExternalPower = "ignore";
      HandleLidSwitchDocked = "ignore";
      HandleSuspendKey = "ignore";
      HandleHibernateKey = "ignore";
      IdleAction = "ignore";
    };
  };

  systemd.sleep.settings.Sleep = {
    AllowSuspend = "no";
    AllowHibernation = "no";
    AllowHybridSleep = "no";
    AllowSuspendThenHibernate = "no";
  };

  security.rtkit.enable = true;

  programs.firefox.enable = true;

  users.users.nixypanda = {
    isNormalUser = true;
    description = "nixypanda";
    extraGroups = [
      "networkmanager"
      "wheel"
    ];
    openssh.authorizedKeys.keyFiles = [
      ./ssh.pub
    ];
  };

  environment = {
    systemPackages = with pkgs; [
      git
      vim
      wget
      kitty.terminfo
    ];
  };

  system.stateVersion = "25.11";
}
