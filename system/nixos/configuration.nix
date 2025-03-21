# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [ ./hardware-configuration.nix ];

  # Get me proprietary packages
  nixpkgs.config.allowUnfree = true;
  boot = {

    loader = {
      efi.canTouchEfiVariables = true;
      grub = {
        devices = [ "nodev" ];
        efiSupport = true;
        enable = true;
        useOSProber = true;
      };
    };

    kernelModules = [
      "i2c-dev"
      "i2c-piix4"
      "kvm-amd"
    ];
    initrd.kernelModules = [ "amdgpu" ];
  };

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    # https://github.com/NixOS/nixpkgs/issues/124215
    settings.extra-sandbox-paths = [ "/bin/sh=${pkgs.bash}/bin/sh" ];
  };
  networking = {

    # Enables wireless support via wpa_supplicant.
    networkmanager.enable = true;

    # The global useDHCP flag is deprecated, therefore explicitly set to false here.
    # Per-interface useDHCP will be mandatory in the future, so this generated config
    # replicates the default behaviour.
    useDHCP = false;
    interfaces.enp6s0.useDHCP = true;
    interfaces.wlp5s0.useDHCP = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Asia/Kolkata";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    openrgb
    i2c-tools
    ddccontrol
  ];
  environment.pathsToLink = [ "/libexec" ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.dconf.enable = true;
  services = {

    # List services that you want to enable:
    openssh.enable = true;

    # Enable the X11 windowing system.
    blueman.enable = true;
    xserver = {
      enable = true;
      xkbOptions = "caps:swapescape";
      videoDrivers = [ "amdgpu" ];
      autoRepeatDelay = 200;
      autoRepeatInterval = 20;

      displayManager.setupCommands = ''
        ${pkgs.xorg.xrandr}/bin/xrandr --output DisplayPort-0 --mode 3840x2160 --scale 0.70x0.70 --output DisplayPort-1 --mode 2560x1440 --rotate left --left-of DisplayPort-0
      '';

      desktopManager.session = [
        {
          name = "home-manager";
          start = ''
            ${pkgs.runtimeShell} $HOME/.hm-xsession &
            waitPID=$!
          '';
        }
      ];
    };
  };
  hardware = {
    # Logitech wireless device setup
    logitech.wireless.enable = true;
    logitech.wireless.enableGraphical = true;
    bluetooth.enable = true;
    pulseaudio.enable = true;
    enableRedistributableFirmware = true;

    opengl = {
      enable = true;
      driSupport = true;
    };
  };

  # Enable sound.
  sound.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.nixypanda = {
    isNormalUser = true;
    # Enable ‘sudo’, 'audio' for the user.
    extraGroups = [
      "wheel"
      "audio"
      "docker"
      "qemu-libvirtd"
      "libvirtd"
    ];
  };

  # Switch Pro Controller udev rules
  # NOTE: Removing these udev rules
  # services.udev.extraRules = ''
  #   ${builtins.readFile ./openrgb.rules}
  # '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

  users.extraGroups.vboxusers.members = [ "nixypanda" ];
  virtualisation = {
    virtualbox.host.enable = true;
    libvirtd.enable = true;
    docker.enable = true;
    docker.enableOnBoot = true;
  };
}
