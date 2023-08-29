{ pkgs, ... }: {
  home.packages = with pkgs; [
    # CLI tools / Terminal facification
    arandr
    bashmount
    dig
    docker-compose
    dua
    fx
    fd
    gnumake
    graphviz
    hexyl
    jq
    lsd
    ngrok
    nodePackages.prettier
    openvpn
    tokei
    unrar
    unzip
    # Build failing on MacOS
    # wireshark
    pgcli
    dogdns
    drone-cli

    ruff
    shfmt

    # Moar colors
    zsh-syntax-highlighting

    # Searching/Movement helpers
    ripgrep
    zoxide

    # system info
    bottom
    neofetch

    # Nix itself
    nix
  ];

  programs.starship = { enable = true; };
}
