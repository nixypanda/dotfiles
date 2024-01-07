{ pkgs, ... }: {
  home.packages = with pkgs; [
    # consul
    # nomad_1_4
    packer
    # terraform
    # vagrant
    vault
  ];
}
