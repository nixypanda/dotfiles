{ pkgs, ... }: {
  home.packages = with pkgs; [ consul nomad packer terraform vagrant vault ];
}
