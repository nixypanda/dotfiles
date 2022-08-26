{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    # CLI tools / Terminal facification
    awscli
  ];

  home.file.".aws/config".source = ../../.secrets/aws_config;
}
