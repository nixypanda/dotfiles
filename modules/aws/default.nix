{ pkgs, ... }:
{
  home.packages = with pkgs; [ awscli ];

  home.file.".aws/config".source = ../../.secrets/aws_config;
}
