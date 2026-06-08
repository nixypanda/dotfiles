{ config, lib, pkgs, ... }:
let
  cfg = config.services.calco;
in
{
  age.secrets.calcoEnv = {
    file = ./secrets/calco.env.age;
    mode = "0440";
    owner = "calco";
    group = "calco";
  };
  services.calco = {
    enable = true;
    port = 3002;
    secretsFile = config.age.secrets.calcoEnv.path;
  };
}
