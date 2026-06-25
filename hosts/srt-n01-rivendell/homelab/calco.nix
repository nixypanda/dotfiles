{
  config,
  homelab,
  ...
}:
{
  age.secrets.calcoEnv = {
    file = ./secrets/calco.env.age;
    mode = "0440";
    owner = "calco";
    group = "calco";
  };
  services.calco = {
    enable = true;
    port = homelab.services.calco.local;
    secretsFile = config.age.secrets.calcoEnv.path;
  };
}
