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
  age.secrets.calcoOpencodeApiKey = {
    file = ./secrets/calco-opencode-api-key.age;
    mode = "0400";
    owner = "calco";
    group = "calco";
  };
  services.calco = {
    enable = true;
    port = homelab.services.calco.local;
    secretsFile = config.age.secrets.calcoEnv.path;
    assistant = {
      enable = true;
      endpoint = "https://opencode.ai/inference/go/openai/v1/chat/completions";
      model = "deepseek-v4.1-flash";
      supportsImages = true;
      authentication = {
        kind = "bearer";
        apiKeyFile = config.age.secrets.calcoOpencodeApiKey.path;
      };
    };
  };
}
