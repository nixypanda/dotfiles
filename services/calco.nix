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
  age.secrets.calcoGroqApiKey = {
    file = ./secrets/calco-groq-api-key.age;
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
      endpoint = "https://api.groq.com/openai/v1/chat/completions";
      model = "qwen/qwen3.8-27b";
      supportsImages = true;
      authentication = {
        kind = "bearer";
        apiKeyFile = config.age.secrets.calcoGroqApiKey.path;
      };
    };
  };
}
