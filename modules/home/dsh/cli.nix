{
  pkgs,
  nodejs,
  version,
}:
pkgs.writeShellApplication {
  name = "dsh";
  runtimeInputs = [
    nodejs
    pkgs.pnpm
  ];
  text = ''
    exec npx --yes "@deepseek-ai/dsh@${version}" "$@"
  '';
}
