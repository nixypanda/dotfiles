{ pkgs, ... }:
{
  home.packages = with pkgs; [
    git-crypt
  ];
  programs.git = {
    enable = true;
    userName = "Sherub Thakur";
    userEmail = "sherub.thakur@gmail.com";

    delta = {
      enable = true;
      options = {
        features = "side-by-side line-numbers decorations";
        decorations = {
          commit-decoration-style = "bold yellow box ul";
          file-style = "bold yellow";
          file-decoration-style = "none";
        };
      };
    };

    extraConfig = {
      pull.ff = "only";

      # NOTE: Required so that `go get` can fetch private repos
      # NOTE: cargo breaks if this is present in the config
      # So you have choose between rust or go (Or find a solution for this)
      # url."ssh://git@github.com/".insteadOf = "https://github.com/";
    };
  };

  programs.gh = {
    enable = true;
    settings.git_protocol = "ssh";
  };

  programs.gpg.enable = pkgs.stdenv.isLinux;
  services.gpg-agent.enable = pkgs.stdenv.isLinux;
}
