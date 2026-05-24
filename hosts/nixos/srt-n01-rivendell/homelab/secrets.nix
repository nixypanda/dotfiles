_: {
  age = {
    identityPaths = [
      "/etc/ssh/ssh_host_ed25519_key"
      "/home/nixypanda/.ssh/github-key"
    ];

    secrets = {
      qbittorrentPassword = {
        file = ./secrets/qbittorrent-password.age;
        owner = "radarr";
      };
    };
  };
}
