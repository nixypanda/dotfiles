_: {
  age = {
    identityPaths = [
      "/etc/ssh/ssh_host_ed25519_key"
      "/home/nixypanda/.ssh/github-key"
    ];

    secrets = {
      qbittorrentPassword = {
        file = ./secrets/qbittorrent-password.age;
        group = "arr-secrets";
        mode = "0440";
        owner = "root";
      };
    };
  };
}
