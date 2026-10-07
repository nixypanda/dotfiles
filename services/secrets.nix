_: {
  age = {
    identityPaths = [
      "/etc/ssh/ssh_host_ed25519_key"
    ];

    secrets = {
      qbittorrentPassword = {
        file = ./secrets/qbittorrent-password.age;
        group = "arr-secrets";
        mode = "0440";
        owner = "root";
      };
      onepacerrJellyfinPassword = {
        file = ./secrets/onepacerr-jellyfin-password.age;
        group = "arr-secrets";
        mode = "0440";
        owner = "root";
      };
    };
  };
}
