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
      grafanaAdminPassword = {
        file = ./secrets/grafana-admin-password.age;
        group = "grafana";
        mode = "0400";
        owner = "grafana";
      };
      grafanaSecretKey = {
        file = ./secrets/grafana-secret-key.age;
        group = "grafana";
        mode = "0400";
        owner = "grafana";
      };
    };
  };
}
