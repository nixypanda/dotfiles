# Homelab Secrets

This directory stores agenix-encrypted secret files for the Rivendell homelab.

Rivendell recipient:

```text
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKyBYbJ6EtAqs6yI3LPtfcCLQdkhK2VM+ezj0Vfiz/fy root@srt-n01-rivendell
```

The NixOS config uses `/etc/ssh/ssh_host_ed25519_key` and
`/home/nixypanda/.ssh/github-key` as decryption identities on the server.

Current secrets:

- `qbittorrent-password.age`: wired as `age.secrets.qbittorrentPassword` and
  owned by the `radarr` user so Radarr settings-sync can read it.
- `radarr.env.age`: present but not currently wired into `secrets.nix`.
- `prowlarr.env.age`: present but not currently wired into `secrets.nix`.

Plaintext generation scratch files live outside the repository under
`/private/tmp/rivendell-homelab-secrets` while this setup is being worked on.
Delete that directory after recording any credentials you need.
