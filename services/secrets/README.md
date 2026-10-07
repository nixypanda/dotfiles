# Homelab Secrets

This directory stores agenix-encrypted secret files for the Rivendell homelab.

Rivendell recipient:

```text
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKyBYbJ6EtAqs6yI3LPtfcCLQdkhK2VM+ezj0Vfiz/fy root@srt-n01-rivendell
```

The NixOS config uses `/etc/ssh/ssh_host_ed25519_key` as its decryption
identity on the server. Encrypt every secret in this directory to the
Rivendell recipient above.

Current secrets:

- `calco.env.age`: wired as `age.secrets.calcoEnv` for the CalCo service.
- `kavita-token-key.age`: wired as `age.secrets.kavitaTokenKey` for Kavita
  token signing. Its plaintext must not include a trailing newline.
- `onepacerr-jellyfin-password.age`: wired as
  `age.secrets.onepacerrJellyfinPassword` for the dedicated `onepacerr`
  Jellyfin service account.
- `qbittorrent-password.age`: wired as `age.secrets.qbittorrentPassword` and
  readable by the `arr-secrets` group so Radarr and Sonarr settings-sync can
  read it.

Plaintext generation scratch files live outside the repository under
`/private/tmp/rivendell-homelab-secrets` while this setup is being worked on.
Delete that directory after recording any credentials you need.
