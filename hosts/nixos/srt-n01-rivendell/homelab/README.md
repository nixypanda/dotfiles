# Rivendell Homelab

This host keeps the service shape declarative and leaves app-owned setup state
inside each application's state directory.

## Paths

- Movies: `/srv/media/movies`
- Torrent downloads: `/srv/downloads/torrents`
- Completed torrents: `/srv/downloads/torrents/complete`
- Incomplete torrents: `/srv/downloads/torrents/incomplete`

The active paths are also written to `/etc/homelab/media-paths`.

## Services

- Homepage: `http://srt-n01-rivendell:8082`
- Jellyfin: `http://192.168.1.76:8096`
- Radarr: `http://192.168.1.76:7878`
- Prowlarr: `http://192.168.1.76:9696`
- qBittorrent: `http://192.168.1.76:8080`
- Seerr: `http://192.168.1.76:5055`
- Pi-hole: `http://192.168.1.76:8081`

## Declarative State

Nix currently declares:

- all services via the [nixarr](https://nixarr.com) module
- Homepage dashboard over Tailscale MagicDNS
- firewall ports
- service ports
- qBittorrent download paths and Web UI password (PBKDF2 hash, not plaintext)
- Prowlarr app sync (settings-sync)
- Radarr download client (qBittorrent) via settings-sync
- Pi-hole upstreams and local DNS records

Nixarr also handles:

- media user/group creation
- state management under `/srv/.state/nixarr/`
- Prometheus exporters (optional)

Nix intentionally does not declare first-run database state for Jellyfin,
Seerr, Radarr, or Prowlarr. Those apps store setup state in their own state
directories under `/srv/.state/nixarr/`. Back up `/srv/.state/nixarr/` after
first setup.

## Secrets

The qBittorrent Web UI password is encrypted with agenix and decrypted at
runtime to `/run/agenix/qbittorrentPassword`. It is used both as the qBittorrent
login credential and by Radarr's settings-sync to authenticate as a download
client.

The repository also contains encrypted `radarr.env.age` and `prowlarr.env.age`
files reserved for future service environment secrets. They are not currently
wired into `secrets.nix`.

The PBKDF2-SHA512 hash in `media.nix` is what qBittorrent stores — this is a
one-way hash, not a plaintext secret, and is safe in the Nix store.

To generate a new qBittorrent-compatible PBKDF2 hash from a plaintext password,
use Python on any machine (no Nix build needed):

```python
import hashlib, base64, os
password = b"your-new-password"
salt = os.urandom(16)
dk = hashlib.pbkdf2_hmac("sha512", password, salt, 100000)
print(f"@ByteArray({base64.b64encode(salt).decode()}:{base64.b64encode(dk).decode()})")
```

API keys for the *arr stack are managed internally by nixarr. Use
`sudo nixarr list-api-keys` on the server to view them.

## First Setup

The following still needs one-time manual setup in the web UI:

1. Radarr — Settings → Media Management → add root folder `/srv/media/movies`
2. Jellyfin — first-run wizard: create admin user, add `/srv/media/movies` as Movies
3. Seerr — first-run wizard: connect Jellyfin URL, connect Radarr (API key from `sudo nixarr list-api-keys`)
4. Prowlarr indexers — add them via the web UI, or add declarative settings to `media.nix`

The following is handled automatically by nixarr on deploy:

- qBittorrent added as Radarr's download client
- Radarr synced to Prowlarr as an application
- State directories under `/srv/.state/nixarr/`
