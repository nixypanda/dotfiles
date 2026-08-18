# Rivendell Homelab

This host keeps the service shape declarative and leaves app-owned setup state
inside each application's state directory.

## Paths

- Movies: `/srv/media/library/movies`
- TV: `/srv/media/library/shows`
- Books: `/srv/media/library/books`
- Manga: `/srv/media/library/manga`
- Audiobooks: `/srv/media/library/audiobooks`
- Audiobook downloads: `/srv/media/downloads/audiobooks`
- Torrent downloads: `/srv/downloads/torrents`
- Completed torrents: `/srv/downloads/torrents/complete`
- Incomplete torrents: `/srv/downloads/torrents/incomplete`
- Audiobookshelf state: `/srv/.state/audiobookshelf`
- Shelfmark state: `/srv/.state/shelfmark`

The active paths are also written to `/etc/homelab/media-paths` and
`/etc/homelab/ebook-paths`. Audiobook paths are written to
`/etc/homelab/audiobook-paths`.

## Services

Service ports, tailnet-facing aliases, and the tailnet host are defined in
`ports.nix`. The Homepage dashboard uses that file to publish service links.

Ports 9450-9453 serve Hedger over the tailnet:

- 9450: Hedger Mine
- 9451: Hedger Wife
- 9452: Hedger Combined
- 9453: Hedger Dummy

The audiobook services are available only through Caddy on the tailnet:

- Audiobookshelf: `https://srt-n01-rivendell.taila65e7f.ts.net:9466`
- Shelfmark: `https://srt-n01-rivendell.taila65e7f.ts.net:9467`

Their application ports (`127.0.0.1:8000` and `127.0.0.1:8084`) stay bound to
localhost and are not opened in the firewall.

All four Hedger instances use the existing journals under
`/srv/hledger/journals`. Caddy terminates Tailscale TLS on ports 9450-9453 and
proxies to Hedger's module-managed nginx listener on `127.0.0.1:8083`. nginx
selects the matching ledger by its internal hostname, serves the shared
frontend, and proxies `/api/*` to the matching localhost-only Hedger backend on
ports 5001-5004.

The Mine instance also reads
`/srv/hledger/hedger/hedger-mine.yaml`. Journal paths, backend ports, display
commodity, and fiscal-year settings are supplied as command-line arguments by
the NixOS module and override matching YAML values. The other instances do not
use a Hedger YAML configuration.

Hedger loads each journal into an immutable snapshot at startup. The
`sync-hledger` command reloads all four `hedger-*` services after synchronizing
journals or prices. `sync-hledger --all` also copies
`hedger/hedger-mine.yaml`; bootstrap copies it with the rest of the ledger root.

## Declarative State

Nix currently declares:

- all services via the [nixarr](https://nixarr.com) module
- Homepage dashboard over Tailscale MagicDNS
- firewall ports
- service ports
- qBittorrent download paths and Web UI password (PBKDF2 hash, not plaintext)
- Kavita service settings, token key secret, and library directories
- Audiobookshelf through the nixarr module
- Shelfmark through the nixarr module and locked Nixpkgs package
- Prowlarr app sync (settings-sync)
- Radarr and Sonarr download client (qBittorrent) via settings-sync
- Pi-hole upstreams and local DNS records

Nixarr also handles:

- media user/group creation
- state management under `/srv/.state/nixarr/`
- Prometheus exporters (optional)

## Observability

The native LGTM stack is declared in `observability.nix`. Its design,
integration discovery, privacy boundaries, and future approval checkpoints are
recorded in `observability-plan.md`.

Grafana is the only observability component exposed through Caddy:

- Grafana: `https://srt-n01-rivendell.taila65e7f.ts.net:9468`

Loki, Tempo, Prometheus, the systemd exporter, and both OTLP endpoints listen
only on `127.0.0.1`. No observability firewall ports are opened.

The currently approved collection scope is limited to:

- coarse host CPU, load, memory, disk, filesystem, network, paging, and system
  metrics
- health and restart counts for an explicit allowlist of core systemd and
  observability units
- Grafana, Loki, Tempo, Prometheus, and OpenTelemetry Collector self-metrics

No journal messages or application-specific telemetry are collected.

Runtime state uses the native systemd-managed directories:

- Grafana: `/var/lib/grafana`
- Loki: `/var/lib/loki`
- Tempo: `/var/lib/tempo`
- Prometheus: `/var/lib/prometheus2`
- OpenTelemetry Collector: `/var/lib/opentelemetry-collector`

Back up Grafana and both encrypted Grafana secret files. Prometheus, Loki, and
Tempo contain disposable operational telemetry unless longer-term history is
important. Stop the relevant service or use an upstream-supported snapshot
mechanism before copying a live telemetry database.

After an authorized deployment, retrieve Grafana's initial admin password
locally on Rivendell:

```sh
sudo cat /run/agenix/grafanaAdminPassword
```

Then verify:

```sh
systemctl --failed
systemctl status grafana loki tempo prometheus opentelemetry-collector
ss -lntp
curl --fail http://127.0.0.1:9090/-/ready
curl --fail http://127.0.0.1:3100/ready
curl --fail http://127.0.0.1:3200/ready
curl --fail http://127.0.0.1:3000/api/health
curl --fail http://127.0.0.1:8888/metrics
curl --fail http://127.0.0.1:8889/metrics
```

Nix intentionally does not declare first-run database state for Jellyfin,
Seerr, Radarr, Prowlarr, Audiobookshelf, or Shelfmark. The nixarr apps store
setup state under `/srv/.state/nixarr/`; Audiobookshelf and Shelfmark use the
state paths listed above. Back up all three state locations after first setup
and before application upgrades. The Audiobookshelf and Shelfmark backups
contain their databases, configuration, and application metadata, but the
audiobook files under `/srv/media/library/audiobooks` need a separate media
backup.

## Secrets

The qBittorrent Web UI password is encrypted with agenix and decrypted at
runtime to `/run/agenix/qbittorrentPassword`. It is used both as the qBittorrent
login credential and by the Radarr/Sonarr settings-sync jobs to authenticate as
a download client.

OnePacerr runs directly as a native Node.js systemd service. It monitors One
Pace releases, queues missing or updated episodes in qBittorrent, and hard-links
completed downloads into the Jellyfin shows library. Its dedicated Jellyfin
account password is decrypted to
`/run/agenix/onepacerrJellyfinPassword`; neither credential is copied into the
Nix store.

Kavita's token key is encrypted with agenix and decrypted at runtime to
`/run/agenix/kavitaTokenKey`. It must remain stable across restarts because
Kavita uses it for token signing.

The secrets directory also contains `radarr.env.age` and `prowlarr.env.age`
files reserved for future service environment secrets. Create them when needed.

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

1. Radarr — Settings → Media Management → add root folder
   `/srv/media/library/movies`
2. Sonarr — Settings → Media Management → add root folder
   `/srv/media/library/shows`
3. Jellyfin — first-run wizard: create an administrator, then add
   `/srv/media/library/movies` as Movies and `/srv/media/library/shows` as TV
   Shows
4. Seerr — first-run wizard: connect Jellyfin URL, connect Radarr/Sonarr (API keys from `sudo nixarr list-api-keys`)
5. Kavita — first-run wizard: create admin user, add
   `/srv/media/library/books` as Books and `/srv/media/library/manga` as Manga
6. Audiobookshelf — create the administrator, then create an Audiobooks library
   whose folder is `/srv/media/library/audiobooks`
7. Shelfmark — create the administrator or enable authentication, then:
   - keep Universal search enabled and use Open Library for metadata discovery
   - keep `/srv/media/downloads/audiobooks` as the ingest/download directory
   - configure file processing to prefer M4B for audiobook results
   - move reviewed completed downloads into `/srv/media/library/audiobooks`
     for Audiobookshelf to scan
   - configure only download sources whose terms and rights metadata permit
     the intended use; do not add private trackers or Usenet providers
   - for Internet Archive or LibriVox material, verify rights in the browser
     and import the downloaded files manually because Shelfmark does not
     document a direct Internet Archive/LibriVox source
8. Prowlarr indexers — add them via the web UI, or declare them under
   `nixarr.prowlarr.settings-sync.indexers` in `media.nix`

Internet Archive and LibriVox labels are useful signals, not a universal legal
determination. Check each item's rights metadata and whether its public-domain
or open-license status applies in the jurisdiction where it will be used.

Shelfmark has no direct Seerr integration. Book and audiobook requests happen
through Shelfmark's own UI.

The following is handled automatically by nixarr on deploy:

- qBittorrent added as Radarr's download client
- qBittorrent added as Sonarr's download client
- Radarr and Sonarr synced to Prowlarr as applications
- State directories under `/srv/.state/nixarr/`
