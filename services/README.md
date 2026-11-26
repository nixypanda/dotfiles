# Rivendell Homelab

This host keeps the service shape declarative and leaves app-owned setup state
inside each application's state directory.

## Paths

The canonical media and state paths are defined in `paths.nix`. Their resolved
values are also written to `/etc/homelab/media-paths` and
`/etc/homelab/bookorbit-paths` on the host.

Run `media-unlinked` on Rivendell to list files larger than 50 MiB under
`/srv/media` whose hardlink count is one. It accepts optional root and minimum
size arguments, for example `media-unlinked /srv/media/library 100M`.

## Services

`ports.nix` defines service ports and the tailnet host. Homepage publishes the
service links; `reverse-proxy.nix` configures Caddy's tailnet HTTPS access.
Selected endpoints at `https://srt-n01-rivendell.taila65e7f.ts.net:<port>`:

| Service | HTTPS port | Application listener |
| --- | --- | --- |
| Hedger | 9450 | `127.0.0.1:8083` (nginx) |
| Lidarr | 9471 | `127.0.0.1:8686` |
| Navidrome | 9472 | `127.0.0.1:4533` |
| Stremio Local Debrid | 9474 | `127.0.0.1:7810` |
| SoulSync | 9476 | `127.0.0.1:8008` |
| Home Assistant | 9477 | LAN port 8123 |
| BookOrbit | 9478 | `127.0.0.1:3000` |

The localhost listeners are not opened in the firewall. Home Assistant also
allows direct LAN access for onboarding and device control.

## SoulSync

SoulSync runs as `soulsync` in the shared `media` group. Application state lives
in `/srv/.state/soulsync`, raw downloads in `/srv/media/downloads/soulsync`, and
organized music in `/srv/media/library/music-soulsync`. Its sandbox permits
persistent writes only to these directories and keeps Lidarr's
`/srv/media/library/music` read-only.

Navidrome has read-only access to the SoulSync library; register it as described
in [First Setup](#first-setup).

When connecting Navidrome in SoulSync, select `All Libraries` instead of the
default first-library behavior. Selecting only the first library can make the
existing Lidarr tracks appear missing even though Navidrome serves them. In the
connection's `Additional Music Libraries` setting, add
`/srv/media/library/music` while keeping
`/srv/media/library/music-soulsync`; the extra path lets SoulSync resolve the
existing files locally. SoulSync's systemd sandbox keeps the Lidarr tree
read-only, so adding it does not grant write access to that library.
Duplicate-file counts are normal when a track exists in more than one library;
use unique-track totals when comparing libraries.

Hiding podcasts, audiobooks, or video in the setup wizard is a UI preference;
it does not disable their backends.

Nix seeds paths into the initial `config.json`. SoulSync 3.6.4 uses its database
as the primary configuration store after first boot, so UI changes persist and
are not reset on restart.

yt-dlp is provided and updated through the Nix package. SoulSync's in-app
runtime updater is not compatible with this deployment and should not be used.

Configure connections and credentials in SoulSync → Settings → Connections.
Secrets live in its encrypted state database, outside the Nix store. Use
`http://127.0.0.1:4533` for the Navidrome connection.

## Home Assistant

The native NixOS service is reachable on LAN port 8123 and at
<https://srt-n01-rivendell.taila65e7f.ts.net:9477> over the tailnet. On first
setup, create the owner account and pair devices under Settings → Devices & services.

`home-assistant.nix` manages the base configuration and packages Philips CoAP,
LocalTuya, and SmartIR. HTTP settings are maintained in Settings → System →
Network, not YAML. Accounts, device registries, dashboards, and UI-managed
automations, scripts, and scenes persist in `/srv/.state/home-assistant`;
the writable YAML includes are initialized only when absent. Back up this
directory to preserve onboarding and device configuration.

## Stremio Local Debrid

`stremio-local-debrid` runs a Stremio addon that downloads Torrentio torrents on
Rivendell. Downloads continue to completion after the player closes, and later
plays reuse the on-disk cache. Clients play the cached HTTP stream over the
tailnet.

- Addon manifest: `https://srt-n01-rivendell.taila65e7f.ts.net:9474/<token>/manifest.json`
- Status dashboard: `https://srt-n01-rivendell.taila65e7f.ts.net:9474/<token>/`
- The token and both URLs are generated on first start. Read them with
  `sudo cat /var/lib/stremio-local-debrid/addon-url.txt`.
- Homepage links to the status dashboard through a runtime-generated
  environment file, so the token never enters the Nix store or the repo.
- Cache: `/srv/media/cache/stremio-debrid` (100 GiB budget, 10 GiB free-space
  reserve). The upstream module keeps the cache private to the `stremio` user
  (directory mode `0700`); existing downloads stay in place.
- Torrentio defaults to `https://torrentio.strem.fun/manifest.json`. Replace
  `settings.sources` in `services/stremio-local-debrid.nix` for public provider or
  filter settings. For credential-bearing URLs, use the upstream module's
  `configFile` option with a private runtime JSON file, never a Nix store path.
- Install the addon once in Stremio; the account syncs it to the other devices.
  Pick a "Local Cache" source to download and stream from Rivendell.
- The package and hardened NixOS service come from the `stremio-local-debrid`
  flake input, tracking `main` with a lockfile pin. Local configuration adds the
  Homepage environment-file adapter and Rivendell's media-mount safeguards.
- Torrent transfers use TCP; native uTP and WebRTC are intentionally unavailable.
- Migration reuses `/var/lib/stremio-local-debrid/token`, so addon URLs do not
  change and clients do not need to reinstall the addon. Back up this token and
  the state directory before deployment; the previous NixOS generation remains
  available for rollback.

## BookOrbit

Native NixOS service (no containers). Package is built from pinned upstream
source `v3.8.0`; state is in `/srv/.state/bookorbit`, PostgreSQL uses the native
`postgresql_16` service with pgvector. The app listens on `127.0.0.1:3000` and
is exposed at `https://srt-n01-rivendell.taila65e7f.ts.net:9478`.

Books and audiobooks are writable at `/books/books` and `/books/audiobooks`
so downloaded requests can be filed, including audio added to existing ebooks.
Manga remains read-only at `/books/manga`.
The Library Dock is `/srv/media/.bookorbit` at `/books/dock`.
BookOrbit manages creation of the shared books, manga, and audiobook directories.
The audiobook directory is owned by `bookorbit:media`; books and manga remain
owned by `nixypanda:media`. The audiobook download directory at
`/srv/media/downloads/audiobooks` is retained with `nixypanda:media` ownership.
These directory rules do not recursively change existing file ownership.

Separate bind mounts currently prevent hardlinks from downloads into the Dock,
so imports fall back to copying and consume additional disk space until the
common-mount hardlink migration is implemented.

Secrets (`jwt_secret`, `setup_bootstrap_token`) are generated at runtime under
`/var/lib/bookorbit/secrets` and are not in the Nix store. PostgreSQL uses a
local peer-authenticated `bookorbit` role/database with pgvector.

First setup: apply with `just apply-system rivendell`, wait for
`bookorbit.service`, then open the tailnet URL and create the admin account using
`sudo cat /var/lib/bookorbit/secrets/setup_bootstrap_token`. A previous
activation attempt stopped at PostgreSQL setup due to the template1 collation
mismatch; repair/back up first.

Migration/rollback: change the pinned source version/hash and service config,
then apply. Database migrations run through upstream `entrypoint.sh` before the
server starts. Downgrades require a pre-upgrade database backup and restore.

Existing PostgreSQL `template1` has a glibc collation version mismatch (2.42 vs
2.44). BookOrbit does not alter the cluster; its database is provisioned by
NixOS from `template1` and will fail until `template1` is repaired. Operator repair only, after a backup:

```sh
sudo -u postgres sh -c 'umask 077; pg_dumpall -f /var/lib/postgresql/pre-collation-repair.sql'
sudo -u postgres psql -d template1 -c 'REINDEX DATABASE template1;'
sudo -u postgres psql -d postgres -c 'ALTER DATABASE template1 REFRESH COLLATION VERSION;'
```

Verify the backup is nonempty and mode `0600` before repairing, and store or
copy it off-host securely. `REINDEX` cannot run in a transaction. Only refresh
other databases such as `postgres` or `calco` after reviewing their indexes and
data; do not run a blanket refresh. No repair command is executed automatically.

## Hedger

Hedger uses a single server on port 9450. It serves all three journals under
`/srv/hledger/journals` as separate ledgers (Mine, Wife, Combined) selectable
in the app. Caddy terminates Tailscale TLS on port 9450 and proxies to Hedger's
module-managed nginx listener on `127.0.0.1:8083`, which serves the shared
frontend and proxies `/api/*` to the single localhost-only Hedger backend on
port 5001.

Caddy requires HTTP Basic Auth for both Hedger (9450) and Paisa (9460), including
their API routes. Both use the `finance` username and the same randomly generated
password. Retrieve it on Rivendell with `sudo cat /var/lib/finance-auth/password`.
The password is created on first deployment and retained across rebuilds; only
its hash is readable by Caddy. The login is in addition to tailnet access. Back
up `/var/lib/finance-auth/password` if you need to preserve the credential when
reinstalling the server. To rotate it, replace that file with a new random
password and restart `finance-auth.service` followed by `caddy.service`.
Hedger's service worker would otherwise intercept the browser's login challenge,
so Caddy serves a non-intercepting worker for Hedger. Existing visitors may need
to clear Hedger's site data once (including its service worker) and reload to
get the browser login prompt. This disables Hedger's offline app-shell cache.

Journal paths are supplied by the NixOS module. Per-ledger display and
investment settings live in version-1 settings files under
`/srv/hledger/hedger/` (`hedger-mine.yaml`, `hedger-wife.yaml`,
`hedger-combined.yaml`), which are synced from the tracked ledger source.
Hedger loads each journal into an immutable snapshot at startup. Home Manager
installs `sync-hledger` as a normal command available anywhere on the Mac; it
reloads `hedger-mine` after synchronizing journals or prices.

## Declarative State

Nix declares the services through [nixarr](https://nixarr.com), native NixOS
modules, and separate application modules. Deployment handles:

- Service/firewall ports, Caddy access, and Homepage links.
- Media users/groups and state directories.
- qBittorrent paths, Web UI username, and PBKDF2 password hash.
- qBittorrent download-client registration in Radarr and Sonarr.
- Radarr, Sonarr, and Lidarr registration in Prowlarr, plus Bazarr connections
  to Radarr and Sonarr.
- Radarr/Sonarr quality profiles, custom formats, quality sizes, and Jellyfin
  naming through daily Recyclarr sync.
- BookOrbit service, runtime secrets, and shared library directories.
- Pi-hole upstreams and local DNS records.

Application accounts, pairing, and first-run database state require the manual
steps below. Back up state after setup and before upgrades. Media-stack state
is under `/srv/.state/nixarr/`; BookOrbit uses `/srv/.state/bookorbit` plus its
PostgreSQL database and runtime secrets. Other application state
paths are listed in `paths.nix`, with Hedger paths documented above. State
backups do not include media libraries such as `/srv/media/library/audiobooks`;
back up media separately.

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

The PBKDF2-SHA512 hash in `services/media/default.nix` is what qBittorrent stores — this is a
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
3. Lidarr — open `https://srt-n01-rivendell.taila65e7f.ts.net:9471`, then:
   - add `/srv/media/library/music` as the root folder
   - add qBittorrent as the download client at `localhost:8085`, using username
     `qbittorrent_admin` and the password from
     `sudo cat /run/agenix/qbittorrentPassword`; use the `lidarr` category
   - choose the desired metadata profile and quality profile before adding artists
4. Navidrome — open `https://srt-n01-rivendell.taila65e7f.ts.net:9472` and create
   the initial administrator account. In Settings → Libraries, add a library
   named `SoulSync` at `/srv/media/library/music-soulsync` and grant it to
   non-admin users alongside the existing Lidarr library. Then use the same URL
   and credentials to add a Subsonic account in Amperfy.
5. Jellyfin — first-run wizard: create an administrator, then add
   `/srv/media/library/movies` as Movies and `/srv/media/library/shows` as TV
   Shows
6. Seerr — first-run wizard: connect Jellyfin URL, connect Radarr/Sonarr (API keys from `sudo nixarr list-api-keys`)
7. BookOrbit — follow the bootstrap instructions above and add libraries at
   `/books/books`, `/books/manga`, and `/books/audiobooks`.
8. Prowlarr indexers — add them via the web UI, or declare them under
   `nixarr.prowlarr.settings-sync.indexers` in `services/media/default.nix`
9. Bazarr — open `https://srt-n01-rivendell.taila65e7f.ts.net:9470`, then:
   - add the desired subtitle languages and create default language profiles
     for movies and series
   - enable subtitle providers such as OpenSubtitles.com
   - keep `Use Embedded Subtitles` enabled and enable
     `Ignore Embedded PGS Subtitles`, so bitmap PGS tracks do not satisfy the
     profile
   - leave `Use Original Format` disabled in the language profiles to save
     downloaded text subtitles as SRT
10. After Recyclarr's first sync, select `[SQP] SQP-1 WEB (2160p)` as Seerr's
    Radarr quality profile and `WEB-2160p + 1080p fallback` as its Sonarr quality
    profile. Bulk-edit existing movies and series in Radarr and Sonarr to use the
    corresponding profiles; Recyclarr manages profiles but does not assign them
    to library items.
