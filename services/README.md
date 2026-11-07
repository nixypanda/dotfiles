# Rivendell Homelab

This host keeps the service shape declarative and leaves app-owned setup state
inside each application's state directory.

## Paths

The canonical media and state paths are defined in `paths.nix`. Their resolved
values are also written to `/etc/homelab/media-paths`, `/etc/homelab/ebook-paths`
and `/etc/homelab/audiobook-paths` on the host.

Run `media-unlinked` on Rivendell to list files larger than 50 MiB under
`/srv/media` whose hardlink count is one. It accepts optional root and minimum
size arguments, for example `media-unlinked /srv/media/library 100M`.

## Services

Service ports, tailnet-facing aliases, and the tailnet host are defined in
`ports.nix`. The Homepage dashboard uses that file to publish service links.

Ports 9450-9452 serve Hedger over the tailnet:

- 9450: Hedger Mine
- 9451: Hedger Wife
- 9452: Hedger Combined

The audiobook services are available only through Caddy on the tailnet:

- Audiobookshelf: `https://srt-n01-rivendell.taila65e7f.ts.net:9466`
- Shelfmark: `https://srt-n01-rivendell.taila65e7f.ts.net:9467`

Their application ports (`127.0.0.1:8000` and `127.0.0.1:8084`) stay bound to
localhost and are not opened in the firewall.

The music services are also available only through Caddy on the tailnet:

- Lidarr: `https://srt-n01-rivendell.taila65e7f.ts.net:9471`
- Navidrome: `https://srt-n01-rivendell.taila65e7f.ts.net:9472`

Their application ports (`127.0.0.1:8686` and `127.0.0.1:4533`) stay bound to
localhost and are not opened in the firewall. Amperfy connects to the Navidrome
HTTPS URL.

MeTube is available only through Caddy on the tailnet:

- MeTube: `https://srt-n01-rivendell.taila65e7f.ts.net:9475`

Its application port (`127.0.0.1:8086`) stays bound to localhost. MeTube writes
video downloads to `/srv/media/downloads/metube` and audio downloads to
`/srv/media/downloads/metube/audio`; its queue and history live in
`/srv/.state/metube`. Audio downloads are imported automatically by Beets; see
[Beets / MeTube import](#beets--metube-import).

## Beets / MeTube import

Beets imports finished audio downloads from MeTube into a separate music
library that Navidrome also serves. Lidarr keeps owning
`/srv/media/library/music`; Beets only writes
`/srv/media/library/music-beets`, and its Lidarr tree is mounted read-only into
the import service as a defensive measure.

- MeTube audio inbox: `/srv/media/downloads/metube/audio`
- Review queue for skipped duplicates:
  `/srv/media/downloads/metube/review/leftover`
- Failed imports, including corrupt or unconvertible files:
  `/srv/media/downloads/metube/review/failed`
- Orphaned artwork preserved from the inbox:
  `/srv/media/downloads/metube/review/orphan-art/<timestamp>`
- Beets library: `/srv/media/library/music-beets`
- Beets state, database, import logs, and run summaries: `/srv/.state/beets`

The `beets-import` service runs from a two-minute timer and is also triggered
by changes to the audio inbox. It walks the inbox recursively and imports each
audio file as a singleton once that file has been stable for at least two
minutes. Per-file imports mean a playlist can be imported while later tracks
are still downloading: there is no partial-album import, and playlist
directories are never imported or deleted as a unit.

The `youtube_clean` Beets plugin removes YouTube noise before tagging:
`- Topic` suffixes, trailing `(Official Video)`, `[Official Music Video]`,
`(Official Audio)`, `(Lyric Video)`, `(Visualizer)`, `(4K Remaster)`,
`(Remastered)`, `(HD)`, and `(HQ)` labels, and trailing 11-character video ids
on titles. It also removes a duplicated `<artist> -` prefix from the title.
The `fromfilename` plugin is not used. When tags are missing, the plugin
derives metadata from the filename instead: it strips the extension, an
`NN -`/`NN.` track prefix, a trailing YouTube id, and YouTube noise. A cleaned
`Artist - Title` basename supplies a missing artist without treating a numeric
track prefix as the artist.

When the album tag is empty and the file's parent directory is named like
`Playlist Name [PL...]` or `[OLAK5uy_...]`, the directory name without the
bracketed id becomes the album and the albumartist becomes `Various Artists`.
That keeps mixed-artist playlists in one `Various Artists/<Playlist>/`
directory instead of splitting them by track artist. For real album tags,
albumartist falls back to the track artist when empty, or to `Unknown Artist`
when both are missing. An `NN -` or `NN.` filename prefix supplies a missing
track number.

WebM and WAV downloads are converted before import: WebM becomes
`<name>.converted.opus` with libopus at 160k, and WAV becomes
`<name>.converted.flac`. The original is removed only after a successful
conversion. Beets imports the converted path, and a failed import keeps the
converted file in `review/failed`.

Singletons with album tags are stored as `albumartist/album/track title`;
files without album tags go under `Singles/artist/title`. Downloads that share
an album therefore land in the same album directory instead of receiving a
separate disambiguated folder per file. Strong MusicBrainz matches get
canonical tags; weak or missing matches are imported as-is instead of being
discarded.

When Beets exits successfully but leaves a file in the inbox, the file is
checked with `ffprobe`. A valid file is treated as a duplicate/skip (or a
leftover) and moved to `review/leftover`. Missing or corrupt audio, and any
nonzero Beets exit, count as failures and move the file to `review/failed`;
the run then exits nonzero. `review/failed` is created on every run.

MeTube's JPG/PNG sidecars are left alongside their audio. Once the audio has
left the inbox, an otherwise orphaned image is moved to
`review/orphan-art/<timestamp>/` instead of being deleted, so artwork is
preserved even if a file lacked embedded cover art. An image is only moved
when no non-image file with the same stem remains in the same directory, so
partial downloads keep their artwork. Only empty directories that have stayed
empty for 24 hours are removed; directories MeTube may still be using are left
alone.

Lidarr and Beets remain separate libraries. If the same release exists in both,
Navidrome shows two albums; Beets duplicate detection only covers its own
library.

Each run with stable audio writes one `import-*.log`. Logs older than 14 days
are deleted, and `runs.jsonl` is trimmed to the newest 2000 lines (or 1000
lines when it grows beyond 1 MB). Runs with no stable audio create neither a
run log nor a `runs.jsonl` entry.

The timer is authoritative: systemd path units do not watch subdirectories, so
the path unit is only a low-latency hint for new top-level entries in the inbox.

To inspect or run it manually:

```sh
systemctl status beets-import.timer
sudo systemctl start beets-import.service
journalctl -u beets-import.service
tail -n 20 /srv/.state/beets/runs.jsonl
find /srv/media/downloads/metube/review/leftover -type f
find /srv/media/downloads/metube/review/failed -type f
find /srv/media/downloads/metube/review/orphan-art -type f
```

Navidrome library registration is database/UI state, so it is a one-time manual
step: open Navidrome, go to Settings → Libraries, add a library named `Beets`
with path `/srv/media/library/music-beets`, and grant it to non-admin users
alongside the existing Lidarr library.

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
  reserve).
- Torrentio defaults to `https://torrentio.strem.fun/manifest.json`. Replace
  `torrentioManifest` in `services/stremio-local-debrid.nix` with your configured
  Torrentio URL if you use custom providers, filters, or debrid.
- Install the addon once in Stremio; the account syncs it to the other devices.
  Pick a "Local Cache" source to download and stream from Rivendell.
- The package stubs WebRTC, so browser-only WebRTC peers are not used; ordinary
  TCP/uTP BitTorrent peers are unaffected.

All three Hedger instances use the existing journals under
`/srv/hledger/journals`. Caddy terminates Tailscale TLS on ports 9450-9452 and
proxies to Hedger's module-managed nginx listener on `127.0.0.1:8083`. nginx
selects the matching ledger by its internal hostname, serves the shared
frontend, and proxies `/api/*` to the matching localhost-only Hedger backend on
ports 5001-5003.

The Mine instance also reads
`/srv/hledger/hedger/hedger-mine.yaml`. Journal paths, backend ports, display
commodity, and fiscal-year settings are supplied as command-line arguments by
the NixOS module and override matching YAML values. The other instances do not
use a Hedger YAML configuration.

Hedger loads each journal into an immutable snapshot at startup. The
`sync-hledger` command reloads all three `hedger-*` services after synchronizing
journals or prices. It copies the tracked ledger files, including
`hedger/hedger-mine.yaml`.

## Declarative State

Nix currently declares:

- all services via the [nixarr](https://nixarr.com) module
- Homepage dashboard over Tailscale MagicDNS
- firewall ports
- service ports
- qBittorrent download paths, Web UI username, and Web UI password (PBKDF2
  hash, not plaintext)
- Kavita service settings, token key secret, and library directories
- Audiobookshelf through the nixarr module
- Shelfmark through the nixarr module and locked Nixpkgs package
- Lidarr through the nixarr module and its registration in Prowlarr
- MeTube through a native systemd service
- Beets import of MeTube audio through a native systemd service and timer
- Navidrome through the native NixOS module
- Prowlarr app sync (settings-sync)
- Radarr and Sonarr download client (qBittorrent) via settings-sync
- Bazarr connections to Radarr and Sonarr via settings-sync
- Radarr and Sonarr quality profiles, custom formats, quality sizes, and
  Jellyfin naming via a daily Recyclarr sync
- Pi-hole upstreams and local DNS records

Nixarr also handles:

- media user/group creation
- state management under `/srv/.state/nixarr/`

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
4. MeTube — open `https://srt-n01-rivendell.taila65e7f.ts.net:9475` and queue a
   download. No first-run account or database setup is required. Use the audio
   mode for music; Beets imports the finished audio automatically.
5. Beets / MeTube import — no first-run database setup is required. Confirm the
   timer is active with `systemctl status beets-import.timer`; inspect
   `/srv/media/downloads/metube/review/leftover`,
   `/srv/media/downloads/metube/review/failed`, and
   `/srv/media/downloads/metube/review/orphan-art` after the first imports.
6. Navidrome — open `https://srt-n01-rivendell.taila65e7f.ts.net:9472` and create
   the initial administrator account. In Settings → Libraries, add a library
   named `Beets` at `/srv/media/library/music-beets` and grant it to non-admin
   users alongside the existing Lidarr library. Then use the same URL and
   credentials to add a Subsonic account in Amperfy.
7. Jellyfin — first-run wizard: create an administrator, then add
   `/srv/media/library/movies` as Movies and `/srv/media/library/shows` as TV
   Shows
8. Seerr — first-run wizard: connect Jellyfin URL, connect Radarr/Sonarr (API keys from `sudo nixarr list-api-keys`)
9. Kavita — first-run wizard: create admin user, add
   `/srv/media/library/books` as Books and `/srv/media/library/manga` as Manga
10. Audiobookshelf — create the administrator, then create an Audiobooks library
    whose folder is `/srv/media/library/audiobooks`
11. Shelfmark — create the administrator or enable authentication, then:
    - keep Universal search enabled and use Open Library for metadata discovery
    - keep `/srv/media/library/books` as the ebook destination so Kavita scans
      completed downloads directly
    - keep `/srv/media/downloads/audiobooks` as the audiobook download directory
    - restrict supported book formats to `epub,pdf,cbz,cbr`, which Kavita can read
    - configure file processing to prefer M4B for audiobook results
    - move reviewed completed audiobook downloads into
      `/srv/media/library/audiobooks` for Audiobookshelf to scan
    - configure only download sources whose terms and rights metadata permit
      the intended use; do not add private trackers or Usenet providers
    - for Internet Archive or LibriVox material, verify rights in the browser
      and import the downloaded files manually because Shelfmark does not
      document a direct Internet Archive/LibriVox source
12. Prowlarr indexers — add them via the web UI, or declare them under
    `nixarr.prowlarr.settings-sync.indexers` in `services/media/default.nix`
13. Bazarr — open `https://srt-n01-rivendell.taila65e7f.ts.net:9470`, then:
    - add the desired subtitle languages and create default language profiles
      for movies and series
    - enable subtitle providers such as OpenSubtitles.com
    - keep `Use Embedded Subtitles` enabled and enable
      `Ignore Embedded PGS Subtitles`, so bitmap PGS tracks do not satisfy the
      profile
    - leave `Use Original Format` disabled in the language profiles to save
      downloaded text subtitles as SRT
14. After Recyclarr's first sync, select `[SQP] SQP-1 WEB (2160p)` as Seerr's
   Radarr quality profile and `WEB-2160p + 1080p fallback` as its Sonarr quality
   profile. Bulk-edit existing movies and series in Radarr and Sonarr to use the
   corresponding profiles; Recyclarr manages profiles but does not assign them
   to library items.

Internet Archive and LibriVox labels are useful signals, not a universal legal
determination. Check each item's rights metadata and whether its public-domain
or open-license status applies in the jurisdiction where it will be used.

Shelfmark has no direct Seerr integration. Book and audiobook requests happen
through Shelfmark's own UI. Shelfmark also has no direct Kavita integration:
ebooks are written into Kavita's books library and picked up by Kavita's normal
library scan.

The following is handled automatically by nixarr on deploy:

- qBittorrent added as Radarr's download client
- qBittorrent added as Sonarr's download client
- Radarr and Sonarr synced to Prowlarr as applications
- Bazarr connected to Radarr and Sonarr
- Radarr and Sonarr streaming-optimised 2160p-with-1080p-fallback policies
  synced daily by Recyclarr
- State directories under `/srv/.state/nixarr/`
