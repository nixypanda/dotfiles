# Rivendell Observability Plan

## Objective

Add a native, declarative Grafana LGTM observability stack to
`srt-n01-rivendell`:

- Grafana for visualization
- Loki for logs
- Tempo for traces
- Prometheus for metrics
- OpenTelemetry Collector Contrib as the localhost telemetry gateway and host
  collector

Use the first-class NixOS modules in the pinned Nixpkgs. Do not use containers
or imperative installers.

## Safety and Scope

- Do not run `nixos-rebuild switch` without explicit deployment approval.
- Preserve unrelated worktree changes, including the pre-existing `flake.lock`
  modification and untracked top-level `.secrets/` directory.
- Do not add public firewall openings.
- Bind every backend and ingestion endpoint to localhost.
- Expose only Grafana through the existing Caddy and Tailscale HTTPS pattern.
- Keep the configuration application-agnostic.
- Do not ingest an existing service's telemetry until the source has been
  explicitly approved.
- Never place plaintext secrets in Git or the Nix store.
- Do not collect authentication headers, cookies, request bodies, secrets,
  tokens, sensitive command arguments, or unnecessarily high-cardinality
  personal labels.
- If journald collection is approved later, use an explicit unit allowlist and
  exclude the observability stack and its exporters to prevent recursive
  ingestion.

## Repository Conventions

- Host configuration:
  `hosts/srt-n01-rivendell/configuration.nix`
- Homelab imports:
  `hosts/srt-n01-rivendell/homelab/default.nix`
- Central port registry:
  `hosts/srt-n01-rivendell/homelab/ports.nix`
- Tailnet reverse proxy:
  `hosts/srt-n01-rivendell/homelab/reverse-proxy.nix`
- Runtime secrets: agenix, decrypted below `/run/agenix`
- Native service state: systemd-managed directories below `/var/lib`
- Large media content: `/srv/media`; this disk is not appropriate for
  latency-sensitive telemetry databases

## Capacity Discovery

Live inspection on 2026-07-30 found:

- Intel Core i5-8500T, 6 physical cores
- 15 GiB RAM, approximately 12 GiB available
- No swap
- NVMe root filesystem: 937 GiB total, 848 GiB free
- Media filesystem: 3.6 TiB total, 3.2 TiB free
- Very low steady CPU load
- Approximately 490 MiB of retained systemd journal data
- Approximately 15,000-18,000 journal entries per day

Pinned package versions at discovery time:

- Grafana 13.0.3
- Loki 3.7.3
- Tempo 3.0.2
- Prometheus 3.12.0
- OpenTelemetry Collector Contrib 0.155.0

## Architecture

```text
approved metric sources
  |-- hostmetrics receiver
  |-- selected systemd exporter
  `-- LGTM/Collector self-metrics
                 |
                 v
             Prometheus <------ Grafana

localhost OTLP gRPC/HTTP
                 |
                 v
      OpenTelemetry Collector
        | metrics -> Prometheus exporter endpoint
        | traces  -> Tempo OTLP
        ` logs    -> Loki OTLP

tailnet browser -> Caddy/Tailscale TLS -> localhost Grafana
```

The OTLP endpoints are available for future approved integrations. Merely
providing an endpoint does not configure any existing application to send data.

## Port Plan

All ports must be registered in `ports.nix` and covered by a duplicate-port
assertion.

| Component | Bind address | Port | Exposure |
| --- | --- | ---: | --- |
| Grafana | `127.0.0.1` | 3000 | Caddy only |
| Loki HTTP | `127.0.0.1` | 3100 | Local |
| Loki gRPC | `127.0.0.1` | 9096 | Local |
| Tempo HTTP | `127.0.0.1` | 3200 | Local |
| Tempo gRPC | `127.0.0.1` | 9095 | Local |
| Tempo OTLP gRPC | `127.0.0.1` | 14317 | Collector only |
| Tempo OTLP HTTP | `127.0.0.1` | 14318 | Collector only |
| Prometheus | `127.0.0.1` | 9090 | Local |
| OTLP gRPC | `127.0.0.1` | 4317 | Local |
| OTLP HTTP | `127.0.0.1` | 4318 | Local |
| Collector metrics export | `127.0.0.1` | 8889 | Local |
| Collector self-metrics | `127.0.0.1` | 8888 | Local |
| systemd exporter | `127.0.0.1` | 9558 | Local |
| Grafana tailnet HTTPS | Caddy listener | 9468 | Tailnet firewall only |

Do not enable `openFirewall` on Grafana, Prometheus, or any exporter.

## Core Storage and Retention

Use native module and systemd state-directory conventions:

- Grafana: `/var/lib/grafana`
- Loki: `/var/lib/loki`
- Tempo: `/var/lib/tempo`
- Prometheus: `/var/lib/prometheus2`
- Collector queue/state: `/var/lib/opentelemetry-collector`

Initial small-host retention targets:

- Prometheus: 30 days, capped at 10 GiB
- Loki: 7 days with compaction and retention enabled
- Tempo: 72 hours

The short log and trace windows limit privacy exposure and background
compaction. Metrics receive the longest retention because they have the lowest
volume and operational sensitivity.

## Resource Guardrails

Use systemd limits as guardrails, not expected steady-state allocations:

| Service | Memory high | Memory maximum | CPU quota |
| --- | ---: | ---: | ---: |
| Grafana | 384 MiB | 768 MiB | 75% |
| Loki | 768 MiB | 1536 MiB | 100% |
| Tempo | 512 MiB | 1024 MiB | 100% |
| Prometheus | 768 MiB | 1536 MiB | 100% |
| Collector | 384 MiB | 768 MiB | 75% |

Revisit these after observing compaction and query behavior. Rivendell has no
swap, so limits should prevent a telemetry burst from starving primary
services.

## Approved Integrations

The user approved only group 1 on 2026-07-30:

1. Host metrics through the Collector `hostmetrics` receiver.
2. Selected systemd health metrics through the native systemd Prometheus
   exporter.
3. Grafana, Loki, Tempo, Prometheus, and Collector self-metrics.

No existing-service logs are approved. Caddy, PostgreSQL, Tailscale, Homepage,
media, DNS, finance, and application telemetry remain disabled as sources.

The selected systemd exporter should expose aggregate unit state while using a
unit allowlist for core host and observability units. It must not ingest journal
messages.

## Deferred Integration Catalog

These sources were discovered but are not approved:

| Source | Available signals | Method | Value and cost | Privacy/security | Required changes | Code changes |
| --- | --- | --- | --- | --- | --- | --- |
| Caddy | Metrics, operational logs | Existing `127.0.0.1:2019/metrics`, journald | High value, low volume | Handler/server labels; access logs would be more sensitive | Prometheus scrape or journal allowlist | No |
| PostgreSQL | Metrics, logs | Restricted `pg_monitor` exporter, journald | High value, low-medium volume | Current application database is CalCo; DB names/activity and errors are sensitive | Monitoring role and exporter | No |
| Tailscale | Metrics, logs | Local debug metrics or OAuth exporter, journald | Medium value, low volume | Peer identities, IPs, topology; OAuth secret | Filtered collector/exporter | No |
| Homepage | Logs, synthetic health | Journald, blackbox probing | Medium value, low-medium volume | Reveals service names and URLs | Journal allowlist or probe config | No |
| Jellyfin | Logs; metrics with plugin/exporter | Journald, external integration | Medium value, medium volume | Titles, users, playback activity | Plugin/exporter or logs | No for logs |
| Seerr | Logs | Journald | Medium value, high log volume | Media requests, users, titles | Journal allowlist | No |
| Radarr | Metrics, logs | Exportarr, journald | High value, low-medium volume | Movie titles, queue/download activity | Runtime API-key credential and exporter | No |
| Sonarr | Metrics, logs | Exportarr, journald | High value, low-medium volume | Series/episode titles and downloads | Runtime API-key credential and exporter | No |
| Prowlarr | Metrics, logs | Exportarr, journald | Medium value, medium volume | Indexer names and search activity | Runtime API-key credential and exporter | No |
| qBittorrent/qui | Metrics, logs | qBittorrent exporter, journald | Medium value, medium volume | Torrent names, trackers, peers, transfer history | Existing runtime secret and exporter | No |
| Kavita | Logs | Journald | Medium value, medium volume | Titles, users, reading activity | Journal allowlist | No |
| Audiobookshelf | Logs | Journald | Medium value, low volume | Titles, users, listening history | Journal allowlist | No |
| Shelfmark | Logs | Journald | Medium value, low volume | Search terms, sources, downloads | Journal allowlist | No |
| Pi-hole aggregate | Metrics | Secret-safe exporter | High value, low volume | Aggregate activity; exporter authentication secret | Credential-safe wrapper/exporter | No |
| Pi-hole queries | Logs | FTL database/log files | High volume | Browsing history and client addresses | Purpose-built receiver and strict filters | No |
| CalCo | Logs; future metrics/traces | Rust tracing/journald; future OTel | Medium value, low volume | Health, nutrition, authentication, and DB context | Filters; OTel instrumentation for richer signals | Yes for metrics/traces |
| Hedger | Logs; future metrics/traces | Journald; future OTel | Medium value, low volume | Financial paths, commodities, ledger activity | Filters; application instrumentation | Yes for metrics/traces |
| Paisa | Logs | Journald | Medium value, low volume | Accounts, commodities, transaction context | Journal allowlist | No |
| Authentication | Logs | SSH/logind journald units | Medium value, medium volume | Usernames, IPs, session history | Separate explicit approval and filters | No |

## Grafana Security

- Disable anonymous access.
- Disable analytics and update checks where supported.
- Keep user self-signup disabled.
- Provision Prometheus, Loki, and Tempo data sources declaratively.
- Supply Grafana's secret key and initial admin password through agenix files
  and Grafana's file provider.
- Generate secret plaintext only in a private temporary directory, encrypt it
  immediately to the host SSH recipient, and remove the temporary directory.
- Retrieve the initial admin password on the deployed host through root-only
  access; never print it during evaluation, builds, or agent output.

## Telemetry Hygiene

- Use Collector memory limiting, batching, persistent sending queues, and
  retries.
- Add resource attributes with only stable host/service identifiers.
- Delete attributes whose keys indicate authorization, cookies, passwords,
  tokens, API keys, or secrets.
- Do not add request-body capture.
- Bound attribute counts and value lengths.
- Do not enable debug exporters that print telemetry payloads.
- Keep Collector diagnostic telemetry on localhost.
- No journald receiver is enabled in the approved baseline.

## Validation

Before requesting deployment authorization:

1. Format modified Nix files with `nixfmt`.
2. Run targeted `statix` and `deadnix`.
3. Evaluate:
   `nix eval --raw .#nixosConfigurations.srt-n01-rivendell.config.system.build.toplevel.drvPath`
4. Inspect the evaluated configuration for bind addresses and firewall changes.
5. Verify every registered numeric port is unique.
6. If practical, run a build-only validation with the documented remote Linux
   builder workflow. Do not use `switch`.

After an authorized deployment:

1. Check all five services and the exporter with `systemctl`.
2. Confirm backend listeners with `ss -lntp` and ensure they are localhost-only.
3. Check Prometheus targets and query `up`.
4. Open Grafana through the Tailscale HTTPS URL and test all three provisioned
   data sources.
5. Send a synthetic, nonsensitive OTLP metric and trace through localhost and
   confirm they appear in Prometheus and Tempo.
6. Send a synthetic, nonsensitive OTLP log and confirm it appears in Loki.
7. Confirm no public firewall opening was added.

## Backup and Upgrade

- Back up `/var/lib/grafana`; it contains users, dashboards, and UI state.
- Back up the agenix source files for the Grafana secret key and admin
  password. The secret key must remain stable across upgrades.
- Prometheus, Loki, and Tempo data are rebuildable operational telemetry and
  can normally be excluded from essential backups. If retained, snapshot their
  directories while services are stopped or use each project's supported
  snapshot mechanism.
- Back up configuration in this repository independently of runtime data.
- Before a major Grafana/Loki/Tempo/Prometheus upgrade, read upstream migration
  notes and take a state snapshot.
- Validate upgrades with Nix evaluation and a build-only run before deployment.
- Avoid restoring telemetry databases across incompatible major versions
  without following upstream migration guidance.

## Remaining Checkpoints

- Additional service telemetry requires another explicit source-selection
  approval.
- Applying the NixOS configuration requires explicit deployment authorization.
