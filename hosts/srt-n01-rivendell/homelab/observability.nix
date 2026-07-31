{
  config,
  homelab,
  lib,
  pkgs,
  ...
}:

let
  inherit (homelab) tailnetHost;
  ports = homelab.services.observability;

  grafanaUrl = "https://${tailnetHost}:${toString ports.grafana.tailnet}";
  localUrl = port: "http://127.0.0.1:${toString port}";

  sensitiveAttributeKeys = [
    "api-key"
    "api_key"
    "authorization"
    "cookie"
    "db.statement"
    "http.request.body"
    "http.request.body.content"
    "http.request.header.authorization"
    "http.request.header.cookie"
    "http.response.header.set-cookie"
    "password"
    "process.command_args"
    "process.command_line"
    "secret"
    "set-cookie"
    "token"
    "url.query"
  ];

  deleteAttribute = key: {
    action = "delete";
    inherit key;
  };

  sanitizeAttributes = map deleteAttribute sensitiveAttributeKeys;

  exporterQueue = {
    enabled = true;
    num_consumers = 2;
    queue_size = 1000;
    storage = "file_storage";
  };

  exporterRetry = {
    enabled = true;
    initial_interval = "1s";
    max_elapsed_time = "5m";
    max_interval = "30s";
  };

  commonProcessors = [
    "memory_limiter"
    "attributes/sanitize"
    "resource/sanitize"
    "resource/identity"
    "batch"
  ];

  tempoSettings = {
    target = "all";
    multitenancy_enabled = false;
    stream_over_http_enabled = true;
    server = {
      grpc_listen_address = "127.0.0.1";
      grpc_listen_port = ports.tempo.grpc;
      http_listen_address = "127.0.0.1";
      http_listen_port = ports.tempo.http;
      log_level = "warn";
    };
    distributor.receivers.otlp.protocols = {
      grpc.endpoint = "127.0.0.1:${toString ports.tempo.otlpGrpc}";
      http.endpoint = "127.0.0.1:${toString ports.tempo.otlpHttp}";
    };
    backend_scheduler.provider.compaction.compaction = {
      block_retention = "72h";
      compacted_block_retention = "1h";
    };
    backend_worker.compaction = {
      block_retention = "72h";
      compacted_block_retention = "1h";
    };
    storage.trace = {
      backend = "local";
      local.path = "/var/lib/tempo/blocks";
      wal.path = "/var/lib/tempo/wal";
    };
    usage_report.reporting_enabled = false;
  };

  tempoConfig = (pkgs.formats.yaml { }).generate "tempo-rivendell.yaml" tempoSettings;
  checkedTempoConfig =
    pkgs.runCommand "tempo-rivendell-checked.yaml" { nativeBuildInputs = [ pkgs.tempo ]; }
      ''
        tempo -config.file=${tempoConfig} -config.verify=true
        ln -s ${tempoConfig} "$out"
      '';

  otelConfig = (pkgs.formats.yaml { }).generate "otelcol-rivendell.yaml" {
    extensions.file_storage = {
      create_directory = true;
      directory = "/var/lib/opentelemetry-collector/queue";
    };

    receivers = {
      otlp.protocols = {
        grpc.endpoint = "127.0.0.1:${toString ports.otel.otlpGrpc}";
        http.endpoint = "127.0.0.1:${toString ports.otel.otlpHttp}";
      };

      hostmetrics = {
        collection_interval = "30s";
        scrapers = {
          cpu = { };
          disk = { };
          filesystem = {
            exclude_fs_types = {
              fs_types = [
                "^(autofs|binfmt_misc|bpf|cgroup2?|configfs|debugfs|devpts|devtmpfs|fusectl|hugetlbfs|mqueue|overlay|proc|pstore|securityfs|sysfs|tracefs)$"
              ];
              match_type = "regexp";
            };
          };
          load = { };
          memory = { };
          network = { };
          paging = { };
          system = { };
        };
      };
    };

    processors = {
      memory_limiter = {
        check_interval = "1s";
        limit_mib = 384;
        spike_limit_mib = 128;
      };
      batch = {
        send_batch_size = 1024;
        timeout = "5s";
      };
      "attributes/sanitize".actions = sanitizeAttributes;
      "resource/sanitize".attributes = sanitizeAttributes;
      "resource/identity".attributes = [
        {
          action = "upsert";
          key = "host.name";
          value = "srt-n01-rivendell";
        }
        {
          action = "insert";
          key = "deployment.environment.name";
          value = "homelab";
        }
      ];
    };

    exporters = {
      prometheus = {
        endpoint = "127.0.0.1:${toString ports.otel.metrics}";
        enable_open_metrics = true;
        resource_to_telemetry_conversion.enabled = true;
      };
      "otlp/tempo" = {
        endpoint = "127.0.0.1:${toString ports.tempo.otlpGrpc}";
        retry_on_failure = exporterRetry;
        sending_queue = exporterQueue;
        tls.insecure = true;
      };
      "otlphttp/loki" = {
        endpoint = "${localUrl ports.loki.http}/otlp";
        retry_on_failure = exporterRetry;
        sending_queue = exporterQueue;
      };
    };

    service = {
      extensions = [ "file_storage" ];
      pipelines = {
        metrics = {
          receivers = [
            "otlp"
            "hostmetrics"
          ];
          processors = commonProcessors;
          exporters = [ "prometheus" ];
        };
        traces = {
          receivers = [ "otlp" ];
          processors = commonProcessors;
          exporters = [ "otlp/tempo" ];
        };
        logs = {
          receivers = [ "otlp" ];
          processors = commonProcessors;
          exporters = [ "otlphttp/loki" ];
        };
      };
      telemetry = {
        logs.level = "warn";
        metrics = {
          level = "basic";
          readers = [
            {
              pull.exporter.prometheus = {
                host = "127.0.0.1";
                port = ports.otel.telemetry;
              };
            }
          ];
        };
      };
    };
  };

  selectedSystemdUnits = builtins.concatStringsSep "|" [
    "NetworkManager\\.service"
    "grafana\\.service"
    "loki\\.service"
    "nix-daemon\\.service"
    "opentelemetry-collector\\.service"
    "prometheus-systemd-exporter\\.service"
    "prometheus\\.service"
    "sshd\\.service"
    "systemd-journald\\.service"
    "systemd-oomd\\.service"
    "tailscaled\\.service"
    "tempo\\.service"
  ];

  allRegisteredPorts = lib.collect builtins.isInt homelab;
in
{
  assertions = [
    {
      assertion = builtins.length allRegisteredPorts == builtins.length (lib.unique allRegisteredPorts);
      message = "Rivendell's central homelab port registry contains a duplicate port.";
    }
  ];

  services = {
    grafana = {
      enable = true;
      openFirewall = false;
      settings = {
        analytics = {
          check_for_plugin_updates = false;
          check_for_updates = false;
          reporting_enabled = false;
        };
        "auth.anonymous".enabled = false;
        security = {
          admin_password = "$__file{${config.age.secrets.grafanaAdminPassword.path}}";
          cookie_secure = true;
          disable_gravatar = true;
          secret_key = "$__file{${config.age.secrets.grafanaSecretKey.path}}";
        };
        server = {
          domain = tailnetHost;
          enforce_domain = true;
          http_addr = "127.0.0.1";
          http_port = ports.grafana.local;
          root_url = "${grafanaUrl}/";
          router_logging = false;
        };
        users.allow_sign_up = false;
      };
      provision = {
        enable = true;
        datasources.settings = {
          apiVersion = 1;
          prune = true;
          datasources = [
            {
              access = "proxy";
              editable = false;
              isDefault = true;
              name = "Prometheus";
              type = "prometheus";
              uid = "prometheus";
              url = localUrl ports.prometheus.local;
              jsonData = {
                httpMethod = "POST";
                timeInterval = "30s";
              };
            }
            {
              access = "proxy";
              editable = false;
              name = "Loki";
              type = "loki";
              uid = "loki";
              url = localUrl ports.loki.http;
            }
            {
              access = "proxy";
              editable = false;
              name = "Tempo";
              type = "tempo";
              uid = "tempo";
              url = localUrl ports.tempo.http;
              jsonData = {
                nodeGraph.enabled = true;
                tracesToLogsV2 = {
                  datasourceUid = "loki";
                  filterBySpanID = true;
                  filterByTraceID = true;
                };
              };
            }
          ];
        };
      };
    };

    loki = {
      enable = true;
      configuration = {
        auth_enabled = false;
        analytics.reporting_enabled = false;
        server = {
          grpc_listen_address = "127.0.0.1";
          grpc_listen_port = ports.loki.grpc;
          http_listen_address = "127.0.0.1";
          http_listen_port = ports.loki.http;
          log_level = "warn";
        };
        common = {
          instance_addr = "127.0.0.1";
          path_prefix = "/var/lib/loki";
          replication_factor = 1;
          ring.kvstore.store = "inmemory";
          storage.filesystem = {
            chunks_directory = "/var/lib/loki/chunks";
            rules_directory = "/var/lib/loki/rules";
          };
        };
        schema_config.configs = [
          {
            from = "2024-01-01";
            index = {
              period = "24h";
              prefix = "index_";
            };
            object_store = "filesystem";
            schema = "v13";
            store = "tsdb";
          }
        ];
        compactor = {
          delete_request_store = "filesystem";
          retention_enabled = true;
          working_directory = "/var/lib/loki/compactor";
        };
        limits_config = {
          allow_structured_metadata = true;
          ingestion_burst_size_mb = 6;
          ingestion_rate_mb = 4;
          max_entries_limit_per_query = 5000;
          max_query_lookback = "168h";
          reject_old_samples = true;
          reject_old_samples_max_age = "24h";
          retention_period = "168h";
        };
      };
    };

    tempo = {
      enable = true;
      configFile = checkedTempoConfig;
    };

    prometheus = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = ports.prometheus.local;
      retentionTime = "30d";
      extraFlags = [ "--storage.tsdb.retention.size=10GB" ];
      globalConfig = {
        evaluation_interval = "30s";
        scrape_interval = "30s";
        scrape_timeout = "10s";
      };
      scrapeConfigs = [
        {
          job_name = "prometheus";
          static_configs = [ { targets = [ "127.0.0.1:${toString ports.prometheus.local}" ]; } ];
        }
        {
          job_name = "grafana";
          static_configs = [ { targets = [ "127.0.0.1:${toString ports.grafana.local}" ]; } ];
        }
        {
          job_name = "loki";
          static_configs = [ { targets = [ "127.0.0.1:${toString ports.loki.http}" ]; } ];
        }
        {
          job_name = "tempo";
          static_configs = [ { targets = [ "127.0.0.1:${toString ports.tempo.http}" ]; } ];
        }
        {
          job_name = "otel-hostmetrics";
          static_configs = [ { targets = [ "127.0.0.1:${toString ports.otel.metrics}" ]; } ];
        }
        {
          job_name = "otel-collector";
          static_configs = [ { targets = [ "127.0.0.1:${toString ports.otel.telemetry}" ]; } ];
        }
        {
          job_name = "systemd";
          static_configs = [ { targets = [ "127.0.0.1:${toString ports.systemdExporter.local}" ]; } ];
        }
      ];
      exporters.systemd = {
        enable = true;
        listenAddress = "127.0.0.1";
        openFirewall = false;
        port = ports.systemdExporter.local;
        extraFlags = [
          "--systemd.collector.enable-restart-count"
          "--systemd.collector.unit-include=${selectedSystemdUnits}"
        ];
      };
    };

    opentelemetry-collector = {
      enable = true;
      package = pkgs.opentelemetry-collector-contrib;
      configFile = otelConfig;
      validateConfigFile = true;
    };
  };

  systemd.services = {
    grafana.serviceConfig = {
      CPUQuota = "75%";
      MemoryHigh = "384M";
      MemoryMax = "768M";
    };
    loki.serviceConfig = {
      CPUQuota = "100%";
      MemoryHigh = "768M";
      MemoryMax = "1536M";
    };
    tempo.serviceConfig = {
      CPUQuota = "100%";
      MemoryHigh = "512M";
      MemoryMax = "1024M";
    };
    prometheus.serviceConfig = {
      CPUQuota = "100%";
      MemoryHigh = "768M";
      MemoryMax = "1536M";
    };
    opentelemetry-collector.serviceConfig = {
      CPUQuota = "75%";
      MemoryHigh = "384M";
      MemoryMax = "768M";
    };
  };
}
