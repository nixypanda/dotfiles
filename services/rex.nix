{ homelab, ... }:
{
  services.rex.loginServer = {
    enable = true;
    tailscaleServe = {
      enable = true;
      # Keep Caddy's HTTPS endpoint on port 443 available.
      httpsPort = homelab.services.rex.tailnet;
    };
  };
}
