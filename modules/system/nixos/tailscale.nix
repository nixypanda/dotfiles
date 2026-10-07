_: {
  services.tailscale = {
    enable = true;
    openFirewall = true;
    # Without a tailnet-wide global resolver, accepting Tailscale DNS makes
    # 100.100.100.100 recursively depend on itself and external lookups fail.
    extraSetFlags = [ "--accept-dns=false" ];
  };
}
