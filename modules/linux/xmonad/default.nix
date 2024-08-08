{
  config,
  pkgs,
  colorscheme,
  ...
}:
{
  xsession = {
    enable = true;
    scriptPath = ".hm-xsession";
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: with haskellPackages; [ taffybar ];
      config =
        pkgs.writeText "xmonad.hs" # haskell
          ''
            ${builtins.readFile ./config.hs}

            myFocusedBorderColor = "${colorscheme.accent-primary}"
            myNormalBorderColor = "${colorscheme.bg-primary-bright}"
          '';
    };
  };
}
