{ pkgs, ... }:
{
  services.xidlehook = {
    enable = true;
    # NOTE: The delays add onto the previous value (and the value is in seconds)
    timers = [
      {
        delay = 240;
        command = "${pkgs.notify-desktop}/bin/notify-desktop --app-name=betterlockscreen --urgency=critical \"Locking screen in 1 min\"";
        canceller = "${pkgs.notify-desktop}/bin/notify-desktop --app-name=betterlockscreen \"Locking screen cancelled\"";
      }
      {
        delay = 60;
        command = "${pkgs.betterlockscreen}/bin/betterlockscreen --wall blur -l";
      }
      {
        delay = 240 + 1200;
        command = "${pkgs.notify-desktop}/bin/notify-desktop --app-name=systemctl --urgency=critical \"Sleeping in 1 min\"";
        canceller = "${pkgs.notify-desktop}/bin/notify-desktop --app-name=systemctl \"Sleep cancelled\"";
      }
      {
        delay = 60;
        command = "systemctl -i suspend";
      }
    ];
  };
}
