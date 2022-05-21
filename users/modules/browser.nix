{ config, pkgs, libs, ... }:
let
  one-password-id = "aeblfdkhhhdcdjpifhhbdiojplfjncoa";
  adblock-id = "gighmmpiobklfepjocnamgkkbiglidom";
  dark-reader-id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
  vimium-id = "dbepggeogbaibhgnhhndojpepiihcmeb";
  octotree-id = "bkhaagjahfmjljalopjnoealnfndnagc";
  plasma-integration-id = "cimiefiiaegbelhefglklhhakcgmhkai";
  keepa-id = "neebplgakaahbhdphmkckjjcegoiijjo"; # Amazon price tracker
  tosdr-id = "hjdoplcnndgiblooccencgcggcoihigg"; # Terms of Service; Didn't read
  grammerly-id = "kbfnbcaeplbcioakkpcpgfkobkghlhen";
  return-youtube-dislikes = "gebbhagfogifgggkldgodflihgfeippi";
  pocket-tube-id = "kdmnjgijlmjgmimahnillepgcgeemffb"; # Youtube subscription manager
  color-picker = "ohcpnigalekghcmgcdcenkpelffpdolg";
  okta-id = "glnpjglilkicbckjpbgcfkogebgllemb";
  unhook-id = "khncfooichmfjbepaaaebmommgaepoid";
in
{
  programs.chromium = {
    enable = true;
    extensions = [
      { id = one-password-id; }
      { id = adblock-id; }
      { id = dark-reader-id; }
      { id = vimium-id; }
      { id = octotree-id; }
      { id = plasma-integration-id; }
      { id = keepa-id; }
      { id = tosdr-id; }
      { id = grammerly-id; }
      { id = return-youtube-dislikes; }
      { id = pocket-tube-id; }
      { id = color-picker; }
      { id = okta-id; }
      { id = unhook-id; }
    ];
  };
}
