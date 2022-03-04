{
  description = "XMonad Config";
  inputs = {
    unstable.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  outputs = inputs:
    let
      system = "x86_64-linux";
      pkgs = inputs.unstable.legacyPackages.${system};
      haskellDeps = ps: with ps; [
        xmonad
        xmonad-contrib
        xmonad-extras
        haskell-language-server
        taffybar
      ];
    in
    {
      devShell."${system}" = pkgs.mkShell {
        buildInputs = with pkgs; [ (ghc.withPackages haskellDeps) ];
      };
    };
}
