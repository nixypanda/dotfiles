{
  description = "Home manager flake";
  inputs = {
    # Use the current stable package set by default on both macOS and NixOS.
    # Individual packages can still opt into unstable where needed.
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-26.05-darwin";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/release-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur = {
      url = "github:nix-community/NUR";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixarr = {
      url = "github:nix-media-server/nixarr";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agent-skills = {
      url = "git+ssh://git@github.com/nixypanda/agent-skills.git";
      flake = false;
    };
    # Applying the configuration happens from the.dotfiles directory so the
    # relative path is defined accordingly. This has potential of causing issues.
    vim-plugins = {
      url = "path:/Users/nixypanda/.dotfiles/modules/nvim/plugins";
    };
    # MacOS specific inputs
    darwin = {
      url = "github:nix-darwin/nix-darwin/nix-darwin-26.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # my stuff
    kitty-upstream = {
      url = "github:nixypanda/kitty/floating-pane-experiment";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    calco = {
      url = "git+ssh://git@github.com/nixypanda/calco.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    onepacerr-ui = {
      url = "git+ssh://git@github.com/nixypanda/onepacerr-ui.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    hedger = {
      url = "git+ssh://git@github.com/nixypanda/hedger.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    ownai = {
      url = "git+ssh://git@github.com/nixypanda/ownai.git";
    };
  };
  outputs =
    {
      nur,
      agent-skills,
      vim-plugins,
      nixpkgs,
      nixpkgs-unstable,
      home-manager,
      agenix,
      darwin,
      kitty-upstream,
      nixarr,
      calco,
      onepacerr-ui,
      hedger,
      ownai,
      ...
    }:
    let
      inherit (nixpkgs) lib;
      kitty-dev-build-overlay = import ./modules/kitty/dev-overlay.nix { inherit kitty-upstream; };

      # These overlays are scoped to the Home Manager package set. nix-darwin
      # intentionally keeps plain nixpkgs for system configuration.
      macOverlays = [
        kitty-dev-build-overlay
        (_: prev: { agenix = agenix.packages.${prev.system}.default; })
        nur.overlays.default
        vim-plugins.overlay
      ];

      # Mac hosts mapped to the system each builds for. Everything the Macs
      # share lives in modules/mac/; a host directory only holds what is
      # genuinely host-specific (see hosts/<host>/system/configuration.nix).
      macHosts = {
        srt-l02-sekhmet = "x86_64-darwin";
        srt-l03-shire = "aarch64-darwin";
      };

      mkMacHome = _name: system: home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.${system}.extend (lib.composeManyExtensions macOverlays);
        extraSpecialArgs = {
          inherit agent-skills nixpkgs-unstable ownai;
        };
        modules = [
          ./modules/mac/home.nix
        ];
      };

      mkMacSystem = name: system: darwin.lib.darwinSystem {
        pkgs = nixpkgs.legacyPackages.${system};
        modules = [
          ./hosts/${name}/system/configuration.nix
        ];
      };
    in
    {
      homeConfigurations = lib.mapAttrs mkMacHome macHosts;

      darwinConfigurations = lib.mapAttrs mkMacSystem macHosts;

      nixosConfigurations."srt-n01-rivendell" = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        specialArgs = { inherit onepacerr-ui; };
        modules = [
          ./hosts/srt-n01-rivendell/configuration.nix
          agenix.nixosModules.default
          nixarr.nixosModules.default
          calco.nixosModules.default
          hedger.nixosModules.default
          home-manager.nixosModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              users.nixypanda = import ./hosts/srt-n01-rivendell/home.nix;
            };
          }
          {
            nixpkgs.overlays = [
              (final: _: { agenix = agenix.packages.${final.system}.default; })
            ];
          }
        ];
      };
    };
}
