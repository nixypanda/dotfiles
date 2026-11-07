{
  description = "Home manager flake";
  inputs = {
    # Track nixpkgs unstable for both macOS and NixOS.
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
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
    darwin = {
      url = "github:nix-darwin/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # my stuff
    kitty-upstream = {
      url = "github:nixypanda/kitty/floating-pane-clean";
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
    codect = {
      url = "git+ssh://git@github.com/nixypanda/codect.git";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # Applying the configuration happens from the.dotfiles directory so the
    # relative path is defined accordingly. This has potential of causing issues.
    vim-plugins = {
      url = "path:/Users/nixypanda/.dotfiles/pkgs/vim-plugins";
    };
  };
  outputs =
    {
      nur,
      vim-plugins,
      nixpkgs,
      home-manager,
      agenix,
      darwin,
      kitty-upstream,
      nixarr,
      calco,
      onepacerr-ui,
      hedger,
      codect,
      ...
    }:
    let
      inherit (nixpkgs) lib;
      kitty-dev-build-overlay = import ./pkgs/kitty-fork-overlay.nix { inherit kitty-upstream; };
      agenixOverlay = final: _: { agenix = agenix.packages.${final.system}.default; };

      # These overlays are scoped to the Home Manager package set. nix-darwin
      # intentionally keeps plain nixpkgs for system configuration.
      darwinOverlays = [
        kitty-dev-build-overlay
        agenixOverlay
        nur.overlays.default
        vim-plugins.overlay
      ];

      # Single source of truth for every machine this flake manages. The
      # justfile reads `hosts` and `hostAliases`, so adding a host does not
      # require a new recipe.
      hostRegistry = {
        srt-l03-shire = {
          platform = "darwin";
          system = "aarch64-darwin";
          user = "nixypanda";
          aliases = [ "shire" ];
        };
        srt-n01-rivendell = {
          platform = "nixos";
          system = "x86_64-linux";
          user = "nixypanda";
          aliases = [ "rivendell" ];
        };
      };

      hostsOf = platform: lib.filterAttrs (_: host: host.platform == platform) hostRegistry;

      # Flat alias -> canonical-name map for CLI lookup: `shire` and
      # `srt-l03-shire` both resolve to `srt-l03-shire`.
      hostAliases = lib.listToAttrs (
        lib.concatLists (
          lib.mapAttrsToList (
            name: host: map (alias: lib.nameValuePair alias name) ([ name ] ++ host.aliases)
          ) hostRegistry
        )
      );

      # Darwin hosts mapped to the system each builds for. Shared config lives
      # in modules/, composed by the workstation profile; a host directory
      # holds only what is genuinely host-specific.
      darwinHosts = lib.mapAttrs (_: host: host.system) (hostsOf "darwin");

      mkDarwinHome =
        name: system:
        home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.${system}.extend (lib.composeManyExtensions darwinOverlays);
          extraSpecialArgs = {
            inherit codect;
          };
          modules = [
            ./hosts/${name}/home.nix
          ];
        };

      mkDarwinSystem =
        name: system:
        darwin.lib.darwinSystem {
          pkgs = nixpkgs.legacyPackages.${system};
          modules = [
            ./hosts/${name}/configuration.nix
          ];
        };

      mkNixos =
        name: host:
        nixpkgs.lib.nixosSystem {
          inherit (host) system;
          specialArgs = { inherit onepacerr-ui; };
          modules = [
            ./hosts/${name}/configuration.nix
            agenix.nixosModules.default
            nixarr.nixosModules.default
            calco.nixosModules.default
            hedger.nixosModules.default
            home-manager.nixosModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.${host.user} = import ./hosts/${name}/home.nix;
              };
            }
            {
              nixpkgs.overlays = [ agenixOverlay ];
            }
          ];
        };
    in
    {
      hosts = hostRegistry;

      inherit hostAliases;

      homeConfigurations = lib.mapAttrs mkDarwinHome darwinHosts;

      darwinConfigurations = lib.mapAttrs mkDarwinSystem darwinHosts;

      nixosConfigurations = lib.mapAttrs mkNixos (hostsOf "nixos");
    };
}
