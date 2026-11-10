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
    rex = {
      url = "https://pkgs.superlogical.com/rex/nix/snapshot.tar.gz";
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
    # OpenCode Rex activity plugin; plain source tree, not a flake.
    opencode-rex-activity = {
      url = "github:nixypanda/opencode-rex-activity";
      flake = false;
    };
    # Applying the configuration happens from the.dotfiles directory so the
    # relative path is defined accordingly. This has potential of causing issues.
    vim-plugins = {
      url = "path:/Users/nixypanda/.dotfiles/pkgs/vim-plugins";
    };
    # Local development path until this project is published as a git repo.
    beets-south-asian-retag = {
      url = "path:/Users/nixypanda/Documents/media/beets-south-asian-retag";
      inputs.nixpkgs.follows = "nixpkgs";
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
      nixarr,
      rex,
      calco,
      onepacerr-ui,
      hedger,
      codect,
      opencode-rex-activity,
      beets-south-asian-retag,
      ...
    }:
    let
      inherit (nixpkgs) lib;
      agenixOverlay = final: _: { agenix = agenix.packages.${final.system}.default; };

      # These overlays are scoped to the Home Manager package set. nix-darwin
      # intentionally keeps plain nixpkgs for system configuration.
      darwinOverlays = [
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
            inherit codect opencode-rex-activity;
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
          specialArgs = { inherit onepacerr-ui beets-south-asian-retag; };
          modules = [
            ./hosts/${name}/configuration.nix
            beets-south-asian-retag.nixosModules.default
            agenix.nixosModules.default
            nixarr.nixosModules.default
            rex.nixosModules.default
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

      # Exposed for build tests. The package itself is defined under pkgs/soulsync.
      packages = {
        x86_64-linux = {
          soulsync = nixpkgs.legacyPackages.x86_64-linux.callPackage ./pkgs/soulsync { };
        };
      };
    };
}
