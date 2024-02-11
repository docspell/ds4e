{
  description = "Flake for ds4e";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ flake-parts.flakeModules.easyOverlay ];

      systems = [ "x86_64-linux" "x86_64-darwin" ];

      perSystem = { config, pkgs, ... }: {
        packages = rec {
          ds4e = pkgs.callPackage (import ./nix/ds4e.nix) {};
          default = ds4e;
        };
      };
    };
}
