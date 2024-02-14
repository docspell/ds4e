{
  description = "Flake for ds4e";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils }:
    {
      overlays.default = final: prev: {
        ds4e = final.pkgs.callPackage (import ./nix/ds4e.nix) {};
      };
     } // flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system;
      };
    in
    {
      packages = rec {
        ds4e = pkgs.callPackage (import ./nix/ds4e.nix) {};
        default = ds4e;
      };
    });
}
