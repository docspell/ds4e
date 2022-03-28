{ pkgs ? import <nixpkgs> {} }:

pkgs.callPackage (import nix/ds4e.nix) {}
