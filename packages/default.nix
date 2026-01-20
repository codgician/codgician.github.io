{ pkgs, ... }:

with pkgs.lib;

let
  callPackage = callPackageWith (pkgs // mypkgs);
  mypkgs = pipe (builtins.readDir ./.) [
    (filterAttrs (_: type: type == "directory"))
    (mapAttrs (name: _: callPackage ./${name} { }))
  ];
in
filterAttrs (
  _: pkg:
  !(pkg.meta or { }) ? platforms || (builtins.elem pkgs.stdenv.hostPlatform.system pkg.meta.platforms)
) mypkgs
