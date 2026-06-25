# Globs the in-repo libs, threading `final` so cross-lib deps resolve.
# Keys come from readDir (static), values force `final.*` lazily — no cycle.
{ pkgs, lib, buildLisp, final }:

let
  imp = dir:
    lib.mapAttrs
      (name: _: import (dir + "/${name}") {
        inherit pkgs lib buildLisp;
        lisp = final;
      })
      (lib.filterAttrs (_: t: t == "directory") (builtins.readDir dir));
in
imp ./first-party // imp ./vendored
