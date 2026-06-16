# Shared sandbox-derived values for the buildLispOrn binary builders.
#
# `landlock` is the typed projection consumed by the Lisp-side stage-2
# enforcement; the env vars built from it never read directly from
# `sandbox.<path-field>`. Bwrap-layer fields (`storeAccess`,
# `sourceAccess`, `source*Paths`, `coordinationWritePaths`) are outside
# Landlock's vocabulary and remain on the raw profile.
#
# `sandboxBwrapArgs` is intentionally NOT in the returned record:
# `program.nix` strips the blanket `/nix/store` bind when
# `!sandbox.storeAccess` and threads per-path binds via bwrap fd 4;
# `script.nix` passes `sandboxOrn.toBwrap` through unmodified. Each
# builder constructs its own bwrap-arg list.
#
# `portList` is a colon-joined renderer for Landlock's port lists.

{ lib, mb, sandboxOrn, forceThunk }:

let
  portList = ports: lib.concatMapStringsSep ":" toString ports;

  derive = { sandbox, sandboxEnabled }: rec {
    landlock = if sandboxEnabled then sandboxOrn.toLandlock sandbox else null;
    sandboxBpfBwrap =
      if sandboxEnabled
      then forceThunk (sandboxOrn.toSeccomp sandbox mb.operations.seccompStages.Bwrap)
      else null;
    sandboxBpfSelf =
      if sandboxEnabled
      then forceThunk (sandboxOrn.toSeccomp sandbox mb.operations.seccompStages.Self)
      else null;
    sandboxBpfSelfFile =
      if sandboxEnabled
      then "${sandboxBpfSelf}/${sandboxBpfSelf.name}.bpf"
      else null;
    sandboxRoPaths = lib.optionalString sandboxEnabled
      (lib.concatStringsSep ":" landlock.readOnlyPaths);
    sandboxRwPaths = lib.optionalString sandboxEnabled
      (lib.concatStringsSep ":" landlock.readWritePaths);
  };
in
{
  inherit derive portList;
}
