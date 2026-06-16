# Typed input contracts for the buildLispOrn builders.
#
# Each builder accepts an open attrset of arguments today. This module
# declares HOAS product types over those argument shapes plus a
# `validate` smart-checker that:
#   * runs `fx.types.validateValue` against the spec type and emits
#     diagnostics as `lib.warn` messages (warn mode — fixture-safe);
#   * for daemons additionally enforces the structural REPL constraints
#     (`swank.mode = Foreground`, `swank.lifecycle = LongRunning`) as
#     hard throws, preserving the existing user-facing error contract.
#
# Type hierarchy:
#   LispScriptableSpec      — common root (name/srcs/deps/cLibraries/
#                             tests/commandTools/passthru/brokenOn/
#                             implementation).
#   LispLibrarySpec         — ornaments root with replInit, muffle.
#   LispProgramSpec         — ornaments root with main, verifyPackages,
#                             preDump, dynamicSpaceSize, runtimeAssets,
#                             swank, sandbox.
#   LispScriptSpec          — ornaments root with main, dynamicSpaceSize,
#                             preLaunch, extraEnv, swank, sandbox.
#   LispDaemonSpec          — ornaments LispProgramSpec with serviceSpec.
#                             swank.mode / swank.lifecycle constrained
#                             at the validator (kernel ornament algebra
#                             cannot refine a sub-datatype's constructor
#                             at the field level without re-encoding).
#
# Implementation, commandTools, passthru, extraEnv, serviceSpec and
# similar open-attrset slots type as `H.attrs`. Their inner shape is
# validated downstream by the consumers that interpret them.

{ world, lib, ... }:

let
  fx = world.nix.nix-effects;
  mb = world.nix.metaBuilderOrn;

  H = fx.types.hoas;
  validateValue = fx.types.validateValue;

  REPLServerSpec = mb.descriptions.REPLServerSpec;
  SandboxProfile = mb.descriptions.SandboxProfile;

  # `TestSpec` documents the inner shape `mkTestDrv` consumes. It is
  # NOT enforced at the LispScriptableSpec layer because `H.product`
  # requires every declared field to be present on the attrset (the
  # `H.maybe` parent only relaxes the field VALUE shape, not its
  # PRESENCE). Depot callers pass partial attrsets — `tests =
  # { expression = "t"; }` is canonical — so the parent's `tests` field
  # types as `H.maybe H.attrs` and inner validation happens inside
  # `mkTestDrv` at the point of use.
  TestSpec = H.product "LispTestSpec" [
    (H.field "name" (H.maybe H.string))
    (H.field "srcs" (H.listOf H.any))
    (H.field "deps" (H.listOf H.derivation))
    (H.field "expression" H.string)
    (H.field "commandTools" H.attrs)
  ];

  # `srcs` accepts both literal Nix paths (e.g. `src + "/foo.lisp"`)
  # and derivations (e.g. `pkgs.writeText`); kernel lacks a path|drv
  # union form, so type at `H.any` and rely on downstream builders to
  # reject non-realisable entries. `commandTools`, `passthru`,
  # `implementation`, `extraEnv`, `serviceSpec` are intentionally open
  # attrsets; their inner shape is validated by the consumers that
  # interpret them.
  LispScriptableSpec = H.product "LispScriptableSpec" [
    (H.field "name" H.string)
    (H.field "srcs" (H.listOf H.any))
    (H.field "implementation" H.attrs)
    (H.field "brokenOn" (H.listOf H.string))
    (H.field "deps" (H.listOf H.derivation))
    (H.field "cLibraries" (H.listOf H.derivation))
    (H.field "tests" (H.maybe H.attrs))
    (H.field "commandTools" H.attrs)
    (H.field "passthru" H.attrs)
  ];

  scriptableKeeps = [
    { keep = "name"; }
    { keep = "srcs"; }
    { keep = "implementation"; }
    { keep = "brokenOn"; }
    { keep = "deps"; }
    { keep = "cLibraries"; }
    { keep = "tests"; }
    { keep = "commandTools"; }
    { keep = "passthru"; }
  ];

  LispLibrarySpec = H.ornament LispScriptableSpec {
    name = "LispLibrarySpec";
    constructors.LispScriptableSpec.fields = scriptableKeeps ++ [
      { insert = "replInit"; type = H.maybe H.string; }
      { insert = "muffle"; type = H.listOf H.string; }
      { insert = "resources"; type = H.attrs; }
    ];
  };

  # `runtimeAssets` admits both derivations and store-path strings.
  # The program builder interpolates entries as `"${d}"`, which coerces
  # both shapes; the daemon flow merges `builtins.attrValues
  # serviceSpec.assets` (which can be strings) into the same list.
  # Same path|drv-union absence rationale as `srcs`.
  LispProgramSpec = H.ornament LispScriptableSpec {
    name = "LispProgramSpec";
    constructors.LispScriptableSpec.fields = scriptableKeeps ++ [
      { insert = "main"; type = H.string; }
      { insert = "verifyPackages"; type = H.listOf H.string; }
      { insert = "preDump"; type = H.string; }
      { insert = "dynamicSpaceSize"; type = H.maybe H.int_; }
      { insert = "runtimeAssets"; type = H.listOf H.any; }
      { insert = "swank"; type = H.maybe REPLServerSpec.T; }
      { insert = "sandbox"; type = H.maybe SandboxProfile.T; }
    ];
  };

  LispScriptSpec = H.ornament LispScriptableSpec {
    name = "LispScriptSpec";
    constructors.LispScriptableSpec.fields = scriptableKeeps ++ [
      { insert = "main"; type = H.string; }
      { insert = "dynamicSpaceSize"; type = H.maybe H.int_; }
      { insert = "preLaunch"; type = H.string; }
      { insert = "extraEnv"; type = H.attrs; }
      { insert = "swank"; type = H.maybe REPLServerSpec.T; }
      { insert = "sandbox"; type = H.maybe SandboxProfile.T; }
    ];
  };

  # LispProgramSpec, like every ornament, reuses the root base's
  # constructor name (`LispScriptableSpec`). Refining it requires
  # spelling out the inherited program-level fields as keeps.
  LispDaemonSpec = H.ornament LispProgramSpec {
    name = "LispDaemonSpec";
    constructors.LispScriptableSpec.fields = [
      { keep = "name"; }
      { keep = "srcs"; }
      { keep = "implementation"; }
      { keep = "brokenOn"; }
      { keep = "deps"; }
      { keep = "cLibraries"; }
      { keep = "tests"; }
      { keep = "commandTools"; }
      { keep = "passthru"; }
      { keep = "main"; }
      { keep = "verifyPackages"; }
      { keep = "preDump"; }
      { keep = "dynamicSpaceSize"; }
      { keep = "runtimeAssets"; }
      { keep = "swank"; }
      { keep = "sandbox"; }
      { insert = "serviceSpec"; type = H.maybe H.attrs; }
    ];
  };

  specTypeOf = kind:
    if kind == "library" then LispLibrarySpec.T
    else if kind == "program" then LispProgramSpec.T
    else if kind == "script" then LispScriptSpec.T
    else if kind == "daemon" then LispDaemonSpec.T
    else throw "buildLisp.descriptions.specTypeOf: unknown kind '${kind}'";

  # All four spec types are ornaments rooted at LispScriptableSpec, so
  # they share the root base's constructor tag (`ornamentedCon` reuses
  # the base `_con` by kernel invariant). The validator stamps this tag
  # on the user's untagged arg attrset before checking.
  rootConName = "LispScriptableSpec";

  # Path segments expose pre-formatted `segment` strings (`.field` or
  # `[N]` style) directly from the kernel diagnostic walker. Concat
  # without separator so nested paths render as `.tests.name` etc.
  formatDiagnostic = d:
    let
      path = d.path or [ ];
      pathStr =
        if path == [ ] then "<root>"
        else lib.concatStrings (map (p: p.segment or ".${p.name or "?"}") path);
    in
    "  - ${pathStr}: ${d.reason or "<no reason>"}";

  # Daemon-specific structural REPL constraint. Kept as a value-level
  # check because the kernel ornament algebra cannot refine
  # `swank : Maybe REPLServerSpec` to a Foreground/LongRunning-only
  # variant without re-encoding the mode/lifecycle enums.
  daemonReplCheck = name: swank:
    if swank == null then null
    else if (swank.mode._con or null) != "Foreground" then
      throw "buildLisp.daemon[${name}]: swank.mode must be Foreground; got ${swank.mode._con or "<unknown>"}. The daemon pattern requires the REPL on the main thread (keep-alive) and user code on a worker thread. Use buildLisp.program or buildLisp.script for Background-mode REPL embedding."
    else if (swank.lifecycle._con or null) != "LongRunning" then
      throw "buildLisp.daemon[${name}]: swank.lifecycle must be LongRunning; got ${swank.lifecycle._con or "<unknown>"}. Use buildLisp.script for OneShot lifecycle."
    else null;

  # validate { kind, name, spec } → spec
  #   * adds `_con` tag matching the kind's HOAS product
  #   * runs validateValue; emits lib.warn on non-empty diagnostics
  #   * for daemon, enforces the swank mode/lifecycle structural check
  #   * returns the ORIGINAL (untagged) spec — caller continues with the
  #     existing arg destructure values
  validate = { kind, name, spec }:
    let
      ty = specTypeOf kind;
      tagged = spec // { _con = rootConName; };
      diagnostics = validateValue [ ] ty tagged;
      report =
        "buildLisp.${kind}[${name}]: ${toString (builtins.length diagnostics)} diagnostic(s):\n"
        + lib.concatStringsSep "\n" (map formatDiagnostic diagnostics);
      # Force the daemon REPL check even when it returns null, so its
      # throw side-effect propagates ahead of the warn/passthrough.
      daemonCheck =
        if kind == "daemon"
        then daemonReplCheck name (spec.swank or null)
        else null;
      passed =
        if diagnostics == [ ] then spec
        else lib.warn report spec;
    in
    builtins.seq daemonCheck passed;

in
{
  inherit
    TestSpec
    LispScriptableSpec
    LispLibrarySpec
    LispProgramSpec
    LispScriptSpec
    LispDaemonSpec
    validate;
}
