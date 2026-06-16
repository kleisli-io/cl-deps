# buildLisp.script — shell-script launcher that loads FASLs at runtime
# instead of dumping a Lisp image. Use this for programs whose CFFI
# dependencies spawn C threads at load time (GTK, GObject, WebKit) —
# `save-lisp-and-die` cannot serialise non-Lisp threads.
#
# Trade-off: slower startup (FASL load on every invocation) but no
# image-dump constraint.
#
# Optional Swank REPL embedding and three-stage sandboxing follow the
# same typed-input contract as `buildLisp.program`.
#
# Input shape:
#   { name             : String
#   , srcs             : [Path]              ? []
#   , implementation   : Implementation       ? defaultImplementation
#   , brokenOn         : [String]             ? []
#   , main             : String               ? "${name}:main"
#   , deps             : [LispLibrary]        ? []
#   , cLibraries       : [Package]            ? []
#   , tests            : TestSpec | Null      ? null
#   , commandTools     : { name = Package; }  ? {}
#   , passthru         : AttrSet              ? {}
#   , dynamicSpaceSize : Int | Null           ? null
#   , preLaunch        : String               ? ""    # Lisp run before main
#   , extraEnv         : { name = String | [String]; } ? {}
#   , swank            : REPLServerSpec | Null ? null
#   , sandbox          : SandboxProfile | Null ? null
#   }

{ lib, pkgs, fx, mb, implFilter, allDeps, allNative, testSuite, defaultImplementation, libraryBuilder, swankLib, swankCodegen, validateSpec }:

let
  inherit (pkgs) runCommand makeWrapper writeText writeShellScriptBin;
  inherit (fx.state) forceThunk;

  toolEnvOrn = mb.ornaments.toolEnv;
  sandboxOrn = mb.ornaments.sandbox;
  unwrapDeps = mb.ornaments.dependencies.unwrapDeps;

  inherit (import ./internal/swank.nix { }) codegenConfig;
  inherit (import ./internal/test.nix { inherit testSuite; }) mkTestDrv;
  sandboxDerivedLib = import ./internal/sandbox-derived.nix {
    inherit lib mb sandboxOrn forceThunk;
  };
in

{ name
, srcs ? [ ]
, implementation ? defaultImplementation
, brokenOn ? [ ]
, main ? "${name}:main"
, deps ? [ ]
, cLibraries ? [ ]
, tests ? null
, commandTools ? { }
, passthru ? { }
, dynamicSpaceSize ? null
, preLaunch ? ""
, extraEnv ? { }
, swank ? null
, sandbox ? null
, __defaultImplName ? null
, __brokenOn ? [ ]
}:

let
  _validated = validateSpec {
    kind = "script";
    inherit name;
    spec = {
      inherit name srcs implementation brokenOn main deps cLibraries tests
        commandTools passthru dynamicSpaceSize preLaunch extraEnv
        swank sandbox;
    };
  };

  filteredSrcs = implFilter implementation srcs;
  filteredDeps = implFilter implementation (unwrapDeps deps);
  lispDeps = allDeps implementation filteredDeps;
  libPath = lib.makeLibraryPath (allNative cLibraries (unwrapDeps lispDeps));
  toolEnv = toolEnvOrn.create commandTools;

  hasSrcs = filteredSrcs != [ ];
  # `swank = null` is absence; `swank.enable = false` is declared-but-
  # inactive. Operational wiring gates on activation; passthru exposure
  # gates on declaration alone, so inspectors can distinguish
  # "configured but disabled" from "never configured" via `replSpec`
  # presence.
  swankDeclared = swank != null;
  swankEnabled = swankDeclared && swank.enable;
  sandboxEnabled = sandbox != null;

  swankCfg = lib.optionalAttrs swankEnabled (codegenConfig swank);
  wrapperCode = if !swankEnabled then null else
  writeText "${name}-swank-wrapper.lisp"
    (swankCodegen.mkWrapper {
      inherit name main;
      config = swankCfg;
      inherit sandboxEnabled;
    });

  selfLib =
    if hasSrcs
    then
      libraryBuilder
        {
          inherit name brokenOn commandTools cLibraries;
          # `lispDeps` carries a `markResolved` record; unwrap to a plain
          # list so `libraryBuilder`'s typed input contract (`deps :
          # [LispLibrary]`) holds. The library re-applies `unwrapDeps`
          # idempotently, so this is a no-op operationally.
          deps = unwrapDeps lispDeps;
          srcs = filteredSrcs;
        }
    else null;

  # Load order: dep FASLs first, then optional swank wrapper, then
  # optional preLaunch. The launcher script invokes `(run)` (swank) or
  # `(${main})` (no swank) after this file finishes loading.
  loadDeps =
    lib.optional hasSrcs selfLib
    ++ unwrapDeps lispDeps
    ++ lib.optional swankEnabled swankLib;
  loadableDeps = builtins.filter (d: (d.lispSrcs or [ ]) != [ ]) loadDeps;

  loadScript = writeText "${name}-load.lisp" ''
    ${lib.concatMapStringsSep "\n" (dep:
      "(load \"${dep}/${dep.lispName}.${implementation.faslExt}\")"
    ) loadableDeps}

    ${lib.optionalString swankEnabled ''
    (load "${wrapperCode}")
    ''}

    ${lib.optionalString (preLaunch != "") preLaunch}
  '';

  runtimeArgs = lib.optionalString (dynamicSpaceSize != null)
    "--dynamic-space-size ${toString dynamicSpaceSize}";

  # `extraEnv`: string values export verbatim; list values join with `:`
  # and prepend onto any existing value (path-list semantics).
  extraEnvScript = lib.concatStringsSep "\n" (lib.mapAttrsToList
    (var: value:
      if builtins.isList value
      then "export ${var}=\"${lib.concatStringsSep ":" value}\${${var}:+:}\$${var}\""
      else "export ${var}=\"${value}\""
    )
    extraEnv);
  extraEnvFile =
    if extraEnv == { } then null
    else writeText "${name}-env.sh" extraEnvScript;

  inherit (sandboxDerivedLib.derive { inherit sandbox sandboxEnabled; })
    landlock sandboxBpfBwrap sandboxBpfSelf sandboxBpfSelfFile
    sandboxRoPaths sandboxRwPaths;
  sandboxBwrapArgs = if sandboxEnabled then sandboxOrn.toBwrap sandbox else [ ];

  # `--` separator: SBCL parses argv up to this token as runtime opts;
  # the eval-form strips it post-hoc so UIOP-based parsers see clean
  # user args.
  sbclCmd = ''
    ${pkgs.sbcl}/bin/sbcl \
      ${runtimeArgs} \
      ''${NIX_BUILDLISP_LISP_ARGS:-} \
      --noinform \
      --load "${loadScript}" \
      --eval "(setf sb-ext:*posix-argv* (delete \"--\" sb-ext:*posix-argv* :test #'string= :count 1))" \
      --eval "${if swankEnabled then "(buildlisp-repl-wrapper:run)" else "(${main})"}" \
      --quit \
      -- "$@"
  '';

  launcher = writeShellScriptBin name (if sandboxEnabled then ''
    export LD_LIBRARY_PATH="${libPath}''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH"
    export LANG="C.UTF-8"
    ${lib.optionalString (extraEnvFile != null) ". ${extraEnvFile}"}
    ${toolEnvOrn.toExportSnippet toolEnv}

    export DEPOT_ROOT="''${DEPOT_ROOT:-$(pwd)}"

    ${lib.optionalString swankEnabled ''
    REPL_DIR="''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/repl/swank"
    mkdir -p "$REPL_DIR"
    ''}

    ${lib.optionalString sandbox.sourceAccess ''
    if [ -n "$DEPOT_ROOT" ] && [ -d "$DEPOT_ROOT" ]; then
      SOURCE_ROOT="$(cd "$DEPOT_ROOT" && ${pkgs.git}/bin/git rev-parse --show-toplevel 2>/dev/null || echo "$DEPOT_ROOT")"
    fi
    ''}

    ${lib.optionalString (sandbox.sourceWritePaths != []) ''
    SOURCE_WRITE_BINDS=""
    SOURCE_WRITE_EXTRA=""
    ${lib.concatMapStringsSep "\n    " (rel: ''
    if [ -n "$DEPOT_ROOT" ]; then
      _swp="$DEPOT_ROOT/${rel}"
      mkdir -p "$_swp"
      SOURCE_WRITE_BINDS="$SOURCE_WRITE_BINDS --bind $_swp $_swp"
      SOURCE_WRITE_EXTRA="$SOURCE_WRITE_EXTRA:$_swp"
    fi'') sandbox.sourceWritePaths}
    ''}

    ${lib.optionalString (sandbox.coordinationWritePaths != []) ''
    if [ -n "$SOURCE_ROOT" ]; then
      ${lib.concatMapStringsSep "\n      " (pattern: ''
      for _cwp in $SOURCE_ROOT/${pattern}; do
        if [ -d "$_cwp" ]; then
          SOURCE_WRITE_BINDS="$SOURCE_WRITE_BINDS --bind $_cwp $_cwp"
          SOURCE_WRITE_EXTRA="$SOURCE_WRITE_EXTRA:$_cwp"
        fi
      done'') sandbox.coordinationWritePaths}
    fi
    ''}

    export SANDBOX_BPF_SELF="${sandboxBpfSelfFile}"
    export SANDBOX_READ_ONLY_PATHS="${sandboxRoPaths}${lib.optionalString sandbox.sourceAccess ":$SOURCE_ROOT"}"
    export SANDBOX_READ_WRITE_PATHS="${sandboxRwPaths}${lib.optionalString swankEnabled ":$REPL_DIR"}''${SOURCE_WRITE_EXTRA:-}"
    export SANDBOX_ALLOW_EXECVE="${if landlock.allowExecve then "1" else "0"}"

    # shellcheck disable=SC2086
    exec ${pkgs.bubblewrap}/bin/bwrap \
      ${lib.concatStringsSep " \\\n      " sandboxBwrapArgs} \
      ${lib.optionalString sandbox.sourceAccess ''--ro-bind "$SOURCE_ROOT" "$SOURCE_ROOT" \
      ''}${lib.optionalString swankEnabled ''--bind "$REPL_DIR" "$REPL_DIR" \
      ''}$SOURCE_WRITE_BINDS \
      --setenv LD_LIBRARY_PATH "${libPath}''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH" \
      --setenv LANG "C.UTF-8" \
      --setenv DEPOT_ROOT "$DEPOT_ROOT" \
      --setenv SANDBOX_BPF_SELF "${sandboxBpfSelfFile}" \
      --setenv SANDBOX_READ_ONLY_PATHS "${sandboxRoPaths}${lib.optionalString sandbox.sourceAccess ":$SOURCE_ROOT"}" \
      --setenv SANDBOX_READ_WRITE_PATHS "${sandboxRwPaths}${lib.optionalString swankEnabled ":$REPL_DIR"}''${SOURCE_WRITE_EXTRA:-}" \
      --setenv SANDBOX_ALLOW_EXECVE "${if landlock.allowExecve then "1" else "0"}" \
      --seccomp 3 \
      -- ${sbclCmd} \
      3< ${sandboxBpfBwrap}/${sandboxBpfBwrap.name}.bpf
  '' else ''
    export LD_LIBRARY_PATH="${libPath}''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH"
    export LANG="C.UTF-8"
    ${lib.optionalString (extraEnvFile != null) ". ${extraEnvFile}"}
    ${toolEnvOrn.toExportSnippet toolEnv}

    # shellcheck disable=SC2086
    exec ${sbclCmd}
  '');

  testDrv = mkTestDrv {
    inherit tests name filteredSrcs filteredDeps implementation commandTools;
  };
in
builtins.seq _validated (lib.fix (self: runCommand name
{
  nativeBuildInputs = [ makeWrapper ] ++ toolEnvOrn.toolInputs toolEnv;
  passthru = passthru // {
    lispName = name;
    lispDeps = if hasSrcs then [ selfLib ] else unwrapDeps lispDeps;
    lispNativeDeps = cLibraries;
    lispBinary = true;
    tests = testDrv;
    inherit brokenOn commandTools;
    toolPackages = toolEnvOrn.toolPackages toolEnv;
    lib = selfLib;
    inherit self loadScript;
    isScript = true;
  } // lib.optionalAttrs swankDeclared {
    replSpec = swank;
  } // lib.optionalAttrs sandboxEnabled {
    sandboxProfile = sandbox;
    inherit sandboxBpfBwrap sandboxBpfSelf;
  };
}
  (
    ''
      ${toolEnvOrn.toExportSnippet toolEnv}
      ${lib.optionalString (testDrv != null) ''
        if [ -f "${testDrv}" ]; then
          echo "Tests passed: ${testDrv}"
        else
          echo "Error: Tests must pass to build program"
          exit 1
        fi
      ''}
      mkdir -p $out/bin
      cp ${launcher}/bin/${name} $out/bin/${name}
    ''
  )))
