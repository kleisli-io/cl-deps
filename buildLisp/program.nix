# buildLisp.program — compile a Lisp program to an `$out/bin/$name`
# executable via `save-lisp-and-die`. Optionally embeds a Swank REPL
# server (in a background thread, so the user's main owns the main
# thread) and a three-stage sandbox (bwrap mount namespace + seccomp
# stage-1; Landlock + seccomp stage-2 self-applied from Lisp).
#
# Input shape:
#   { name             : String
#   , srcs             : [Path]              ? []
#   , implementation   : Implementation       ? defaultImplementation
#   , brokenOn         : [String]             ? []
#   , main             : String               ? "${name}:main"
#   , deps             : [LispLibrary]        ? []
#   , cLibraries       : [Package]            ? []
#   , runtimeContracts : [NativeRuntimeContract] ? []
#   , tests            : TestSpec | Null      ? null
#   , commandTools     : { name = Package; }  ? {}
#   , passthru         : AttrSet              ? {}
#   , verifyPackages   : [String]             ? []
#   , preDump          : String               ? ""
#   , dynamicSpaceSize : Int | Null           ? null
#   , runtimeAssets    : [Derivation]         ? []
#   , swank            : REPLServerSpec | Null ? null
#   , sandbox          : SandboxProfile | Null ? null
#   }
#
# REPLServerSpec is constructed via `mb.ornaments.replServer.define`;
# SandboxProfile via `mb.ornaments.sandbox.define` (or `profiles.sealed`
# / `profiles.effectful`). Both are typed values — there are no
# `enable` flags; `null` is the off-switch.

# sandboxLib: the in-image SANDBOX package (stage-2 self-application);
# distinct from the per-invocation `sandbox` profile argument.
{ sandboxLib, lib, pkgs, fx, mb, implFilter, allDeps, allNative, testSuite, defaultImplementation, libraryBuilder, swankLib, swankCodegen, validateSpec }:

let
  inherit (pkgs) runCommand makeWrapper writeText;
  inherit (fx.state) forceThunk;

  toolEnvOrn = mb.ornaments.toolEnv;
  runtimeContractOrn = mb.ornaments.runtime-contract;
  sandboxOrn = mb.ornaments.sandbox;
  unwrapDeps = mb.ornaments.dependencies.unwrapDeps;

  inherit (import ./internal/swank.nix { }) codegenConfig;
  inherit (import ./internal/test.nix { inherit testSuite; }) mkTestDrv;
  sandboxDerivedLib = import ./internal/sandbox-derived.nix {
    inherit lib mb sandboxOrn forceThunk;
  };

  # Bwrap arg list with the consumer-policy /nix/store handling layered
  # in. When `storeAccess` is true, the blanket `--ro-bind /nix/store
  # /nix/store` from `toBwrap` covers the closure; when false, that
  # triple is stripped and the launcher script supplies per-path binds
  # via bwrap `--args 4`.
  bwrapBaseArgs = profile:
    let
      raw = sandboxOrn.toBwrap profile;
      stripStore = args:
        let
          go = i:
            if i + 2 >= builtins.length args then [ (builtins.elemAt args i) ]
            else if builtins.elemAt args i == "--ro-bind"
              && builtins.elemAt args (i + 1) == "/nix/store"
              && builtins.elemAt args (i + 2) == "/nix/store"
            then go (i + 3)
            else [ (builtins.elemAt args i) ] ++ go (i + 1);
        in
        if args == [ ] then [ ] else go 0;
    in
    if profile.storeAccess then raw else stripStore raw;

  # `extra.kind` is the swank registration kind label ("daemon"/"script");
  # `style` ditto for the event-loop selector. Both are conventionally
  # carried on the REPLServerSpec's open `extra` attrset.
in

{ name
, srcs ? [ ]
, implementation ? defaultImplementation
, brokenOn ? [ ]
, main ? "${name}:main"
, deps ? [ ]
, cLibraries ? [ ]
, runtimeContracts ? [ ]
, tests ? null
, commandTools ? { }
, passthru ? { }
, verifyPackages ? [ ]
, preDump ? ""
, dynamicSpaceSize ? null
, runtimeAssets ? [ ]
, swank ? null
, sandbox ? null
  # Coordination protocol fields threaded by `implementations.withExtras`.
, __defaultImplName ? null
, __brokenOn ? [ ]
}:

let
  _validated = validateSpec {
    kind = "program";
    inherit name;
    spec = {
      inherit name srcs implementation brokenOn main deps cLibraries tests
        runtimeContracts commandTools passthru verifyPackages preDump dynamicSpaceSize
        runtimeAssets swank sandbox;
    };
  };

  filteredSrcs = implFilter implementation srcs;
  filteredDeps = implFilter implementation (unwrapDeps deps);
  lispDeps = allDeps implementation filteredDeps;
  lispRuntimeContracts = runtimeContractOrn.contractsFor {
    nativeLibraries = cLibraries;
    explicit = runtimeContracts;
    deps = filteredDeps;
  };
  nativeRuntimeLibraries = runtimeContractOrn.nativeLibrariesOf lispRuntimeContracts;
  libPath = lib.makeLibraryPath nativeRuntimeLibraries;
  toolEnv = toolEnvOrn.create commandTools;

  hasSrcs = filteredSrcs != [ ];
  # `swank = null` is absence; `swank.enable = false` is declared-but-
  # inactive. Operational wiring (wrapper lib, effectiveMain, env vars)
  # gates on activation; passthru exposure gates on declaration alone,
  # so inspectors can distinguish "configured but disabled" from
  # "never configured" via `replSpec` presence.
  swankDeclared = swank != null;
  swankEnabled = swankDeclared && swank.enable;
  sandboxEnabled = sandbox != null;

  # Swank wiring: build the codegen-shaped record once; route through
  # `swankCodegen.mkWrapper` to produce the Lisp source loaded into the
  # wrapper library.
  swankCfg = lib.optionalAttrs swankEnabled (codegenConfig swank);
  swankSource = lib.optionalAttrs swankEnabled {
    code = writeText "${name}-repl-wrapper.lisp"
      (swankCodegen.mkWrapper {
        inherit name main sandboxEnabled;
        config = swankCfg;
      });
  };

  swankWrapperLib = lib.optionalAttrs swankEnabled {
    lib = libraryBuilder {
      name = "${name}-repl-wrapper";
      deps = deps ++ [ swankLib ]
        ++ lib.optional (sandboxEnabled && sandboxLib != null) sandboxLib;
      srcs = [ swankSource.code ];
      inherit implementation cLibraries;
    };
  };

  effectiveMain = if swankEnabled then "buildlisp-repl-wrapper:run" else main;

  inherit (sandboxDerivedLib.derive { inherit sandbox sandboxEnabled; })
    landlock sandboxBpfBwrap sandboxBpfSelf sandboxBpfSelfFile
    sandboxRoPaths sandboxRwPaths;
  inherit (sandboxDerivedLib) portList;
  sandboxBwrapArgs = if sandboxEnabled then bwrapBaseArgs sandbox else [ ];

  # Per-path closure binds: when `storeAccess` is false, compute the
  # exact set of `/nix/store` paths the program reaches at runtime via
  # `exportReferencesGraph`. The launcher feeds these to bwrap on fd 4,
  # replacing the blanket store bind.
  runtimeClosureBindArgs =
    if sandboxEnabled && !sandbox.storeAccess then
      let
        runtimeDeps = writeText "${name}-runtime-deps" (
          lib.concatStringsSep "\n" (
            lib.optional hasSrcs "${selfLib}"
            ++ lib.optional swankEnabled "${swankWrapperLib.lib}"
            ++ map (d: "${d}") (unwrapDeps lispDeps)
            ++ map (d: "${d}") nativeRuntimeLibraries
            ++ [ "${sandboxBpfBwrap}" "${sandboxBpfSelf}" ]
            ++ map (d: "${d}") runtimeAssets
            ++ map (d: "${d}") (builtins.attrValues (toolEnvOrn.toolPackages toolEnv))
          )
        );
      in
      runCommand "${name}-closure-binds"
        {
          __structuredAttrs = true;
          exportReferencesGraph.runtime = runtimeDeps;
          nativeBuildInputs = [ pkgs.jq ];
        } ''
        jq -r '.runtime | map(.path) | sort | .[]' "$NIX_ATTRS_JSON_FILE" \
          | while read -r dep; do
              printf '%s\0' --ro-bind "$dep" "$dep"
            done > $out
      ''
    else null;

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
          inherit runtimeContracts;
        }
    else null;

  dumpDeps =
    lib.optional hasSrcs selfLib
    ++ lib.optional swankEnabled swankWrapperLib.lib
    ++ unwrapDeps lispDeps;

  testDrv = mkTestDrv {
    inherit tests name filteredSrcs filteredDeps implementation commandTools;
  };

  sandboxLauncher = if !sandboxEnabled then null else
  writeText "${name}-sandbox-launcher" ''
    #!${pkgs.runtimeShell}
    SELF_DIR="$(cd "$(dirname "$0")" && pwd)"

    # security.lisp queries git for DEPOT_ROOT if unset; setting it here
    # avoids a subprocess inside the sandbox where git is unavailable.
    export DEPOT_ROOT="''${DEPOT_ROOT:-$(pwd)}"

    # Capture parent PID before --unshare-pid hides the host tree.
    PARENT_PID="$PPID"

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

    # Sandbox env vars read by the Lisp-side stage-2 applier.
    export SANDBOX_BPF_SELF="${sandboxBpfSelfFile}"
    export SANDBOX_READ_ONLY_PATHS="${sandboxRoPaths}${lib.optionalString sandbox.sourceAccess ":$SOURCE_ROOT"}"
    export SANDBOX_READ_WRITE_PATHS="${sandboxRwPaths}${lib.optionalString swankEnabled ":$REPL_DIR"}''${SOURCE_WRITE_EXTRA:-}"
    export SANDBOX_ALLOW_EXECVE="${if landlock.allowExecve then "1" else "0"}"
    export SANDBOX_LISTEN_PORTS="${portList landlock.listenPorts}"
    export SANDBOX_CONNECT_PORTS="${portList landlock.connectPorts}"
    export SANDBOX_CONNECT_ANY="${if landlock.connectAny then "1" else "0"}"

    ${lib.optionalString (!sandbox.storeAccess) ''
    # The runtime closure derivation lists transitive deps; the program's
    # own store path is not in it (would be circular), so bind it explicitly.
    SELF_STORE_PATH="$(cd "$SELF_DIR/.." && pwd)"
    ''}

    # shellcheck disable=SC2086
    exec ${pkgs.bubblewrap}/bin/bwrap \
      ${if sandbox.storeAccess then ''
      ${lib.concatStringsSep " \\\n      " sandboxBwrapArgs} \
      '' else ''--ro-bind "$SELF_STORE_PATH" "$SELF_STORE_PATH" \
      --args 4 \
      ${lib.concatStringsSep " \\\n      " sandboxBwrapArgs} \
      ''}${lib.optionalString sandbox.sourceAccess ''--ro-bind "$SOURCE_ROOT" "$SOURCE_ROOT" \
      ''}${lib.optionalString swankEnabled ''--bind "$REPL_DIR" "$REPL_DIR" \
      ''}$SOURCE_WRITE_BINDS \
      --setenv DEPOT_ROOT "$DEPOT_ROOT" \
      --setenv PARENT_PID "$PARENT_PID" \
      --setenv SANDBOX_BPF_SELF "${sandboxBpfSelfFile}" \
      --setenv SANDBOX_READ_ONLY_PATHS "${sandboxRoPaths}${lib.optionalString sandbox.sourceAccess ":$SOURCE_ROOT"}" \
      --setenv SANDBOX_READ_WRITE_PATHS "${sandboxRwPaths}${lib.optionalString swankEnabled ":$REPL_DIR"}''${SOURCE_WRITE_EXTRA:-}" \
      --setenv SANDBOX_ALLOW_EXECVE "${if landlock.allowExecve then "1" else "0"}" \
      --setenv SANDBOX_LISTEN_PORTS "${portList landlock.listenPorts}" \
      --setenv SANDBOX_CONNECT_PORTS "${portList landlock.connectPorts}" \
      --setenv SANDBOX_CONNECT_ANY "${if landlock.connectAny then "1" else "0"}" \
      --setenv LD_LIBRARY_PATH "${pkgs.stdenv.cc.libc}/lib${lib.optionalString (libPath != "") ":${libPath}"}" \
      --seccomp 3 \
      -- "$SELF_DIR/.${name}-unwrapped" "$@" \
      3< ${sandboxBpfBwrap}/${sandboxBpfBwrap.name}.bpf${lib.optionalString (!sandbox.storeAccess) '' \
      4< ${runtimeClosureBindArgs}''}
  '';

in
builtins.seq _validated (lib.fix (self: runCommand name
  ({
    nativeBuildInputs = lib.unique ([ makeWrapper ] ++ toolEnvOrn.toolInputs toolEnv);
    LD_LIBRARY_PATH = libPath;
    LANG = "C.UTF-8";
  } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
    # dyld ignores LD_LIBRARY_PATH; expose native libs to it on macOS so
    # bare-soname loads resolve when dumping the image.
    DYLD_LIBRARY_PATH = libPath;
  } // lib.optionalAttrs (dynamicSpaceSize != null) {
    NIX_BUILDLISP_LISP_ARGS = "--dynamic-space-size ${toString dynamicSpaceSize}";
  } // {
    passthru = passthru // {
      lispName = name;
      lispDeps = if hasSrcs then [ selfLib ] else unwrapDeps lispDeps;
      lispNativeDeps = nativeRuntimeLibraries;
      inherit lispRuntimeContracts;
      lispBinary = true;
      tests = testDrv;
      inherit brokenOn commandTools;
      toolPackages = toolEnvOrn.toolPackages toolEnv;
      lib = selfLib;
      inherit self;
    } // lib.optionalAttrs swankDeclared {
      replSpec = swank;
    } // lib.optionalAttrs sandboxEnabled {
      sandboxProfile = sandbox;
      inherit sandboxBpfBwrap sandboxBpfSelf;
    };
  })
  (''
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

    ${implementation.runScript} ${
      implementation.genDumpLisp {
        inherit name verifyPackages preDump;
        main = effectiveMain;
        deps = dumpDeps;
      }
    }
  '' + lib.optionalString implementation.wrapProgram (
    if sandboxEnabled then ''
      # Rename the bare binary; the bwrap launcher takes its place and
      # invokes the unwrapped binary inside the sandbox.
      mv $out/bin/${name} $out/bin/.${name}-unwrapped

      wrapProgram $out/bin/.${name}-unwrapped \
        --prefix LD_LIBRARY_PATH : "${libPath}" \
        ${toolEnvOrn.toWrapPrefix toolEnv}--add-flags "$NIX_BUILDLISP_LISP_ARGS --"

      cp ${sandboxLauncher} $out/bin/${name}
      chmod +x $out/bin/${name}
    '' else ''
      wrapProgram $out/bin/${name} \
        --prefix LD_LIBRARY_PATH : "${libPath}" \
        ${toolEnvOrn.toWrapPrefix toolEnv}--add-flags "$NIX_BUILDLISP_LISP_ARGS --"
    ''
  ))))
