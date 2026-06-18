# buildLisp.daemon — long-running Lisp service with optional embedded
# Swank REPL. Built on top of `program` by wrapping the user's main
# behind a foreground keep-alive thread + background worker thread
# pattern:
#
#   Main thread:    Swank manages worker threads (`style = :spawn`);
#                   main thread holds the process alive with a sleep
#                   loop. Interactive debugging hangs off this thread.
#   Worker thread:  user's main runs here; can block, loop, or return.
#                   Errors are logged to stderr but do not crash the
#                   daemon.
#
# Type discipline:
#   - `swank.mode` MUST be `Foreground` (the daemon pattern's defining
#     property). `Background` mode is a category error — use `program`
#     or `script` for that.
#   - `swank.lifecycle` MUST be `LongRunning`. `OneShot` is a category
#     error — use `script` for one-shot REPL launches.
#
# `swank` defaults to `null` (no REPL); pass an explicit typed spec to
# embed one. Even without swank, the wrapper preserves the
# foreground/worker structure for serviceSpec env-var/asset wiring and
# the `isDaemon` passthru marker.
#
# Input shape:
#   { name             : String
#   , srcs             : [Path]                  ? []
#   , main             : String
#   , deps             : [LispLibrary]           ? []
#   , cLibraries       : [Package]               ? []
#   , tests            : TestSpec | Null         ? null
#   , commandTools     : { name = Package; }     ? {}
#   , passthru         : AttrSet                 ? {}
#   , runtimeAssets    : [Derivation]            ? []
#   , swank            : REPLServerSpec | Null   ? null
#   , serviceSpec      : ServiceSpec | Null      ? null
#   , sandbox          : SandboxProfile | Null   ? null
#   , implementation   : Implementation          ? defaultImplementation
#   , brokenOn         : [String]                ? []
#   , dynamicSpaceSize : Int                     ? 2048
#   , preDump          : String                  ? ""
#   , verifyPackages   : [String]                ? []
#   }
#
# Canonical opt-in for embedded Swank:
#   swank = mb.ornaments.replServer.define {
#     protocol = mb.ornaments.replServer.protocols.swank;
#     mode = mb.ornaments.replServer.Mode.Foreground;
#   };

{ sandbox, lib, pkgs, mb, programBuilder, libraryBuilder, swankLib, swankCodegen, defaultImplementation, validateSpec }:

let
  replOrn = mb.ornaments.replServer;
  sandboxOrn = mb.ornaments.sandbox;
  serviceSpecLib = import ./service-spec.nix { inherit lib; };

  inherit (import ./internal/swank.nix { }) codegenConfig;
in

{ name
, srcs ? [ ]
, main
, deps ? [ ]
, cLibraries ? [ ]
, tests ? null
, commandTools ? { }
, passthru ? { }
, runtimeAssets ? [ ]
, swank ? null
, serviceSpec ? null
, sandbox ? null
, implementation ? defaultImplementation
, brokenOn ? [ ]
, dynamicSpaceSize ? 2048
, preDump ? ""
, verifyPackages ? [ ]
, __defaultImplName ? null
, __brokenOn ? [ ]
}:

let
  _validated = validateSpec {
    kind = "daemon";
    inherit name;
    spec = {
      inherit name srcs implementation brokenOn main deps cLibraries tests
        commandTools passthru verifyPackages preDump dynamicSpaceSize
        runtimeAssets swank sandbox serviceSpec;
    };
  };

  swankDeclared = swank != null;
  swankEnabled = swankDeclared && swank.enable;
  sandboxEnabled = sandbox != null;

  swankCfg = if !swankEnabled then null else codegenConfig swank;

  daemonWrapperCode = pkgs.writeText "${name}-daemon-wrapper.lisp" (
    if swankEnabled then
      swankCodegen.mkWrapper { inherit name main sandboxEnabled; config = swankCfg; }
    else
    # Minimal wrapper preserves the package + entry-point contract
    # (`buildlisp-repl-wrapper:run`) so the programBuilder call below
    # is identical regardless of swank state.
      ''
        (defpackage :buildlisp-repl-wrapper
          (:use :cl)
          (:export :run))

        (in-package :buildlisp-repl-wrapper)

        (defun run ()
          (let* ((main-spec "${main}")
                 (colon-pos (position #\: main-spec :from-end t))
                 (pkg-name (string-upcase (subseq main-spec 0 (position #\: main-spec))))
                 (fn-name (string-upcase (subseq main-spec (1+ colon-pos))))
                 (main-fn (find-symbol fn-name pkg-name)))
            (if main-fn
                (funcall main-fn)
                (error "Could not find main function: ~a" main-spec))))
      ''
  );

  wrapperDeps =
    deps
    ++ lib.optional swankEnabled swankLib
    ++ lib.optional sandboxEnabled sandbox;

  daemonWrapperLib = libraryBuilder {
    name = "${name}-repl-wrapper";
    deps = wrapperDeps;
    srcs = [ daemonWrapperCode ];
    inherit implementation cLibraries;
  };

  # When swank embeds, the daemon's port appears as a default env var
  # on the normalized service spec so downstream systemd unit
  # generation has a single source of truth for the port.
  normalizedSpec =
    if serviceSpec == null then null
    else
      let
        base = serviceSpecLib.mkServiceSpec serviceSpec;
        replEnv = lib.optionalAttrs swankEnabled {
          ${swank.portEnvVar} = {
            default = toString swank.port;
            description = "REPL server port";
          };
        };
      in
      base // {
        env = replEnv // base.env;
      };

  assetPaths =
    if serviceSpec != null && serviceSpec ? assets
    then builtins.attrValues serviceSpec.assets
    else [ ];
in

# Structural swank.mode/lifecycle enforcement lives in `validateSpec`'s
  # daemon branch (descriptions.nix:daemonReplCheck) — kernel-validated
  # before any of the let-bindings above are forced.
builtins.seq _validated (
  programBuilder {
    inherit name implementation tests sandbox cLibraries srcs brokenOn
      dynamicSpaceSize preDump verifyPackages commandTools;
    runtimeAssets = assetPaths ++ runtimeAssets;
    deps = [ daemonWrapperLib ] ++ wrapperDeps;
    main = "buildlisp-repl-wrapper:run";
    # daemon owns the wrapper; programBuilder must not also wrap.
    swank = null;
    passthru = passthru // {
      isDaemon = true;
      originalMain = main;
    } // lib.optionalAttrs swankDeclared {
      replSpec = swank;
    } // lib.optionalAttrs (normalizedSpec != null) {
      serviceSpec = normalizedSpec;
    } // lib.optionalAttrs sandboxEnabled {
      systemdHardening = sandboxOrn.toSystemd sandbox;
    };
  })
