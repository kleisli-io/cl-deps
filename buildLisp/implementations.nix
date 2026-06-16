# Aggregates per-impl modules for buildLisp.

{ mb
, lib
, writeText
, writeShellScriptBin
, runCommand
, sbcl
, ecl
, ccl
, rlwrap
, writers
, targetPlatform
, allDeps
, allNative
, baseToolEnv
}:

let
  utils = import ./impl/utils.nix { inherit lib writeText; };

  genLoadLispGeneric = utils.genLoadLispGeneric allDeps;
  genReloadLispGeneric = utils.genReloadLispGeneric;
  genTestLispGeneric = utils.genTestLispGeneric;

  sbclImpl = import ./impl/sbcl.nix {
    inherit mb lib sbcl writeText writeShellScriptBin rlwrap;
    inherit genLoadLispGeneric genReloadLispGeneric genTestLispGeneric allDeps allNative baseToolEnv;
    disableDebugger = utils.disableDebugger;
  };

  eclImpl = import ./impl/ecl.nix {
    inherit mb lib ecl writeText writeShellScriptBin runCommand;
    inherit genLoadLispGeneric genReloadLispGeneric genTestLispGeneric allDeps allNative baseToolEnv;
    disableDebugger = utils.disableDebugger;
  };

  cclImpl = import ./impl/ccl.nix {
    inherit mb lib ccl writeText writeShellScriptBin rlwrap targetPlatform;
    inherit genLoadLispGeneric genReloadLispGeneric genTestLispGeneric allDeps allNative baseToolEnv;
    disableDebugger = utils.disableDebugger;
    writeBash = writers.writeBash;
  };

in
lib.mapAttrs (name: v: { inherit name; } // v) {
  sbcl = sbclImpl.self;
  ecl = eclImpl.self;
  ccl = cclImpl.self;
}
