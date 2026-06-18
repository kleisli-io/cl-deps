# buildLisp — Common Lisp package builder over metaBuilder.
#
# Public surface: library / program / daemon / script / bundled / grovel
# / repl / serviceSpec, plus per-impl entry points (sbcl / ecl / ccl /
# sbclWith) and the orn coordination layer (toolEnv / implementations /
# extend / withTools).

{ pkgs, lib, mb, fx, swankLib, lisp, sandbox ? null, buildLisp, ... }:

let
  inherit (pkgs) runCommand writeText writeShellScriptBin sbcl ecl ccl rlwrap writers;
  inherit (pkgs.stdenv) targetPlatform;
  isDarwin = pkgs.stdenv.hostPlatform.isDarwin;

  toolEnvOrn = mb.ornaments.toolEnv;
  implsOrn = mb.ornaments.implementations;
  depsOrn = mb.ornaments.dependencies;

  # Swank codegen lives in this tree's repl/ (no `meta.*` deps; pure
  # Lisp-code generator over the typed `codegenConfig` projection). The
  # swank library itself is supplied as an argument.
  swankCodegen = (import ./repl { inherit lib; }).swankCodegen;

  implFilter = implsOrn.filterByName;

  # Resolve each dep to its impl-specific variant, then run the orn
  # uniform resolver. Returns a marked record so downstream builders
  # skip redundant re-resolution.
  allDeps = impl: deps:
    let
      deps' = map
        (dep:
          dep."${impl.name}" or
            (if dep ? overrideLisp
            then dep.overrideLisp (_: { implementation = impl; })
            else dep))
        deps;
    in
    depsOrn.markResolved (depsOrn.resolveUniform {
      langName = "lisp";
      deps = deps';
    });

  allNative = native: deps: lib.unique (
    lib.flatten (native ++ (map (d: d.lispNativeDeps or [ ]) deps))
  );

  makeOverridable = f: orig: (f orig) // {
    overrideLisp = new: makeOverridable f (orig // (new orig));
  };

  descriptions = import ./descriptions.nix { inherit lib mb fx; };
  validateSpec = descriptions.validate;

  mkBuildLisp = { baseToolEnv }:
    let
      impls = import ./implementations.nix {
        inherit mb lib writeText writeShellScriptBin runCommand sbcl ecl ccl rlwrap writers targetPlatform;
        inherit allDeps allNative baseToolEnv;
      };

      defaultImplementation = impls.sbcl;

      testSuite = import ./test-suite.nix {
        inherit lib runCommand mb implFilter allDeps allNative isDarwin;
      };

      libraryBuilder = import ./library.nix {
        inherit lib runCommand writeText mb implFilter allDeps allNative testSuite defaultImplementation validateSpec isDarwin;
      };

      programBuilder = import ./program.nix {
        inherit lib pkgs fx mb sandbox implFilter allDeps allNative
          testSuite defaultImplementation libraryBuilder
          swankLib swankCodegen validateSpec;
      };

      scriptBuilder = import ./script.nix {
        inherit lib pkgs fx mb implFilter allDeps allNative
          testSuite defaultImplementation libraryBuilder
          swankLib swankCodegen validateSpec;
      };

      daemonBuilder = import ./daemon.nix {
        inherit lib pkgs mb sandbox libraryBuilder swankLib swankCodegen
          defaultImplementation validateSpec;
        programBuilder = programBuilder;
      };

      # Expose an implementation-provided module (e.g. uiop, sb-posix) as
      # a library. Impls may override via `implementation.bundled` (ECL
      # does); the default wraps `(require '<name>)` in a one-file library.
      bundled = name:
        let
          defaultBundled = implementation: name': libraryBuilder {
            name = name';
            inherit implementation;
            srcs = lib.singleton (builtins.toFile "${name'}.lisp" "(require '${name'})");
          };
          bundled' = { implementation ? defaultImplementation, name }:
            implementation.bundled or (defaultBundled implementation) name;
        in
        (makeOverridable bundled') { inherit name; };

      implsSystem = implsOrn.defineSystem {
        langName = "lisp";
        implementations = {
          sbcl = impls.sbcl // { replWith = impls.sbcl.lispWith; };
          ecl = impls.ecl // { replWith = impls.ecl.lispWith; };
          ccl = impls.ccl // { replWith = impls.ccl.lispWith; };
        };
        defaultImpl = "sbcl";
        inherit makeOverridable;
        allowUserExtensions = true;
      };

      createPublicAPI = currentImplsSystem: {
        library = currentImplsSystem.withExtras libraryBuilder;
        program = currentImplsSystem.withExtras programBuilder;
        script = currentImplsSystem.withExtras scriptBuilder;
        daemon = currentImplsSystem.withExtras daemonBuilder;
        inherit bundled;
        sbclWith = impls.sbcl.lispWith;
        inherit (impls) sbcl ecl ccl;
      };

    in
    implsOrn.makeBuilder {
      inherit implsSystem baseToolEnv createPublicAPI;
    };

in
mkBuildLisp { baseToolEnv = toolEnvOrn.empty; } // {
  grovel = import ./grovel.nix { inherit pkgs lib mb lisp buildLisp; };
  repl = import ./repl { inherit lib; };
  serviceSpec = import ./service-spec.nix { inherit lib; };
  mkRelocatableBundle = import ./relocatable-bundle.nix { inherit pkgs lib; validateSpec = descriptions.validate; };
  inherit descriptions;
  tests = import ./tests { inherit lib pkgs mb buildLisp; };
}
