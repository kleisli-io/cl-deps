# buildLisp.library — compile a Lisp library to a per-impl FASL bundle.
#
# Output is a `runCommand` derivation whose `passthru` carries the
# fields downstream impl modules and consumers consume directly:
# `lispName`, `lispDeps` (unwrapped), `lispNativeDeps`, `lispSrcs`,
# `lispBinary = false`, plus `reloadScript`, optional `tests`,
# `toolPackages`, `commandTools`, `brokenOn`, and optional `replInit`.
#
# Input shape:
#   { name           : String
#   , srcs           : [Path]
#   , implementation : Implementation ? defaultImplementation
#   , brokenOn       : [String]       ? []
#   , deps           : [LispLibrary]  ? []
#   , cLibraries     : [Package]      ? []
#   , tests          : TestSpec | Null ? null
#   , commandTools   : { name = Package; } ? {}
#   , passthru       : AttrSet        ? {}
#   , replInit       : String | Null  ? null
#   , muffle         : [String]       ? []   # condition types to muffle
#   }
#
# TestSpec ≡ { name? ; srcs? ; deps? ; expression ; commandTools? }.

{ lib, runCommand, writeText, mb, implFilter, allDeps, allNative, testSuite, defaultImplementation, validateSpec, isDarwin }:

let
  toolEnvOrn = mb.ornaments.toolEnv;
  unwrapDeps = mb.ornaments.dependencies.unwrapDeps;
  inherit (import ./internal/test.nix { inherit testSuite; }) mkTestDrv;
in

{ name
, srcs
, implementation ? defaultImplementation
, brokenOn ? [ ]
, deps ? [ ]
, cLibraries ? [ ]
, tests ? null
, commandTools ? { }
, passthru ? { }
, replInit ? null
, muffle ? [ ]
  # Resource directories shipped with the library and located at runtime.
  # Attrset of author-chosen key string -> directory path. Each becomes a
  # `register-resource-root` form compiled into the FASL; library code
  # reaches files with `(buildlisp/resources:resource-path key relative)`.
, resources ? { }
  # Coordination protocol fields: `implementations.withExtras` threads
  # these back into the builder on rebuild so per-impl variants and CI
  # targets resolve correctly. Accepted but not user-facing.
, __defaultImplName ? null
, __brokenOn ? [ ]
}:

let
  _validated = validateSpec {
    kind = "library";
    inherit name;
    spec = {
      inherit name srcs implementation brokenOn deps cLibraries tests
        commandTools passthru replInit muffle resources;
    };
  };

  # A resources-bearing library prepends the runtime support package plus
  # a generated registration form. The interpolated store paths land in the
  # FASL as pathname literals, so Nix retains the resource dirs in the
  # library's runtime closure.
  resourceSrcs =
    if resources == { } then [ ]
    else [
      ./runtime/resources.lisp
      (writeText "${name}-resource-roots.lisp"
        (lib.concatStrings (lib.mapAttrsToList
          (key: dir: ''(buildlisp/resources:register-resource-root "${key}" #p"${dir}/")
'')
          resources)))
    ];

  filteredDeps = implFilter implementation (unwrapDeps deps);
  filteredSrcs = implFilter implementation (resourceSrcs ++ srcs);
  lispDeps = allDeps implementation filteredDeps;
  lispNativeDeps = allNative cLibraries filteredDeps;
  toolEnv = toolEnvOrn.create commandTools;

  testDrv = mkTestDrv {
    inherit tests name filteredSrcs filteredDeps implementation commandTools;
  };
in
builtins.seq _validated (lib.fix (self: runCommand "${name}-cllib"
({
  nativeBuildInputs = toolEnvOrn.toolInputs toolEnv;
  LD_LIBRARY_PATH = lib.makeLibraryPath lispNativeDeps;
  LANG = "C.UTF-8";
  passthru = passthru // {
    inherit lispNativeDeps brokenOn commandTools;
    lispDeps = unwrapDeps lispDeps;
    lispName = name;
    lispBinary = false;
    lispSrcs = filteredSrcs;
    lispResources = resources;
    reloadScript = "${self}/reload.lisp";
    tests = testDrv;
    toolPackages = toolEnvOrn.toolPackages toolEnv;
    inherit self;
  } // lib.optionalAttrs (replInit != null) {
    inherit replInit;
  };
}
  # dyld ignores LD_LIBRARY_PATH; expose native libs to it on macOS so
  # bare-soname loads (e.g. cl+ssl's libcrypto.3.dylib) resolve at build.
  // lib.optionalAttrs isDarwin {
    DYLD_LIBRARY_PATH = lib.makeLibraryPath lispNativeDeps;
  }) ''
    ${toolEnvOrn.toExportSnippet toolEnv}
    ${if testDrv != null
      then "echo 'Test ${testDrv} succeeded'"
      else "echo 'No tests run'"}

    mkdir $out

    ${lib.optionalString (lispNativeDeps != []) ''
      # Persist the library path so sandboxed runtime lookups (which
      # cannot evaluate Nix) can find native deps.
      echo -n "${lib.makeLibraryPath lispNativeDeps}" > $out/native-lib-path
    ''}

    # Hot-reload script. Part 1 (single-quoted heredoc) keeps Nix
    # interpolation; part 2 (bash-interpolated heredoc) defers $out
    # expansion to runtime so the self-reference resolves to the realised
    # store path.
    cat > $out/reload.lisp << 'DEPS_EOF'
  ${implementation.genReloadLisp (unwrapDeps lispDeps)}
  DEPS_EOF
    ${lib.optionalString (filteredSrcs != []) ''
    cat >> $out/reload.lisp << SELF_EOF

  ;; Load compiled library FASL
  (load "$out/${name}.${implementation.faslExt}")
  SELF_EOF
    ''}

    ${if filteredSrcs == []
      then ''
        # Meta-package: srcs is empty, deps already produced their FASLs.
        echo "Meta-package ${name}: deps only"
      ''
      else ''
        ${implementation.runScript} ${
          implementation.genCompileLisp {
            srcs = filteredSrcs;
            inherit name muffle;
            deps = lispDeps;
          }
        }
      ''
    }
''))
