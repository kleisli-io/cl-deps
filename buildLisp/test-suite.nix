# buildLisp.test-suite — load srcs+deps under `implementation`, evaluate
# `expression`, fail the build if it returns NIL.
#
# Input shape:
#   { name           : String
#   , expression     : String           # Lisp form, success ≡ non-NIL
#   , srcs           : [Path]
#   , implementation : Implementation
#   , deps           : [LispLibrary]   ? []
#   , cLibraries     : [Package]       ? []
#   , commandTools   : { name = Package; } ? {}
#   }

{ lib, runCommand, mb, implFilter, allDeps, allNative, isDarwin }:

let
  toolEnvOrn = mb.ornaments.toolEnv;
  unwrapDeps = mb.ornaments.dependencies.unwrapDeps;
in

{ name
, expression
, srcs
, implementation
, deps ? [ ]
, cLibraries ? [ ]
, commandTools ? { }
}:

let
  filteredSrcs = implFilter implementation srcs;
  filteredDeps = implFilter implementation (unwrapDeps deps);
  lispDeps = allDeps implementation filteredDeps;
  lispNativeDeps = allNative cLibraries (unwrapDeps lispDeps);
  toolEnv = toolEnvOrn.create commandTools;
in
runCommand name
({
  nativeBuildInputs = toolEnvOrn.toolInputs toolEnv;
  LD_LIBRARY_PATH = lib.makeLibraryPath lispNativeDeps;
  LANG = "C.UTF-8";
} // lib.optionalAttrs isDarwin {
  # dyld ignores LD_LIBRARY_PATH; expose native libs to it on macOS so
  # bare-soname loads resolve while compiling the test image.
  DYLD_LIBRARY_PATH = lib.makeLibraryPath lispNativeDeps;
}) ''
  ${toolEnvOrn.toExportSnippet toolEnv}
  echo "Running test suite ${name}"

  ${implementation.runScript} ${
    implementation.genTestLisp {
      inherit name expression;
      srcs = filteredSrcs;
      deps = lispDeps;
    }
  } | tee $out

  echo "Test suite ${name} succeeded"
''
