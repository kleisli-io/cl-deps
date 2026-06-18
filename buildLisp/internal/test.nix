# Shared test-derivation construction for buildLisp builders.
#
# `mkTestDrv` resolves an optional `tests : TestSpec | Null` against the
# impl-filtered srcs/deps the calling builder has already prepared.
# Returns `null` when `tests` is `null`; otherwise returns the
# `testSuite` derivation.
#
# The post-test SHELL branch is intentionally not shared:
#   - `library.nix` emits "Test ... succeeded" / "No tests run"
#     (informational; build succeeds regardless).
#   - `program.nix` / `script.nix` emit "Tests passed" / "Error: Tests
#     must pass" (fatal-on-missing).
# These two semantics serve different audiences and a forced
# discriminator would obscure intent.

{ testSuite }:

let
  mkTestDrv =
    { tests, name, filteredSrcs, filteredDeps, implementation, commandTools }:
    if tests == null then null
    else
      testSuite {
        name = tests.name or "${name}-test";
        srcs = filteredSrcs ++ (tests.srcs or [ ]);
        deps = filteredDeps ++ (tests.deps or [ ]);
        expression = tests.expression;
        inherit implementation;
        commandTools = commandTools // (tests.commandTools or { });
      };
in
{
  inherit mkTestDrv;
}
