# service-spec.nix — normalizer and projection for service specifications.
#
# A `serviceSpec` is a workload-agnostic description of what a service
# needs at runtime: environment variables (with defaults), asset paths
# baked into the closure, required services, health checks, and
# secrets. Daemons consume it for env-var defaults + asset closure
# inclusion; downstream NixOS modules project it into systemd unit
# `Environment=` / `EnvironmentFile=` lines.
#
# Usage:
#   let spec = mkServiceSpec {
#     env = {
#       MY_PORT = { default = "3000"; description = "Web port"; };
#       MY_HOST = { default = "localhost"; };
#     };
#     assets = { STATIC_ROOT = staticDir; };
#   };
#   env = specToDefaultEnv spec;
#   # => { MY_PORT = "3000"; MY_HOST = "localhost"; STATIC_ROOT = "/nix/store/..."; }

{ lib }:

let
  normalizeEnvEntry = _name: value:
    {
      default = value.default or "";
      description = value.description or "";
    } // (lib.optionalAttrs (value ? required) { inherit (value) required; });

  normalizeSecretEntry = name: value:
    {
      envVar = value.envVar or "";
      sopsKey = value.sopsKey or name;
    };

  mkServiceSpec = spec: {
    env = lib.mapAttrs normalizeEnvEntry (spec.env or { });
    assets = spec.assets or { };
    requires = spec.requires or { };
    healthCheck = spec.healthCheck or { };
    secrets = lib.mapAttrs normalizeSecretEntry (spec.secrets or { });
  };

  # Asset values are interpolated as `"${v}"` (not `toString v`) so the
  # store path is recorded as a runtime dependency on the consuming
  # derivation. `toString` on a raw path would emit the path string
  # without tracking, leaving the env var dangling after GC.
  specToDefaultEnv = spec:
    let
      envDefaults = lib.mapAttrs (_: v: v.default) spec.env;
      assetPaths = lib.mapAttrs (_: v: "${v}") spec.assets;
    in
    envDefaults // assetPaths;

in
{
  inherit mkServiceSpec specToDefaultEnv;
}
