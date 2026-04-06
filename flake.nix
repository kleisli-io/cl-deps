# cl-deps — Canonical Common Lisp dependency set
#
# Provides buildLisp + ~64 library definitions for SBCL.
# Consumed as a flake input by kli and other CL projects.
{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/88d3861acdd3d2f0e361767018218e51810df8a1";

  outputs = { nixpkgs, ... }:
    let
      forAllSystems = nixpkgs.lib.genAttrs [
        "x86_64-linux" "aarch64-linux"
        "x86_64-darwin" "aarch64-darwin"
      ];
    in {
      lib = forAllSystems (system:
        let pkgs = nixpkgs.legacyPackages.${system};
        in import ./default.nix { inherit pkgs; lib = pkgs.lib; });
    };
}
