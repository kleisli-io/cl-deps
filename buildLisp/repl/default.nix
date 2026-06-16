# buildLisp.repl — REPL-server code generation. `swankCodegen.mkWrapper`
# emits the Lisp source loaded by program/script/daemon wrappers to
# embed Swank with the protocol/mode/registration/style projected from
# a typed `REPLServerSpec` via the consumer's `codegenConfig`.

{ world, lib, pkgs, ... }:

{
  swankCodegen = import ./swank-codegen.nix { inherit lib; };
}
