# Shared swank-codegen plumbing for the buildLispOrn builders.
#
# `codegenConfig` projects a typed `REPLServerSpec` (kernel
# `mb.descriptions.REPLServerSpec`) into the string-keyed record that
# `swankCodegen.mkWrapper` consumes. The closed-sum tags (`Mode`,
# `Registration`) map to the codegen's string keys; `extra.style` is
# the swank-specific event-loop selector (`:spawn` / `:fd-handler`)
# carried in the spec's open `extra` attrset.

{}:

let
  codegenConfig = spec: {
    inherit (spec) protocol port portEnvVar interface shortLivedFlags extra;
    mode = if spec.mode._con == "Foreground" then "foreground" else "background";
    registration =
      if spec.registration._con == "XDG" then "xdg"
      else if spec.registration._con == "Global" then "global"
      else if spec.registration._con == "ServiceSpec" then "serviceSpec"
      else "none";
    style = spec.extra.style or ":spawn";
  };
in
{
  inherit codegenConfig;
}
