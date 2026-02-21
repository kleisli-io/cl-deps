# cl-deps

Nix-native Common Lisp dependency set. Packages ~64 CL libraries for SBCL and provides `buildLisp`, a derivation-based builder for libraries and programs.

## What it does

- **`buildLisp.library`** compiles Lisp sources into a FASL derivation with transitive dependency resolution
- **`buildLisp.program`** produces a standalone SBCL executable with an embedded core
- **`buildLisp.bundled`** exposes SBCL contrib modules (uiop, sb-posix, etc.)
- **`lisp.*`** has ~64 library definitions ready to use: alexandria, hunchentoot, clack/lack, ironclad, cl-json, yason, parenscript, swank, and more

## Usage

Add as a flake input:

```nix
{
  inputs.cl-deps.url = "github:kleisli-io/cl-deps";

  outputs = { cl-deps, ... }:
    let
      inherit (cl-deps.lib.x86_64-linux) buildLisp lisp;
    in {
      packages.default = buildLisp.library {
        name = "my-lib";
        deps = [ lisp.alexandria lisp.cl-ppcre ];
        srcs = [ ./src/package.lisp ./src/my-lib.lisp ];
      };
    };
}
```

## Adding a library

Define it in `default.nix` at the appropriate tier (leaf deps first, deeper deps later). Each library needs a `name`, `srcs` list, and optional `deps`:

```nix
my-lib = let src = pkgs.fetchFromGitHub { ... }; in
  buildLisp.library {
    name = "my-lib";
    deps = [ alexandria ];
    srcs = [ "${src}/src/package.lisp" "${src}/src/main.lisp" ];
  };
```

Libraries with native C dependencies use the `native` attribute to add shared libraries to the runtime path.

## License

[MIT](LICENSE)
