# buildLisp.grovel — cffi-grovel code generator.
#
# Builds a typed code-gen spec for one cffi-grovel invocation and
# materialises it into a `pkgs.runCommand` derivation. The result
# carries the materializer's full `{ plan; value; derivation }` record
# plus a lazy `files : [Path]` projection of the generated .lisp sources
# under `$out/lisp/`.
#
# Input shape:
#   { name        : String
#   , grovelFile  : Path
#   , preload     : [Path]        ? []   # lisp sources --load'd first
#   , cLibraries  : [Package]     ? []   # C libs (include/ + so/)
#   , deps        : [LispPackage] ? []   # extra packages in the grovel SBCL
#   }

{ lib, pkgs, mb, lisp, buildLisp, ... }:

let
  cgOrn = mb.ornaments."code-gen";
  ops = mb.operations;

  # cffi-grovel resolves its common.h via the ASDF source registry.
  cffiSrc = pkgs.srcOnly pkgs.sbcl.pkgs.cffi;
  grovelHeaderDir = "${cffiSrc}/grovel";

  # Per-language output subdirectory under $out. The typed `Output`
  # declaration we attach to the spec mirrors this exact path so the
  # plan's `outputs` field stays self-consistent with the filesystem.
  langSubdir = "lisp";

  # cffi-grovel runs under SBCL and shells out to `gcc` + `pkg-config`,
  # so those become `tool` ops (→ nativeBuildInputs / $PATH). C
  # libraries become `dependency` ops (→ LD_LIBRARY_PATH); their
  # include/ directories surface through C_INCLUDE_PATH.
  mkGrovelSpec =
    { name
    , grovelFile
    , preload ? [ ]
    , cLibraries ? [ ]
    , deps ? [ ]
    }:
    let
      sbclWithGrovel = buildLisp.sbclWith ([ lisp.cffi-grovel ] ++ deps);

      includePaths = map (p: "${p}/include") cLibraries ++ [ grovelHeaderDir ];

      sbclTool = ops.tool { name = "sbcl"; package = sbclWithGrovel; };
      gccTool = ops.tool { name = "gcc"; package = pkgs.gcc; };
      pkgConfigTool = ops.tool { name = "pkg-config"; package = pkgs.pkg-config; };

      libDeps = map
        (p: ops.dependency {
          name = p.pname or p.name or "c-library";
          role = "native";
          package = p;
        })
        cLibraries;

      output = ops.output {
        name = langSubdir;
        path = "$out/${langSubdir}";
        format = "tree";
      };

      runOp = ops.runTool {
        name = "${name}-invoke";
        tool = sbclTool;
        args = [
          "--non-interactive"
        ] ++ lib.concatMap (p: [ "--load" "${p}" ]) preload ++ [
          "--eval"
          ''(cffi-grovel:process-grovel-file "${grovelFile}" (uiop:getenv "OUTPUT_FILE"))''
        ];
        env = {
          CL_SOURCE_REGISTRY = "${cffiSrc}//";
          C_INCLUDE_PATH = lib.concatStringsSep ":" includePaths;
          OUTPUT_FILE = "$out/${langSubdir}/${name}.lisp";
        };
      };

      operations =
        (map (d: ops.resolveDependency { dependency = d; }) libDeps)
        ++ [
          (ops.declareTool { tool = sbclTool; })
          (ops.declareTool { tool = gccTool; })
          (ops.declareTool { tool = pkgConfigTool; })
          runOp
          (ops.transformOutput { inherit output; })
          (ops.emitDescriptor {
            descriptor = ops.descriptor {
              name = "${name}-codegen";
              payload = {
                kind = "codegen";
                generator = "cffi-grovel";
                languages = [ langSubdir ];
              };
            };
          })
          (ops.materializeDerivation {
            name = "${name}-generated";
            builder = "runCommand";
          })
        ];

    in
    {
      _con = "MetaBuilderSpec";
      inherit name operations;
      generator = "cffi-grovel";
      languages = [ langSubdir ];
      parameters = [ ];
      inputs = [ ];
      dependencies = libDeps;
      tools = [ sbclTool gccTool pkgConfigTool ];
      outputs = [ output ];
      evidence = [ ];
    };

in
input:
let
  materialised = cgOrn.materialize (mkGrovelSpec input);
  drv = materialised.derivation;
  # Lazy projection: forces realisation of `drv` when accessed.
  files =
    let langDir = drv + "/${langSubdir}";
    in map (f: langDir + "/${f}")
      (builtins.attrNames (builtins.readDir langDir));
in
materialised // { inherit files; }
