# buildLisp — SBCL-only Common Lisp builder for kli
#
# Supports: library, program, bundled, test-suite
#
# Usage:
#   buildLisp.library { name = "my-lib"; srcs = [...]; deps = [...]; }
#   buildLisp.program { name = "my-prog"; srcs = [...]; main = "my-prog:main"; deps = [...]; }
#   buildLisp.bundled "uiop"

{ pkgs, lib }:

let
  inherit (pkgs) runCommand writeText makeWrapper sbcl;

  faslExt = "fasl";

  # macOS uses DYLD_LIBRARY_PATH; Linux uses LD_LIBRARY_PATH
  libPathVar = if pkgs.stdenv.isDarwin then "DYLD_LIBRARY_PATH" else "LD_LIBRARY_PATH";

  # --- Dependency resolution ---

  # Extract identity string from a package for memoization
  identity = pkg:
    pkg.lispName or pkg.name or (builtins.substring 0 32 (baseNameOf (toString pkg)));

  # Collect transitive deps via builtins.genericClosure (C++ BFS, O(n log n))
  flatten = packages:
    map (item: item.pkg) (builtins.genericClosure {
      startSet = map (pkg: { key = identity pkg; inherit pkg; }) packages;
      operator = item:
        map (dep: { key = identity dep; pkg = dep; })
          (item.pkg.lispDeps or []);
    });

  # Topological sort using nixpkgs lib.toposort
  toposort = deps:
    let
      result = lib.toposort
        (a: b: builtins.elem a (b.lispDeps or []))
        deps;
    in if result ? result then result.result
       else if result ? cycle then
         throw "Circular dependency detected: ${toString (map identity (result.cycle or []))}"
       else deps;

  # Resolve transitive deps: flatten, deduplicate, toposort
  resolveDeps = deps:
    let
      flattened = flatten deps;
      # Deduplicate by lispName
      deduped = lib.unique flattened;
    in toposort deduped;

  # --- Lisp code generation ---

  # Generate Lisp code to load all resolved deps
  genLoadLisp = deps:
    let
      loadable = builtins.filter (d: (d.lispSrcs or []) != []) deps;
    in lib.concatStringsSep "\n"
      (map (dep: "(load \"${dep}/${dep.lispName}.${faslExt}\")") loadable);

  # Generate Lisp code to compile sources into a single FASL
  genCompileLisp = { name, srcs, deps, muffle ? [] }: writeText "compile-${name}.lisp" ''
    (require 'sb-posix)

    ${genLoadLisp deps}

    (defun nix-compile-lisp (srcfile)
      (let ((outfile (make-pathname :type "fasl"
                                    :directory (or (sb-posix:getenv "NIX_BUILD_TOP")
                                                   (error "not running in a Nix build"))
                                    :name (substitute #\- #\/ srcfile))))
        (multiple-value-bind (out-truename _warnings-p failure-p)
            ${if muffle == [] then "(compile-file srcfile :output-file outfile)" else ''
            (handler-bind (${lib.concatMapStringsSep "\n                          " (c: "(${c} #'muffle-warning)") muffle})
              (compile-file srcfile :output-file outfile))''}
          (if failure-p (sb-posix:exit 1)
              (progn
                (load out-truename)
                (namestring out-truename))))))

    (let ((*compile-verbose* t)
          (catted-fasl (make-pathname :type "fasl"
                                      :directory (or (sb-posix:getenv "out")
                                                     (error "not running in a Nix build"))
                                      :name "${name}")))

      (with-open-file (file catted-fasl
                            :direction :output
                            :if-does-not-exist :create)

        (sb-ext:run-program "cat"
         (mapcar #'nix-compile-lisp
          '(${lib.concatMapStringsSep "\n" (src: "\"${src}\"") srcs}))
         :output file :search t)))
  '';

  # Generate Lisp code to dump an executable
  genDumpLisp = { name, main, deps }: writeText "dump-${name}.lisp" ''
    (require 'sb-posix)
    ;; Ensure full UIOP (including sub-packages like UIOP/IMAGE) is available.
    ;; ASDF depends on UIOP; requiring it guarantees all sub-packages are interned.
    (require :asdf)

    ${genLoadLisp deps}

    (let* ((bindir (concatenate 'string (sb-posix:getenv "out") "/bin"))
           (outpath (make-pathname :name "${name}"
                                   :directory bindir)))

      ;; Tell UIOP that argv[0] refers to running image
      (when (find-package :uiop)
        (eval `(setq ,(find-symbol "*IMAGE-DUMPED-P*" :uiop) :executable)))

      ;; Clear ASDF configuration to avoid baking build-time paths
      (when (find-package :asdf)
        (funcall (find-symbol "CLEAR-CONFIGURATION" :asdf)))

      (save-lisp-and-die outpath
                         :executable t
                         :toplevel
                         (lambda ()
                           (when (find-package :uiop)
                             (let ((setup-fn (find-symbol "SETUP-TEMPORARY-DIRECTORY" :uiop)))
                               (when setup-fn
                                 (funcall setup-fn))))
                           (setf sb-ext:*posix-argv*
                                 (delete "--" sb-ext:*posix-argv*
                                         :test #'string= :count 1))
                           (${main}))
                         :purify t))
  '';

  # Generate Lisp code to run tests
  genTestLisp = { name, srcs, deps, expression }: writeText "test-${name}.lisp" ''
    ${genLoadLisp deps}

    ${lib.concatStringsSep "\n" (map (src: "(load \"${src}\")") srcs)}

    (unless ${expression}
      (sb-ext:exit :code 1))
  '';

  # --- Builders ---

  library = { name, srcs ? [], deps ? [], native ? [], tests ? null, muffle ? [], passthru ? {} }:
    let
      resolvedDeps = resolveDeps deps;
      lispNativeDeps = lib.unique (lib.flatten (native ++ (map (d: d.lispNativeDeps or []) resolvedDeps)));

      testDrv = if tests != null then
        testSuite {
          name = tests.name or "${name}-test";
          srcs = srcs ++ (tests.srcs or []);
          deps = deps ++ (tests.deps or []);
          expression = tests.expression;
        }
      else null;
    in
    lib.fix (self: runCommand "${name}-cllib"
      {
        "${libPathVar}" = lib.makeLibraryPath lispNativeDeps;
        LANG = "C.UTF-8";
        passthru = passthru // {
          inherit lispNativeDeps;
          lispDeps = resolvedDeps;
          lispName = name;
          lispBinary = false;
          lispSrcs = srcs;
          reloadScript = "${self}/reload.lisp";
          tests = testDrv;
          self = self;
        };
      } ''
      ${if testDrv != null
        then "echo 'Test ${testDrv} succeeded'"
        else "echo 'No tests run'"}

      mkdir $out

      ${lib.optionalString (lispNativeDeps != []) ''
        echo -n "${lib.makeLibraryPath lispNativeDeps}" > $out/native-lib-path
      ''}

      # Generate reload.lisp for hot-reload
      cat > $out/reload.lisp << 'DEPS_EOF'
${genReloadLisp resolvedDeps}
DEPS_EOF
      ${lib.optionalString (srcs != []) ''
      cat >> $out/reload.lisp << SELF_EOF

;; Load compiled library FASL
(load "$out/${name}.${faslExt}")
SELF_EOF
      ''}

      ${if srcs == []
        then ''
          echo "Meta-package ${name}: deps only"
        ''
        else ''
          ${sbcl}/bin/sbcl --script ${
            genCompileLisp {
              inherit name srcs muffle;
              deps = resolvedDeps;
            }
          }
        ''
      }
    '');

  program = { name, main ? "${name}:main", srcs ? [], deps ? [], native ? [], tests ? null, passthru ? {} }:
    let
      resolvedDeps = resolveDeps deps;
      lispNativeDeps = lib.unique (lib.flatten (native ++ (map (d: d.lispNativeDeps or []) resolvedDeps)));
      libPath = lib.makeLibraryPath lispNativeDeps;

      hasSrcs = srcs != [];

      selfLib = if hasSrcs then
        library { inherit name srcs native; deps = resolvedDeps; }
      else null;

      # Deps MUST load before selfLib: kli.fasl references symbols by home
      # package (e.g. UIOP/IMAGE:QUIT) which requires deps to be loaded first.
      # Core's genLoadLispGeneric re-resolves via allDeps (toposort); we just
      # put selfLib last since resolvedDeps is already toposorted.
      dumpDeps = resolvedDeps ++ (if hasSrcs then [ selfLib ] else []);

      testDrv = if tests != null then
        testSuite {
          name = tests.name or "${name}-test";
          srcs = srcs ++ (tests.srcs or []);
          deps = deps ++ (tests.deps or []);
          expression = tests.expression;
        }
      else null;
    in
    lib.fix (self: runCommand name
      {
        nativeBuildInputs = [ makeWrapper ];
        "${libPathVar}" = libPath;
        LANG = "C.UTF-8";
        passthru = passthru // {
          lispName = name;
          lispDeps = if hasSrcs then [ selfLib ] else resolvedDeps;
          inherit lispNativeDeps;
          lispBinary = true;
          tests = testDrv;
          lib = selfLib;
          self = self;
        };
      } ''
      ${if testDrv != null
        then ''
          if [ -f "${testDrv}" ]; then
            echo "Tests passed: ${testDrv}"
          else
            echo "Error: Tests must pass"
            exit 1
          fi
        ''
        else ""}
      mkdir -p $out/bin

      ${sbcl}/bin/sbcl --script ${
        genDumpLisp {
          inherit name main;
          deps = dumpDeps;
        }
      }

      wrapProgram $out/bin/${name} \
        --prefix ${libPathVar} : "${libPath}" \
        --add-flags "--"
    '');

  bundled = name:
    library {
      inherit name;
      srcs = lib.singleton (builtins.toFile "${name}.lisp" "(require '${name})");
    };

  testSuite = { name, expression, srcs, deps ? [], native ? [] }:
    let
      resolvedDeps = resolveDeps deps;
      lispNativeDeps = lib.unique (lib.flatten (native ++ (map (d: d.lispNativeDeps or []) resolvedDeps)));
    in
    runCommand name
      {
        "${libPathVar}" = lib.makeLibraryPath lispNativeDeps;
        LANG = "C.UTF-8";
      } ''
      echo "Running test suite ${name}"
      ${sbcl}/bin/sbcl --script ${
        genTestLisp {
          inherit name expression srcs;
          deps = resolvedDeps;
        }
      } | tee $out
      echo "Test suite ${name} succeeded"
    '';

  # Generate reload.lisp for development hot-reload
  genReloadLisp = deps:
    let
      loadable = builtins.filter (dep: (dep.lispSrcs or []) != []) deps;
      getPkgName = dep: lib.toUpper (dep.lispPackage or dep.lispName);
    in lib.concatStringsSep "\n"
      (map (dep:
        let pkgName = getPkgName dep;
        in ''(unless (find-package :${pkgName})
    (load "${dep}/${dep.lispName}.${faslExt}"))''
      ) loadable);

in {
  inherit library program bundled testSuite;
}
