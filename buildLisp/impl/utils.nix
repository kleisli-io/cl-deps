# Shared codegen helpers used by every per-impl module.

{ lib, writeText }:

{
  # Installs a *debugger-hook* that exits non-zero on unhandled errors,
  # so a build never silently drops into the interactive debugger.
  disableDebugger = writeText "disable-debugger.lisp" ''
    (setf *debugger-hook*
          (lambda (error hook)
            (declare (ignore hook))
            (format *error-output* "~%Unhandled error: ~a~%" error)
            #+sbcl (sb-ext:exit :code 1)
            #+ccl (quit 1)
            #+ecl (ext:quit 1)))
  '';

  # Generates `(load "<dep>/<lispName>.<faslExt>")` for each loadable
  # dep. `deps` may be either an `markResolved`-marked record or a raw
  # list; if raw, `allDeps` is invoked to resolve and mark it.
  # Meta-packages (no `lispSrcs`) are skipped — their transitive deps
  # are already in the resolved list.
  genLoadLispGeneric = allDeps: impl: deps:
    let
      marked =
        if builtins.isAttrs deps && deps.__resolvedDeps or false
        then deps
        else allDeps impl deps;
      flat = marked.deps;
      loadable = builtins.filter (d: (d.lispSrcs or [ ]) != [ ]) flat;
    in
    lib.concatStringsSep "\n"
      (map (dep: "(load \"${dep}/${dep.lispName}.${impl.faslExt}\")") loadable);

  # Reload-dep snippet with package guards (skips already-loaded
  # packages — required for shared transitive deps). Always loads FASLs
  # so eval-when :load-toplevel forms fire correctly. Caller must pass
  # a pre-resolved, flat list.
  genReloadLispGeneric = impl: deps:
    let
      pkgName = dep: lib.toUpper (dep.lispPackage or dep.lispName);
      loadable = builtins.filter (d: (d.lispSrcs or [ ]) != [ ]) deps;
      reloads = lib.concatStringsSep "\n"
        (map
          (dep: ''
            (unless (find-package :${pkgName dep})
              (load "${dep}/${dep.lispName}.${impl.faslExt}"))
          '')
          loadable);
    in
    ''
      ;; Reload dependencies (FASLs with package guards)
      ${reloads}
    '';

  # Test runner: load deps + srcs, evaluate `expression`, exit non-zero
  # if it returns NIL.
  genTestLispGeneric = impl: { name, srcs, deps, expression }: writeText "${name}.lisp" ''
    ;; Dependencies
    ${impl.genLoadLisp deps}

    ;; Sources
    ${lib.concatStringsSep "\n" (map (src: "(load \"${src}\")") srcs)}

    ;; Test expression
    (unless ${expression}
      #+sbcl (sb-ext:exit :code 1)
      #+ccl (quit 1)
      #+ecl (ext:quit 1))
  '';
}
