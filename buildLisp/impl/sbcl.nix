# SBCL (Steel Bank Common Lisp) implementation.

{ mb
, lib
, sbcl
, writeText
, writeShellScriptBin
, rlwrap
, disableDebugger
, genLoadLispGeneric
, genReloadLispGeneric
, genTestLispGeneric
, allDeps
, allNative
, baseToolEnv
}:

rec {
  name = "sbcl";
  runScript = "${sbcl}/bin/sbcl --script";
  faslExt = "fasl";

  genLoadLisp = genLoadLispGeneric self;
  genReloadLisp = genReloadLispGeneric self;

  # Compile each src to a separate FASL, then concatenate (SBCL's FASLs
  # are just appendable) into "$out/${name}.fasl".
  genCompileLisp = { name, srcs, deps, muffle ? [ ] }: writeText "sbcl-compile.lisp" ''
    ;; This file compiles the specified sources into the Nix build
    ;; directory, creating one FASL file for each source.
    (require 'sb-posix)

    ${self.genLoadLisp deps}

    (defun nix-compile-lisp (srcfile)
      (let ((outfile (make-pathname :type "fasl"
                                    :directory (or (sb-posix:getenv "NIX_BUILD_TOP")
                                                   (error "not running in a Nix build"))
                                    :name (substitute #\- #\/ srcfile))))
        (multiple-value-bind (out-truename _warnings-p failure-p)
            ${if muffle == [] then "(compile-file srcfile :output-file outfile)" else ''
            (handler-bind (${lib.concatMapStringsSep "\n                          " (c: "(${c} #'muffle-warning)") muffle})
              (compile-file srcfile :output-file outfile))'' }
          (if failure-p (sb-posix:exit 1)
              (progn
                ;; For the case of multiple files belonging to the same
                ;; library being compiled, load them in order:
                (load out-truename)

                ;; Return pathname as a string for cat-ting it later
                (namestring out-truename))))))

    (let ((*compile-verbose* t)
          (catted-fasl (make-pathname :type "fasl"
                                      :directory (or (sb-posix:getenv "out")
                                                     (error "not running in a Nix build"))
                                      :name "${name}")))

      (with-open-file (file catted-fasl
                            :direction :output
                            :if-does-not-exist :create)

        ;; SBCL's FASL files can just be bundled together using cat
        (sb-ext:run-program "cat"
         (mapcar #'nix-compile-lisp
          ;; These forms were inserted by the Nix build:
          '(${
            lib.concatMapStringsSep "\n" (src: "\"${src}\"") srcs
          }))
         :output file :search t)))
  '';

  # 'genDumpLisp' generates a Lisp file that instructs SBCL to dump
  # the currently loaded image as an executable to $out/bin/$name.
  genDumpLisp = { name, main, deps, verifyPackages ? [ ], preDump ? "" }: writeText "sbcl-dump.lisp" ''
    (require 'sb-posix)

    ${self.genLoadLisp deps}

    ${lib.optionalString (verifyPackages != []) ''
    ;; Verify specified packages loaded correctly
    (format t "~&Verifying packages: ~{~A~^, ~}~%"
            '(${lib.concatMapStringsSep " " (p: "\"${lib.toUpper p}\"") verifyPackages}))
    (dolist (pkg-name '(${lib.concatMapStringsSep " " (p: "\"${lib.toUpper p}\"") verifyPackages}))
      (unless (find-package pkg-name)
        (format *error-output* "~&ERROR: Package ~A not found!~%" pkg-name)
        (sb-posix:exit 1))
      (format t "  ✓ ~A loaded~%" pkg-name))
    ''}

    (let* ((bindir (concatenate 'string (sb-posix:getenv "out") "/bin"))
           (outpath (make-pathname :name "${name}"
                                   :directory bindir)))

      ;; Tell UIOP that argv[0] will refer to running image, not the lisp impl
      (when (find-package :uiop)
        (eval `(setq ,(find-symbol "*IMAGE-DUMPED-P*" :uiop) :executable)))

      ;; Clear ASDF configuration to avoid baking build-time paths
      (when (find-package :asdf)
        (funcall (find-symbol "CLEAR-CONFIGURATION" :asdf)))

      ${lib.optionalString (preDump != "") ''
      ;; Pre-dump hook (from buildLisp.program preDump parameter)
      ${preDump}
      ''}

      (save-lisp-and-die outpath
                         :executable t
                         :toplevel
                         (lambda ()
                           ;; Reinitialize UIOP temporary directory at runtime
                           (when (find-package :uiop)
                             (let ((setup-fn (find-symbol "SETUP-TEMPORARY-DIRECTORY" :uiop)))
                               (when setup-fn
                                 (funcall setup-fn))))

                           ;; Filter out everything prior to the `--` we
                           ;; insert in the wrapper to prevent SBCL from
                           ;; parsing arguments at startup
                           (setf sb-ext:*posix-argv*
                                 (delete "--" sb-ext:*posix-argv*
                                         :test #'string= :count 1))
                           (${main}))
                         :purify t))
  '';

  wrapProgram = true;

  genTestLisp = genTestLispGeneric self;

  lispWith = deps:
    let
      unwrap = mb.ornaments.dependencies.unwrapDeps;
      resolvedDeps = allDeps self deps;
      lispDeps = builtins.filter (d: !(d.lispBinary or false)) (unwrap resolvedDeps);
      replInit =
        if deps != [ ] && (builtins.head deps) ? replInit
        then (builtins.head deps).replInit
        else null;

      loadFile = if deps != [ ] then writeText "load.lisp" (self.genLoadLisp resolvedDeps) else null;
      wrappedLoadFile =
        if replInit != null && loadFile != null
        then
          writeText "wrapped-load.lisp" ''
            ${replInit}
              (load "${loadFile}"))
          ''
        else loadFile;
    in
    writeShellScriptBin "sbcl" ''
      # CFFI libraries (e.g. cl-plus-ssl → libcrypto) need this at load time.
      export LD_LIBRARY_PATH="${lib.makeLibraryPath (allNative [] lispDeps)}''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH"
      export LANG="C.UTF-8"
      ${mb.ornaments.toolEnv.toInlineSnippet baseToolEnv}
      # SBCL runtime options must precede --load.
      # shellcheck disable=SC2086
      RUNTIME_ARGS=''${NIX_BUILDLISP_LISP_ARGS:-}

      if [ -t 0 ]; then
        exec ${rlwrap}/bin/rlwrap -C sbcl ${sbcl}/bin/sbcl $RUNTIME_ARGS ${
          lib.optionalString (wrappedLoadFile != null) "--load ${wrappedLoadFile}"
        } "$@"
      else
        exec ${sbcl}/bin/sbcl $RUNTIME_ARGS ${
          lib.optionalString (wrappedLoadFile != null) "--load ${wrappedLoadFile}"
        } "$@"
      fi
    '';

  # Self-reference for recursive fields
  self = {
    inherit name runScript faslExt genLoadLisp genReloadLisp genCompileLisp genDumpLisp wrapProgram genTestLisp lispWith;
  };
}
