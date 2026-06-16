# CCL (Clozure Common Lisp) implementation.

{ mb
, lib
, ccl
, writeText
, writeShellScriptBin
, rlwrap
, writeBash
, disableDebugger
, genLoadLispGeneric
, genReloadLispGeneric
, genTestLispGeneric
, allDeps
, allNative
, baseToolEnv
, targetPlatform
}:

rec {
  name = "ccl";

  # Relatively bespoke wrapper script necessary to make CCL execute
  # a lisp file as a script.
  runScript = writeBash "ccl" ''
    # don't print intro message etc.
    args=("--quiet")

    # makes CCL crash on error instead of entering the debugger
    args+=("--load" "${disableDebugger}")

    # load files from command line in order
    for f in "$@"; do
      args+=("--load" "$f")
    done

    # Exit if everything was processed successfully
    args+=("--eval" "(quit)")

    exec ${ccl}/bin/ccl ''${args[@]}
  '';

  # See https://ccl.clozure.com/docs/ccl.html#building-definitions
  faslExt =
    if targetPlatform.isPower && targetPlatform.is32bit then "pfsl"
    else if targetPlatform.isPower && targetPlatform.is64bit then "p64fsl"
    else if targetPlatform.isx86_64 && targetPlatform.isLinux then "lx64fsl"
    else if targetPlatform.isx86_32 && targetPlatform.isLinux then "lx32fsl"
    else if targetPlatform.isAarch32 && targetPlatform.isLinux then "lafsl"
    else if targetPlatform.isx86_32 && targetPlatform.isDarwin then "dx32fsl"
    else if targetPlatform.isx86_64 && targetPlatform.isDarwin then "dx64fsl"
    else if targetPlatform.isx86_32 && targetPlatform.isFreeBSD then "fx32fsl"
    else if targetPlatform.isx86_64 && targetPlatform.isFreeBSD then "fx64fsl"
    else if targetPlatform.isx86_32 && targetPlatform.isWindows then "wx32fsl"
    else if targetPlatform.isx86_64 && targetPlatform.isWindows then "wx64fsl"
    else builtins.throw "Don't know what FASLs are called for this platform: "
      + targetPlatform.system;

  genLoadLisp = genLoadLispGeneric self;
  genReloadLisp = genReloadLispGeneric self;

  genCompileLisp = { name, srcs, deps }: writeText "ccl-compile.lisp" ''
    ${self.genLoadLisp deps}

    (defun getenv-or-fail (var)
      (or (getenv var)
          (error (format nil "Missing expected environment variable ~A" var))))

    (defun nix-compile-file (srcfile)
      "Trivial wrapper around COMPILE-FILE which causes CCL to exit if
      compilation fails and LOADs the compiled file on success."
      (let ((output (make-pathname :name (substitute #\_ #\/ srcfile)
                                   :type "${self.faslExt}"
                                   :directory (getenv-or-fail "NIX_BUILD_TOP"))))
        (multiple-value-bind (out-truename _warnings-p failure-p)
            (compile-file srcfile :output-file output :print t :verbose t)
            (declare (ignore _warnings-p))
          (if failure-p (quit 1)
              (progn (load out-truename) out-truename)))))

    (fasl-concatenate (make-pathname :name "${name}" :type "${self.faslExt}"
                                     :directory (getenv-or-fail "out"))
                      (mapcar #'nix-compile-file
                              ;; These forms where inserted by the Nix build
                              '(${
                                  lib.concatMapStrings (src: ''
                                    "${src}"
                                  '') srcs
                               })))
  '';

  genDumpLisp = { name, main, deps, verifyPackages ? [ ] }: writeText "ccl-dump.lisp" ''
    ${self.genLoadLisp deps}

    (let* ((out (or (getenv "out") (error "Not running in a Nix build")))
           (bindir (concatenate 'string out "/bin/"))
           (executable (make-pathname :directory bindir :name "${name}")))

      ;; Tell UIOP that argv[0] will refer to running image, not the lisp impl
      (when (find-package :uiop)
        (eval `(setf ,(find-symbol "*IMAGE-DUMPED-P*" :uiop) :executable)))

      (save-application executable
                        :purify t
                        :error-handler :quit
                        :toplevel-function
                        (lambda ()
                          ;; Filter out everything prior to the `--` we
                          ;; insert in the wrapper to prevent CCL from
                          ;; parsing arguments at startup
                          (setf ccl:*command-line-argument-list*
                                (delete "--" ccl:*command-line-argument-list*
                                             :test #'string= :count 1))
                          (${main}))
                        :mode #o755
                        :prepend-kernel t))
  '';

  wrapProgram = true;

  genTestLisp = genTestLispGeneric self;

  lispWith = deps:
    let
      unwrap = mb.ornaments.dependencies.unwrapDeps;
      resolvedDeps = allDeps self deps;
      lispDeps = builtins.filter (d: !(d.lispBinary or false)) (unwrap resolvedDeps);
    in
    writeShellScriptBin "ccl" ''
      # CFFI libraries (e.g. cl-plus-ssl → libcrypto) need this at load time.
      export LD_LIBRARY_PATH="${lib.makeLibraryPath (allNative [] lispDeps)}''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH"
      ${mb.ornaments.toolEnv.toInlineSnippet baseToolEnv}
      # CCL runtime options must precede --load.
      # shellcheck disable=SC2086
      RUNTIME_ARGS=''${NIX_BUILDLISP_LISP_ARGS:-}

      if [ -t 0 ]; then
        exec ${rlwrap}/bin/rlwrap -C ccl ${ccl}/bin/ccl $RUNTIME_ARGS ${
          lib.optionalString (deps != [])
            "--load ${writeText "load.lisp" (self.genLoadLisp resolvedDeps)}"
        } "$@"
      else
        exec ${ccl}/bin/ccl $RUNTIME_ARGS ${
          lib.optionalString (deps != [])
            "--load ${writeText "load.lisp" (self.genLoadLisp resolvedDeps)}"
        } "$@"
      fi
    '';

  # Self-reference for recursive fields
  self = {
    inherit name runScript faslExt genLoadLisp genReloadLisp genCompileLisp genDumpLisp wrapProgram genTestLisp lispWith;
  };
}
