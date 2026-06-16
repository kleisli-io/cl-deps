# ECL (Embeddable Common Lisp) implementation.

{ mb
, lib
, ecl
, writeText
, writeShellScriptBin
, runCommand
, disableDebugger
, genLoadLispGeneric
, genReloadLispGeneric
, genTestLispGeneric
, allDeps
, allNative
, baseToolEnv
}:

rec {
  name = "ecl";
  runScript = "${ecl}/bin/ecl --load ${disableDebugger} --shell";
  faslExt = "fasc";

  genLoadLisp = genLoadLispGeneric self;
  genReloadLisp = genReloadLispGeneric self;

  genCompileLisp = { name, srcs, deps }: writeText "ecl-compile.lisp" ''
    ;; This seems to be required to bring make the 'c' package available
    ;; early, otherwise ECL tends to fail with a read failure…
    (ext:install-c-compiler)

    ;; Load dependencies
    ${self.genLoadLisp deps}

    (defun getenv-or-fail (var)
      (or (ext:getenv var)
          (error (format nil "Missing expected environment variable ~A" var))))

    (defun nix-compile-file (srcfile &key native)
      "Compile the given srcfile into a compilation unit in :out-dir using
      a unique name based on srcfile as the filename which is returned after
      compilation. If :native is true, create an native object file,
      otherwise a byte-compile fasc file is built and immediately loaded."

      (let* ((unique-name (substitute #\_ #\/ srcfile))
             (out-file (make-pathname :type (if native "o" "fasc")
                                      :directory (getenv-or-fail "NIX_BUILD_TOP")
                                      :name unique-name)))
        (multiple-value-bind (out-truename _warnings-p failure-p)
            (compile-file srcfile :system-p native
                                  :load (not native)
                                  :output-file out-file
                                  :verbose t :print t)
          (if failure-p (ext:quit 1) out-truename))))

    (let* ((out-dir (getenv-or-fail "out"))
           (nix-build-dir (getenv-or-fail "NIX_BUILD_TOP"))
           (srcs
            ;; These forms are inserted by the Nix build
            '(${lib.concatMapStringsSep "\n" (src: "\"${src}\"") srcs})))

      ;; First, we'll byte compile loadable FASL files and load them
      ;; immediately. Since we are using a statically linked ECL, there's
      ;; no way to load native objects, so we rely on byte compilation
      ;; for all our loading — which is crucial in compilation of course.
      (ext:install-bytecodes-compiler)

      ;; ECL's bytecode FASLs can just be concatenated to create a bundle
      (let ((bundle-out (make-pathname :type "fasc" :name "${name}"
                                       :directory out-dir)))

        (with-open-file (fasc-stream bundle-out :direction :output)
          (ext:run-program "cat"
                           (mapcar (lambda (f)
                                     (namestring
                                      (nix-compile-file f :native nil)))
                                   srcs)
                           :output fasc-stream)))

      (ext:install-c-compiler)

      ;; Build a (natively compiled) static archive (.a) file.
      (c:build-static-library
       (make-pathname :type "a" :name "${name}" :directory out-dir)
       :lisp-files (mapcar (lambda (x)
                             (nix-compile-file x :native t))
                           srcs)))
  '';

  genDumpLisp = { name, main, deps, verifyPackages ? [ ] }: writeText "ecl-dump.lisp" ''
    (defun getenv-or-fail (var)
      (or (ext:getenv var)
          (error (format nil "Missing expected environment variable ~A" var))))

    ${self.genLoadLisp deps}

    ;; makes a 'c' package available that can link executables
    (ext:install-c-compiler)

    (c:build-program
     (merge-pathnames (make-pathname :directory '(:relative "bin")
                                     :name "${name}")
                      (truename (getenv-or-fail "out")))
     :epilogue-code `(progn
                      ;; UIOP doesn't understand ECL, so we need to make it
                      ;; aware that we are a proper executable
                      ,(when (find-package :uiop)
                        `(setf ,(find-symbol "*IMAGE-DUMPED-P*" :uiop) :executable))
                      ;; Run the actual application…
                      (${main})
                      ;; … and exit.
                      (ext:quit))
     ;; ECL can't remember these from its own build…
     :ld-flags '("-static")
     :lisp-files
     ;; The following forms are inserted by the Nix build
     '(${
         let
           unwrap = mb.ornaments.dependencies.unwrapDeps;
           resolvedDeps = allDeps self deps;
         in lib.concatMapStrings (dep: ''
           "${dep}/${dep.lispName}.a"
         '') (unwrap resolvedDeps)
       }))
  '';

  wrapProgram = false;

  genTestLisp = genTestLispGeneric self;

  lispWith = deps:
    let
      unwrap = mb.ornaments.dependencies.unwrapDeps;
      resolvedDeps = allDeps self deps;
      lispDeps = builtins.filter (d: !(d.lispBinary or false)) (unwrap resolvedDeps);
    in
    writeShellScriptBin "ecl" ''
      ${mb.ornaments.toolEnv.toInlineSnippet baseToolEnv}
      exec ${ecl}/bin/ecl ${
        lib.optionalString (deps != [])
          "--load ${writeText "load.lisp" (self.genLoadLisp resolvedDeps)}"
      } "$@"
    '';

  bundled = name: runCommand "${name}-cllib"
    {
      passthru = {
        lispName = name;
        lispNativeDeps = [ ];
        lispDeps = [ ];
        lispBinary = false;
        repl = self.lispWith [ (self.bundled name) ];
      };
    } ''
    mkdir -p "$out"
    ln -s "${ecl}/lib/ecl-${ecl.version}/${name}.${self.faslExt}" -t "$out"
    ln -s "${ecl}/lib/ecl-${ecl.version}/lib${name}.a" "$out/${name}.a"
  '';

  # Self-reference for recursive fields
  self = {
    inherit name runScript faslExt genLoadLisp genReloadLisp genCompileLisp genDumpLisp wrapProgram genTestLisp lispWith bundled;
  };
}
