# Hash-fixture suite. Pins the outPath of representative buildLisp
# invocations across library / program / script / daemon, with and without
# swank and sandbox. Locks the shell-text byte content of generated
# runCommand bodies — the eval-time invariant suites verify type-shape
# stability but not shell-text drift; this suite covers that gap.
#
# Refresh workflow when an outPath legitimately moves:
#   nix eval --json '.#lib.x86_64-linux.buildLisp.tests.fixtureActuals'
# then edit the matching `expected.<key>` after auditing the cause.
# Never edit `expected` to mask an unexplained change.

{ lib, pkgs, mb, buildLisp, ... }:

let
  bl = buildLisp;

  testing = mb.ornaments.testing;
  sandboxOrn = mb.ornaments.sandbox;
  replOrn = mb.ornaments.replServer;
  swankProto = replOrn.protocols.swank;
  Foreground = replOrn.Mode.Foreground;

  inherit (testing) testCase testSuite;

  src = name: body: pkgs.writeText "${name}.lisp" body;

  libSrc = src "fx-lib" ''
    (defpackage :fx-lib (:use :cl) (:export :main))
    (in-package :fx-lib)
    (defun main () (format t "fx-lib~%"))
  '';

  programSrc = src "fx-prog" ''
    (defpackage :fx-prog (:use :cl) (:export :main))
    (in-package :fx-prog)
    (defun main () (format t "fx-prog~%"))
  '';

  scriptSrc = src "fx-script" ''
    (defpackage :fx-script (:use :cl) (:export :main))
    (in-package :fx-script)
    (defun main () (format t "fx-script~%"))
  '';

  daemonSrc = src "fx-daemon" ''
    (defpackage :fx-daemon (:use :cl) (:export :main))
    (in-package :fx-daemon)
    (defun main () (format t "fx-daemon~%"))
  '';

  fgSwank = replOrn.define { protocol = swankProto; mode = Foreground; };
  sealed = sandboxOrn.profiles.sealed;

  depLib = bl.library { name = "fx-dep-lib"; srcs = [ libSrc ]; };
  fxProgram = bl.program {
    name = "fx-prog";
    srcs = [ programSrc ];
    main = "fx-prog:main";
  };
  fxRelocatable = bl.mkRelocatableBundle {
    name = "fx-prog";
    program = fxProgram;
    launcherName = "fx-reloc";
  };
  fxRelocatableCaProbe = pkgs.runCommand "fx-relocatable-ca-probe" { } ''
    set -eu
    launcher=${fxRelocatable}/bin/fx-reloc
    grep -q 'SSL_CERT_FILE' "$launcher"
    grep -q '/etc/ssl/certs/ca-certificates.crt' "$launcher"
    grep -q '/etc/pki/tls/certs/ca-bundle.crt' "$launcher"
    touch "$out"
  '';

  derivations = {
    fixLib = bl.library {
      name = "fx-lib";
      srcs = [ libSrc ];
    };

    fixLibDeps = bl.library {
      name = "fx-lib-deps";
      srcs = [ libSrc ];
      deps = [ depLib ];
    };

    fixLibTests = bl.library {
      name = "fx-lib-tests";
      srcs = [ libSrc ];
      tests = { expression = "t"; };
    };

    fixProg = fxProgram;

    fixRelocatable = fxRelocatable;

    fixRelocatableCaProbe = fxRelocatableCaProbe;

    fixProgSwank = bl.program {
      name = "fx-prog-swank";
      srcs = [ programSrc ];
      main = "fx-prog:main";
      swank = fgSwank;
    };

    fixProgSandbox = bl.program {
      name = "fx-prog-sandbox";
      srcs = [ programSrc ];
      main = "fx-prog:main";
      sandbox = sealed;
    };

    fixProgSwankSandbox = bl.program {
      name = "fx-prog-sw-sb";
      srcs = [ programSrc ];
      main = "fx-prog:main";
      swank = fgSwank;
      sandbox = sealed;
    };

    fixScript = bl.script {
      name = "fx-script";
      srcs = [ scriptSrc ];
      main = "fx-script:main";
    };

    fixScriptSandbox = bl.script {
      name = "fx-script-sandbox";
      srcs = [ scriptSrc ];
      main = "fx-script:main";
      sandbox = sealed;
    };

    fixDaemon = bl.daemon {
      name = "fx-daemon";
      srcs = [ daemonSrc ];
      main = "fx-daemon:main";
    };

    fixDaemonSwankSandbox = bl.daemon {
      name = "fx-daemon-sw-sb";
      srcs = [ daemonSrc ];
      main = "fx-daemon:main";
      swank = fgSwank;
      sandbox = sealed;
    };
  };

  # Expected outPaths. Captured once when this file landed; updated only
  # after an intentional, audited drift. Placeholders here are flagged by
  # the suite as drift so the initial capture cannot pass silently.
  expected = {
    fixLib = "/nix/store/7zrlwcqc6i6nplq89qdp6vy2yx1d2crx-fx-lib-cllib";
    fixLibDeps = "/nix/store/yg8jc71ff9xn5hqrv4gd8ghzyjkvcymd-fx-lib-deps-cllib";
    fixLibTests = "/nix/store/4xp0z9gqjvpagk6i8rwsqgf3q7dnaz3w-fx-lib-tests-cllib";
    fixProg = "/nix/store/mgv8z7h0bvln0sgpagwaccz4qqc5x5cq-fx-prog";
    fixRelocatable = "/nix/store/hc1565gv42w5qwp7a1lqk2d384y41zvv-fx-prog-relocatable";
    fixRelocatableCaProbe = "/nix/store/vlxg398hjvhyryqxch7lnyygf3ab3a4g-fx-relocatable-ca-probe";
    fixProgSwank = "/nix/store/ddfpm8kg2r5ab1hwf86737f7b4yw0zxy-fx-prog-swank";
    fixProgSandbox = "/nix/store/i8cfva0mmwi3l6gkzhk74kjsw9qz3sd2-fx-prog-sandbox";
    fixProgSwankSandbox = "/nix/store/5bz8lyahf76rqp3xr5clv23qywrsnqsf-fx-prog-sw-sb";
    fixScript = "/nix/store/y46armpwh5ynaf1rcg1ab6msimxz9wxg-fx-script";
    fixScriptSandbox = "/nix/store/7a4vgj9mfizl5dz67a57qj2d74k7m1kn-fx-script-sandbox";
    fixDaemon = "/nix/store/j3h6s097ikghnfgfibavnv1rxggy9akx-fx-daemon";
    fixDaemonSwankSandbox = "/nix/store/9ybdb4bi47aiygi0fxk43hl5k7l4bjdp-fx-daemon-sw-sb";
  };

  actuals = lib.mapAttrs (_: drv: drv.outPath) derivations;

  mkCase = name: drv: testCase {
    name = "${name} outPath stable";
    body =
      let
        got = drv.outPath;
        want = expected.${name};
      in
      if got == want then true
      else
        builtins.trace
          "fixture drift [${name}]: expected ${want}, got ${got}"
          false;
  };

  suite = testSuite {
    name = "derivation-fixtures";
    cases = lib.mapAttrsToList mkCase derivations;
  };

in
{
  inherit suite derivations actuals;
}
