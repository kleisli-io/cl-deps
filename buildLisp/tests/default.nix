# buildLisp — eval-time test surface.
#
# Each category is a typed `testSuite` of `testCase` records; `runPure`
# evaluates the boolean bodies and produces per-suite pass/fail/skip/error
# tallies. The top-level exposes a roll-up summary so consumers can hang a
# CI gate off `buildLisp.tests.allPass` without forcing a
# derivation build.
#
# Conventions:
#   - All swank specs constructed via `replServer.define` (typed REPLServerSpec).
#   - All sandbox profiles constructed via `sandbox.define` or `sandbox.profiles.*`.
#   - Assertions probe the typed orn passthru contract: `replSpec` (not
#     `replConfig`), `sandboxProfile`, `systemdHardening`, `serviceSpec`,
#     `lib` (= selfLib), `isDaemon`, `isScript`, `originalMain`.

{ lib, pkgs, mb, buildLisp, ... }:

let
  bl = buildLisp;

  testing = mb.ornaments.testing;
  sandboxOrn = mb.ornaments.sandbox;
  replOrn = mb.ornaments.replServer;
  swankProto = replOrn.protocols.swank;
  Foreground = replOrn.Mode.Foreground;
  Background = replOrn.Mode.Background;

  inherit (testing) testCase testSuite runPure;

  # Shared fixtures
  src = name: body: pkgs.writeText "${name}.lisp" body;

  daemonSrc = src "test-daemon" ''
    (defpackage :test-daemon (:use :cl) (:export :main))
    (in-package :test-daemon)
    (defun main () (format t "test-daemon~%"))
  '';

  programSrc = src "test-program" ''
    (defpackage :test-program (:use :cl) (:export :main))
    (in-package :test-program)
    (defun main () (format t "test-program~%"))
  '';

  scriptSrc = src "test-script" ''
    (defpackage :test-script (:use :cl) (:export :main))
    (in-package :test-script)
    (defun main () (format t "test-script~%"))
  '';

  testLib = bl.library {
    name = "test-lib";
    srcs = [
      (src "test-lib" ''
        (defpackage :test-lib (:use :cl) (:export :main))
        (in-package :test-lib)
        (defun main () (format t "test-lib~%"))
      '')
    ];
  };

  # Typed swank specs
  foregroundSwank = replOrn.define { protocol = swankProto; mode = Foreground; };
  backgroundSwank = replOrn.define { protocol = swankProto; mode = Background; };
  customSwank = replOrn.define {
    protocol = swankProto;
    mode = Background;
    port = 14099;
    portEnvVar = "TEST_SWANK_PORT";
  };
  generatedBackgroundSwankWrapper = bl.repl.swankCodegen.mkWrapper {
    name = "test-program-swank";
    main = "test-program:main";
    config = {
      protocol = swankProto;
      mode = "background";
      registration = "none";
      style = ":spawn";
      port = 14099;
      portEnvVar = "TEST_SWANK_PORT";
      interface = "127.0.0.1";
      shortLivedFlags = [];
      extra = {};
    };
  };

  # Builder fixtures
  testProgram = bl.program {
    name = "test-program";
    srcs = [ programSrc ];
    main = "test-program:main";
  };
  testProgramSwank = bl.program {
    name = "test-program-swank";
    srcs = [ programSrc ];
    main = "test-program:main";
    swank = customSwank;
  };
  testProgramDepsOnly = bl.program {
    name = "test-deps-only";
    deps = [ testLib ];
    main = "test-lib:main";
  };

  testScript = bl.script {
    name = "test-script";
    srcs = [ scriptSrc ];
    main = "test-script:main";
  };
  testScriptDepsOnly = bl.script {
    name = "test-script-deps-only";
    deps = [ testLib ];
    main = "test-lib:main";
  };

  testDaemon = bl.daemon {
    name = "test-daemon";
    srcs = [ daemonSrc ];
    main = "test-daemon:main";
  };
  testDaemonSwank = bl.daemon {
    name = "test-daemon-swank";
    srcs = [ daemonSrc ];
    main = "test-daemon:main";
    swank = foregroundSwank;
  };
  testDaemonSpec = bl.daemon {
    name = "test-daemon-spec";
    srcs = [ daemonSrc ];
    main = "test-daemon:main";
    swank = replOrn.define {
      protocol = swankProto;
      mode = Foreground;
      port = 4010;
    };
    serviceSpec = {
      env = {
        MY_PORT = { default = "3000"; description = "Web port"; };
        MY_HOST = { default = "localhost"; };
        MY_FLAG = { };
      };
      assets = { STATIC_ROOT = "/nix/store/fake-static"; };
    };
  };

  # Sandbox fixtures — typed profiles
  sealedProfile = sandboxOrn.profiles.sealed;
  serverProfile = sandboxOrn.define (
    builtins.removeAttrs sealedProfile [ "_con" ] // {
      readWritePaths = [ "/tmp/work" ];
      listenTcp = [ (sandboxOrn.tcpEndpoint { port = 8080; }) ];
    }
  );
  sourceWriteProfile = sandboxOrn.define (
    builtins.removeAttrs sealedProfile [ "_con" ] // {
      sourceAccess = true;
      sourceWritePaths = [ "ace/tasks" ".claude" ];
      listenTcp = [ (sandboxOrn.tcpEndpoint { port = 14011; }) ];
      connectTcp = [ (sandboxOrn.tcpEndpoint { port = 11434; }) ];
    }
  );

  testScriptSandboxed = bl.script {
    name = "test-script-sandboxed";
    srcs = [ scriptSrc ];
    main = "test-script:main";
    sandbox = sealedProfile;
  };
  testProgramSandboxed = bl.program {
    name = "test-program-sandboxed";
    srcs = [ programSrc ];
    main = "test-program:main";
    sandbox = sealedProfile;
  };
  testDaemonSandboxed = bl.daemon {
    name = "test-daemon-sandboxed";
    srcs = [ daemonSrc ];
    main = "test-daemon:main";
    swank = foregroundSwank;
    sandbox = sandboxOrn.define (
      builtins.removeAttrs sealedProfile [ "_con" ] // {
        listenTcp = [ (sandboxOrn.tcpEndpoint { port = 14005; }) ];
      }
    );
  };
  testProgramSourceWrite = bl.program {
    name = "test-program-source-write";
    srcs = [ programSrc ];
    main = "test-program:main";
    sandbox = sourceWriteProfile;
  };
  testScriptServerProfile = bl.script {
    name = "test-script-server";
    srcs = [ scriptSrc ];
    main = "test-script:main";
    sandbox = serverProfile;
  };

  # ============================================================================
  # Surface suite — public attribute existence + types
  # ============================================================================
  surface = testSuite {
    name = "surface";
    cases = [
      { name = "exports library"; body = builtins.isFunction bl.library; }
      { name = "exports program"; body = builtins.isFunction bl.program; }
      { name = "exports script"; body = builtins.isFunction bl.script; }
      { name = "exports daemon"; body = builtins.isFunction bl.daemon; }
      { name = "exports grovel"; body = builtins.isFunction bl.grovel; }
      { name = "exports sbclWith"; body = builtins.isFunction bl.sbclWith; }
      { name = "exports withTools"; body = builtins.isFunction bl.withTools; }
      { name = "exports extend"; body = builtins.isFunction bl.extend; }
      { name = "exports implementations"; body = bl ? implementations; }
      { name = "exports toolEnv"; body = bl ? toolEnv && bl.toolEnv._con == "MetaBuilderToolEnv"; }
      { name = "exports sbcl"; body = bl.sbcl.name == "sbcl"; }
      { name = "exports ecl"; body = bl.ecl.name == "ecl"; }
      { name = "exports ccl"; body = bl.ccl.name == "ccl"; }
      { name = "exports repl.swankCodegen"; body = bl ? repl && bl.repl ? swankCodegen; }
      { name = "exports serviceSpec.mkServiceSpec"; body = builtins.isFunction bl.serviceSpec.mkServiceSpec; }
      { name = "exports serviceSpec.specToDefaultEnv"; body = builtins.isFunction bl.serviceSpec.specToDefaultEnv; }
    ];
  };

  # ============================================================================
  # Implementations suite — multi-impl system shape
  # ============================================================================
  implementations = testSuite {
    name = "implementations";
    cases = [
      { name = "sbcl variant present"; body = bl.implementations.impls ? sbcl; }
      { name = "ecl variant present"; body = bl.implementations.impls ? ecl; }
      { name = "ccl variant present"; body = bl.implementations.impls ? ccl; }
      { name = "sbcl exposes replWith"; body = bl.implementations.impls.sbcl ? replWith; }
      { name = "system exposes withExtras"; body = builtins.isFunction bl.implementations.withExtras; }
      { name = "sbcl.faslExt = fasl"; body = bl.sbcl.faslExt == "fasl"; }
      { name = "ecl.faslExt = fasc"; body = bl.ecl.faslExt == "fasc"; }
      { name = "ccl.faslExt is string"; body = builtins.isString bl.ccl.faslExt; }
      { name = "sbcl.wrapProgram = true"; body = bl.sbcl.wrapProgram == true; }
      { name = "ecl.wrapProgram = false"; body = bl.ecl.wrapProgram == false; }
      { name = "ccl.wrapProgram = true"; body = bl.ccl.wrapProgram == true; }
      { name = "sbcl has genCompileLisp"; body = builtins.isFunction bl.sbcl.genCompileLisp; }
      { name = "sbcl has genDumpLisp"; body = builtins.isFunction bl.sbcl.genDumpLisp; }
      { name = "sbcl has lispWith"; body = builtins.isFunction bl.sbcl.lispWith; }
    ];
  };

  # ============================================================================
  # toolEnv suite — typed tool composition
  # ============================================================================
  toolEnv =
    let
      withGit = bl.withTools { git = pkgs.git; };
      withBoth = withGit.withTools { curl = pkgs.curl; };
    in
    testSuite {
      name = "toolEnv";
      cases = [
        {
          name = "base toolEnv is empty";
          body = mb.ornaments.toolEnv.isEmpty bl.toolEnv;
        }
        {
          name = "toolEnv carries typed _con";
          body = bl.toolEnv._con == "MetaBuilderToolEnv";
        }
        {
          name = "withTools preserves builder API";
          body = withGit ? library && withGit ? program;
        }
        {
          name = "withTools surfaces added tool";
          body = (mb.ornaments.toolEnv.toolPackages withGit.toolEnv) ? git;
        }
        {
          name = "withTools is chainable";
          body =
            let pkgs' = mb.ornaments.toolEnv.toolPackages withBoth.toolEnv;
            in pkgs' ? git && pkgs' ? curl;
        }
        {
          name = "withTools preserves per-impl entry points";
          body = withGit ? sbcl && withGit ? ecl && withGit ? ccl;
        }
        {
          name = "extend + withTools compose";
          body =
            let e = bl.extend { }; w = e.withTools { git = pkgs.git; };
            in w ? library && (mb.ornaments.toolEnv.toolPackages w.toolEnv) ? git;
        }
      ];
    };

  # ============================================================================
  # Library suite
  # ============================================================================
  library =
    let
      libNoTests = bl.library { name = "lib-no-tests"; srcs = [ ]; };
      libWithTests = bl.library {
        name = "lib-with-tests";
        srcs = [ ];
        tests = { expression = "t"; };
      };
    in
    testSuite {
      name = "library";
      cases = [
        {
          name = "returns derivation";
          body = pkgs.lib.isDerivation libNoTests;
        }
        {
          name = "passthru.lispName matches";
          body = libNoTests.passthru.lispName == "lib-no-tests";
        }
        {
          name = "passthru.lispBinary = false";
          body = libNoTests.passthru.lispBinary == false;
        }
        {
          name = "tests spec propagates";
          body = libWithTests.passthru.tests != null
            && libWithTests.passthru.tests.name == "lib-with-tests-test";
        }
        {
          name = "per-impl variants present";
          body = libNoTests ? sbcl && libNoTests ? ecl && libNoTests ? ccl;
        }
      ];
    };

  # ============================================================================
  # Resources suite — runtime resource-root injection
  # ============================================================================
  resources =
    let
      themesDir = pkgs.runCommand "test-themes" { } ''
        mkdir -p $out
        printf '{"name":"x"}' > $out/a.json
      '';
      plainLib = bl.library {
        name = "res-plain";
        srcs = [ (src "res-plain" "(defpackage :res-plain (:use :cl))") ];
      };
      resLib = bl.library {
        name = "res-lib";
        srcs = [ (src "res-lib" "(defpackage :res-lib (:use :cl))") ];
        resources = { "test/res" = themesDir; };
      };
      regName = "res-lib-resource-roots.lisp";
    in
    testSuite {
      name = "resources";
      cases = [
        {
          name = "no resources → srcs unchanged";
          body = builtins.length plainLib.passthru.lispSrcs == 1;
        }
        {
          name = "resources inject support + registration sources";
          body = builtins.length resLib.passthru.lispSrcs == 3;
        }
        {
          name = "registration source present by name";
          body = lib.any
            (s: builtins.isAttrs s && (s.name or "") == regName)
            resLib.passthru.lispSrcs;
        }
        {
          name = "passthru.lispResources preserved";
          body = resLib.passthru.lispResources ? "test/res";
        }
        {
          name = "no resources → empty lispResources";
          body = plainLib.passthru.lispResources == { };
        }
      ];
    };

  # ============================================================================
  # Program suite — typed passthru contract
  # ============================================================================
  program = testSuite {
    name = "program";
    cases = [
      {
        name = "returns derivation";
        body = pkgs.lib.isDerivation testProgram;
      }
      {
        name = "passthru.lispBinary = true";
        body = testProgram.passthru.lispBinary == true;
      }
      {
        name = "deps-only program returns null lib";
        body = testProgramDepsOnly.passthru.lib == null;
      }
      {
        name = "deps-only program has lispDeps";
        body =
          let ld = testProgramDepsOnly.passthru.lispDeps or null;
          in builtins.isList ld && builtins.length ld > 0;
      }
      {
        name = "no swank → no replSpec passthru";
        body = !(testProgram.passthru ? replSpec);
      }
      {
        name = "swank declared → passthru.replSpec present";
        body = testProgramSwank.passthru ? replSpec;
      }
      {
        name = "replSpec carries typed _con";
        body = testProgramSwank.passthru.replSpec._con == "MetaBuilderREPLServerSpec";
      }
      {
        name = "replSpec.port matches user input";
        body = testProgramSwank.passthru.replSpec.port == 14099;
      }
      {
        name = "replSpec.portEnvVar matches user input";
        body = testProgramSwank.passthru.replSpec.portEnvVar == "TEST_SWANK_PORT";
      }
      {
        name = "replSpec.mode = Background";
        body = testProgramSwank.passthru.replSpec.mode._con == "Background";
      }
      {
        name = "per-impl variants present";
        body = testProgram ? sbcl && testProgram ? ecl && testProgram ? ccl;
      }
    ];
  };

  # ============================================================================
  # Script suite
  # ============================================================================
  script = testSuite {
    name = "script";
    cases = [
      {
        name = "returns derivation";
        body = pkgs.lib.isDerivation testScript;
      }
      {
        name = "passthru.isScript = true";
        body = testScript.passthru.isScript == true;
      }
      {
        name = "passthru.lispBinary = true";
        body = testScript.passthru.lispBinary == true;
      }
      {
        name = "passthru.loadScript present";
        body = testScript.passthru ? loadScript;
      }
      {
        name = "srcs-present → passthru.lib non-null";
        body = testScript.passthru.lib != null;
      }
      {
        name = "deps-only → passthru.lib = null";
        body = testScriptDepsOnly.passthru.lib == null;
      }
      {
        name = "per-impl variants present";
        body = testScript ? sbcl && testScript ? ecl && testScript ? ccl;
      }
    ];
  };

  # ============================================================================
  # Daemon suite — Foreground/LongRunning enforcement + serviceSpec
  # ============================================================================
  daemon =
    let
      backgroundRaisesTry = builtins.tryEval (bl.daemon {
        name = "bad-daemon";
        srcs = [ daemonSrc ];
        main = "test-daemon:main";
        swank = backgroundSwank;
      });
    in
    testSuite {
      name = "daemon";
      cases = [
        {
          name = "returns derivation";
          body = pkgs.lib.isDerivation testDaemon;
        }
        {
          name = "passthru.isDaemon = true";
          body = testDaemon.passthru.isDaemon == true;
        }
        {
          name = "passthru.originalMain preserves user main";
          body = testDaemon.passthru.originalMain == "test-daemon:main";
        }
        {
          name = "no swank → no replSpec passthru";
          body = !(testDaemon.passthru ? replSpec);
        }
        {
          name = "swank Foreground → replSpec passthru present";
          body = testDaemonSwank.passthru ? replSpec;
        }
        {
          name = "swank Background → throws (category error)";
          body = !backgroundRaisesTry.success;
        }
        {
          name = "no serviceSpec → no passthru.serviceSpec";
          body = !(testDaemon.passthru ? serviceSpec);
        }
        {
          name = "serviceSpec env preserved";
          body =
            let s = testDaemonSpec.passthru.serviceSpec;
            in s.env ? MY_PORT && s.env.MY_PORT.default == "3000";
        }
        {
          name = "serviceSpec assets preserved";
          body = testDaemonSpec.passthru.serviceSpec.assets.STATIC_ROOT == "/nix/store/fake-static";
        }
        {
          name = "swank port auto-injected into serviceSpec env";
          body =
            let s = testDaemonSpec.passthru.serviceSpec;
            in s.env ? SWANK_PORT && s.env.SWANK_PORT.default == "4010";
        }
        {
          name = "per-impl variants present";
          body = testDaemon ? sbcl && testDaemon ? ecl && testDaemon ? ccl;
        }
      ];
    };

  # ============================================================================
  # Sandbox suite — typed projections + passthru contract
  # ============================================================================
  sandbox = testSuite {
    name = "sandbox";
    cases = [
      {
        name = "script sandbox → passthru.sandboxProfile present";
        body = testScriptSandboxed.passthru ? sandboxProfile;
      }
      {
        name = "script no sandbox → no sandboxProfile";
        body = !(testScript.passthru ? sandboxProfile);
      }
      {
        name = "script sandbox → sandboxBpfBwrap present";
        body = testScriptSandboxed.passthru ? sandboxBpfBwrap;
      }
      {
        name = "script sandbox → sandboxBpfSelf present";
        body = testScriptSandboxed.passthru ? sandboxBpfSelf;
      }
      {
        name = "program sandbox → passthru.sandboxProfile present";
        body = testProgramSandboxed.passthru ? sandboxProfile;
      }
      {
        name = "daemon sandbox → passthru.sandboxProfile present";
        body = testDaemonSandboxed.passthru ? sandboxProfile;
      }
      {
        name = "daemon sandbox → passthru.systemdHardening typed";
        body =
          testDaemonSandboxed.passthru.systemdHardening._con
          == "MetaBuilderSystemdHardening";
      }
      {
        name = "sandbox profile typed _con preserved through builder";
        body = testScriptSandboxed.passthru.sandboxProfile._con
          == "MetaBuilderSandboxProfile";
      }
      {
        name = "server profile preserves readWritePaths";
        body = testScriptServerProfile.passthru.sandboxProfile.readWritePaths
          == [ "/tmp/work" ];
      }
      {
        name = "server profile preserves listenTcp";
        body = builtins.length testScriptServerProfile.passthru.sandboxProfile.listenTcp == 1;
      }
      {
        name = "sourceWritePaths preserved through builder";
        body = testProgramSourceWrite.passthru.sandboxProfile.sourceWritePaths
          == [ "ace/tasks" ".claude" ];
      }
      {
        name = "sourceAccess preserved";
        body = testProgramSourceWrite.passthru.sandboxProfile.sourceAccess == true;
      }
      {
        name = "connectTcp preserved";
        body = builtins.length testProgramSourceWrite.passthru.sandboxProfile.connectTcp == 1;
      }
      {
        name = "toLandlock projects listenPorts from listenTcp";
        body =
          let ll = sandboxOrn.toLandlock testScriptServerProfile.passthru.sandboxProfile;
          in ll.listenPorts == [ 8080 ];
      }
      {
        name = "toSystemd produces typed hardening";
        body =
          (sandboxOrn.toSystemd sealedProfile)._con
          == "MetaBuilderSystemdHardening";
      }
    ];
  };

  # ============================================================================
  # serviceSpec suite — pure-lib normalizer + projection
  # ============================================================================
  serviceSpec =
    let
      ss = bl.serviceSpec;
      spec1 = ss.mkServiceSpec {
        env = {
          FOO = { default = "bar"; description = "test"; };
          BAZ = { };
        };
      };
      spec2 = ss.mkServiceSpec {
        env = { };
        assets = { STATIC = "/nix/store/static-dir"; };
      };
      spec3 = ss.mkServiceSpec {
        secrets = {
          "db-password" = { envVar = "DB_PASSWORD_FILE"; sopsKey = "db-password"; };
          "api-key" = { envVar = "API_KEY_FILE"; };
        };
      };
    in
    testSuite {
      name = "serviceSpec";
      cases = [
        {
          name = "mkServiceSpec normalizes env defaults";
          body = spec1.env.FOO.default == "bar"
            && spec1.env.FOO.description == "test"
            && spec1.env.BAZ.default == "";
        }
        {
          name = "mkServiceSpec preserves assets";
          body = spec2.assets.STATIC == "/nix/store/static-dir";
        }
        {
          name = "specToDefaultEnv flattens env + assets";
          body =
            let e = ss.specToDefaultEnv spec2;
            in e.STATIC == "/nix/store/static-dir";
        }
        {
          name = "mkServiceSpec on empty input is empty";
          body = (ss.mkServiceSpec { }).env == { }
            && (ss.mkServiceSpec { }).assets == { };
        }
        {
          name = "secret normalization defaults sopsKey to name";
          body = spec3.secrets."api-key".sopsKey == "api-key";
        }
        {
          name = "secret normalization preserves explicit sopsKey";
          body = spec3.secrets."db-password".sopsKey == "db-password";
        }
      ];
    };

  # ============================================================================
  # REPL spec suite — typed REPLServerSpec contract
  # ============================================================================
  repl = testSuite {
    name = "repl";
    cases = [
      {
        name = "swankCodegen.mkWrapper is function";
        body = builtins.isFunction bl.repl.swankCodegen.mkWrapper;
      }
      {
        name = "replServer.define produces typed spec";
        body = foregroundSwank._con == "MetaBuilderREPLServerSpec";
      }
      {
        name = "swank protocol defaults port = 4005";
        body = foregroundSwank.port == 4005;
      }
      {
        name = "swank protocol defaults portEnvVar = SWANK_PORT";
        body = foregroundSwank.portEnvVar == "SWANK_PORT";
      }
      {
        name = "Foreground mode carries _con";
        body = foregroundSwank.mode._con == "Foreground";
      }
      {
        name = "Background mode carries _con";
        body = backgroundSwank.mode._con == "Background";
      }
      {
        name = "lifecycle defaults to LongRunning";
        body = foregroundSwank.lifecycle._con == "LongRunning";
      }
      {
        name = "user port override applied";
        body = customSwank.port == 14099;
      }
      {
        name = "user portEnvVar override applied";
        body = customSwank.portEnvVar == "TEST_SWANK_PORT";
      }
      {
        name = "explicit swank port collision refuses ephemeral fallback";
        body =
          lib.hasInfix "from TEST_SWANK_PORT is in use" generatedBackgroundSwankWrapper
          && lib.hasInfix "refusing ephemeral fallback" generatedBackgroundSwankWrapper
          && lib.hasInfix "(sb-ext:exit :code 1 :abort t)" generatedBackgroundSwankWrapper;
      }
      {
        name = "default swank port collision still supports ephemeral fallback";
        body = lib.hasInfix "trying ephemeral port" generatedBackgroundSwankWrapper;
      }
      {
        name = "swank wrapper isolates process stdio from worker threads";
        body =
          lib.hasInfix "(defun make-repl-io-bindings" generatedBackgroundSwankWrapper
          && lib.hasInfix "(*standard-input* . ,input)" generatedBackgroundSwankWrapper
          && lib.hasInfix "(*query-io* . ,io)" generatedBackgroundSwankWrapper
          && lib.hasInfix "(*terminal-io* . ,io)" generatedBackgroundSwankWrapper
          && lib.hasInfix "(*debug-io* . ,io)" generatedBackgroundSwankWrapper
          && lib.hasInfix "\"*DEFAULT-WORKER-THREAD-BINDINGS*\" \"SWANK\"" generatedBackgroundSwankWrapper;
      }
      {
        name = "background swank server thread runs under io isolation";
        body =
          lib.hasInfix "(install-repl-worker-io-isolation)" generatedBackgroundSwankWrapper
          && lib.hasInfix "(with-repl-io-isolation ()" generatedBackgroundSwankWrapper
          && lib.hasInfix "(let ((port (start-repl-server)))" generatedBackgroundSwankWrapper;
      }
    ];
  };

  # ============================================================================
  # Derivation-hash fixtures — locks shell-text byte content of runCommand
  # bodies. Eval-time invariants above verify type shape; this catches drift.
  # ============================================================================
  derivationFixtures = import ./derivation-fixtures.nix { inherit lib pkgs mb buildLisp; };

  # ============================================================================
  # Typed input contracts — datatypes + validator behaviour.
  # ============================================================================
  descriptionsTests = import ./descriptions.nix { inherit lib pkgs mb buildLisp; };

  # ============================================================================
  # Roll-up
  # ============================================================================
  suiteList = [
    surface
    implementations
    toolEnv
    library
    resources
    program
    script
    daemon
    sandbox
    serviceSpec
    repl
    derivationFixtures.suite
    descriptionsTests.suite
  ];

  results = lib.listToAttrs (map
    (s: {
      name = s.name;
      value = runPure s;
    })
    suiteList);

  totals = lib.foldl'
    (acc: r: {
      passed = acc.passed + r.passed;
      failed = acc.failed + r.failed;
      skipped = acc.skipped + r.skipped;
      allPass = acc.allPass && r.allPass;
    })
    { passed = 0; failed = 0; skipped = 0; allPass = true; }
    (builtins.attrValues results);

in
{
  inherit (totals) passed failed skipped allPass;
  suites = results;
  fixtureDerivations = derivationFixtures.derivations;
  fixtureActuals = derivationFixtures.actuals;
  summary =
    "${toString totals.passed} passed, ${toString totals.failed} failed"
    + lib.optionalString (totals.skipped > 0) " (${toString totals.skipped} skipped)";
}
