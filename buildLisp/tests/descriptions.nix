# Datatype + validator tests for buildLisp.descriptions.

{ lib, pkgs, mb, buildLisp, ... }:

let
  bl = buildLisp;
  d = bl.descriptions;

  testing = mb.ornaments.testing;
  replOrn = mb.ornaments.replServer;
  swankProto = replOrn.protocols.swank;
  Foreground = replOrn.Mode.Foreground;
  Background = replOrn.Mode.Background;
  OneShot = replOrn.Lifecycle.OneShot;

  inherit (testing) testCase testSuite;

  fgSwank = replOrn.define { protocol = swankProto; mode = Foreground; };
  bgSwank = replOrn.define { protocol = swankProto; mode = Background; };
  oneShotSwank = replOrn.define {
    protocol = swankProto;
    mode = Foreground;
    lifecycle = OneShot;
  };

  src = pkgs.writeText "ds.lisp" "(defun main () nil)";
  impl = bl.sbcl;

  baseFields = {
    name = "ds-x";
    srcs = [ src ];
    implementation = impl;
    brokenOn = [ ];
    deps = [ ];
    cLibraries = [ ];
    runtimeContracts = [ ];
    tests = null;
    commandTools = { };
    passthru = { };
  };

  libFields = baseFields // { replInit = null; muffle = [ ]; resources = { }; };

  progFields = baseFields // {
    main = "ds-x:main";
    verifyPackages = [ ];
    preDump = "";
    dynamicSpaceSize = null;
    runtimeAssets = [ ];
    swank = null;
    sandbox = null;
  };

  scriptFields = baseFields // {
    main = "ds-x:main";
    dynamicSpaceSize = null;
    preLaunch = "";
    extraEnv = { };
    swank = null;
    sandbox = null;
  };

  daemonFields = progFields // { serviceSpec = null; };

  daemonBackground = daemonFields // { swank = bgSwank; };
  daemonOneShot = daemonFields // { swank = oneShotSwank; };
  daemonForeground = daemonFields // { swank = fgSwank; };

  bgDaemonTry = builtins.tryEval (d.validate { kind = "daemon"; name = "ds-x"; spec = daemonBackground; });
  oneShotDaemonTry = builtins.tryEval (d.validate { kind = "daemon"; name = "ds-x"; spec = daemonOneShot; });
  fgDaemonOk = d.validate { kind = "daemon"; name = "ds-x"; spec = daemonForeground; };

  bundleFields = {
    name = "ds-x";
    program = src;
    share = null;
    dataDir = null;
    dlopenProbe = null;
    runtimeContracts = [ ];
    launcherName = "ds-x";
  };

  libOk = d.validate { kind = "library"; name = "ds-x"; spec = libFields; };
  progOk = d.validate { kind = "program"; name = "ds-x"; spec = progFields; };
  scriptOk = d.validate { kind = "script"; name = "ds-x"; spec = scriptFields; };
  bundleOk = d.validate { kind = "relocatableBundle"; name = "ds-x"; spec = bundleFields; };

  suite = testSuite {
    name = "descriptions";
    cases = [
      {
        name = "LispScriptableSpec datatype present";
        body = d.LispScriptableSpec ? T;
      }
      {
        name = "LispLibrarySpec ornament present";
        body = d.LispLibrarySpec ? T && d.LispLibrarySpec ? _ornMeta;
      }
      {
        name = "LispProgramSpec ornament present";
        body = d.LispProgramSpec ? T && d.LispProgramSpec ? _ornMeta;
      }
      {
        name = "LispScriptSpec ornament present";
        body = d.LispScriptSpec ? T && d.LispScriptSpec ? _ornMeta;
      }
      {
        name = "LispDaemonSpec ornament present";
        body = d.LispDaemonSpec ? T && d.LispDaemonSpec ? _ornMeta;
      }
      {
        name = "TestSpec datatype present";
        body = d.TestSpec ? T;
      }
      {
        name = "RelocatableBundleSpec datatype present";
        body = d.RelocatableBundleSpec ? T;
      }
      {
        name = "specs accept runtimeContracts field";
        body = libOk.runtimeContracts == [ ] && progOk.runtimeContracts == [ ];
      }

      {
        name = "library spec roundtrips through validate";
        body = libOk.name == "ds-x";
      }
      {
        name = "program spec roundtrips through validate";
        body = progOk.name == "ds-x";
      }
      {
        name = "script spec roundtrips through validate";
        body = scriptOk.name == "ds-x";
      }
      {
        name = "relocatableBundle spec roundtrips through validate";
        body = bundleOk.name == "ds-x";
      }
      {
        name = "daemon spec with Foreground swank roundtrips";
        body = fgDaemonOk.name == "ds-x";
      }

      {
        name = "daemon spec rejects Background swank structurally";
        body = !bgDaemonTry.success;
      }
      {
        name = "daemon spec rejects OneShot lifecycle structurally";
        body = !oneShotDaemonTry.success;
      }

      {
        name = "validate is a function";
        body = builtins.isFunction d.validate;
      }
      {
        name = "validate rejects unknown kind";
        body =
          let
            t = builtins.tryEval (d.validate {
              kind = "unknown";
              name = "x";
              spec = libFields;
            });
          in
            !t.success;
      }
    ];
  };

in
{
  inherit suite;
}
