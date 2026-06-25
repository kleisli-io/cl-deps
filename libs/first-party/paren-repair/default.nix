{ pkgs, lib, buildLisp, lisp }:

# Parens-only parinfer indent-mode delimiter repair.
buildLisp.library {
  name = "paren-repair";
  srcs = [
    ./package.lisp
    ./lexer.lisp
    ./judge.lisp
    ./parse.lisp
    ./flatten.lisp
    ./repair.lisp
  ];
  tests = {
    deps = [ lisp.fiveam ];
    srcs = [
      ./t/package.lisp
      ./t/test-engine.lisp
    ];
    expression = "(fiveam:run! 'paren-repair-tests::all)";
  };
}
