{ pkgs, lib, buildLisp, lisp }:

# Parens-only parinfer indent-mode delimiter repair.
buildLisp.library {
  name = "paren-repair";
  srcs = [
    ./package.lisp
    ./lexer.lisp
    ./judge.lisp
    ./parse.lisp
    ./parse-faithful.lisp
    ./flatten.lisp
    ./repair.lisp
    ./structural.lisp
    ./indent.lisp
    ./replace-sexp.lisp
  ];
  tests = {
    deps = [ lisp.fiveam ];
    srcs = [
      ./t/package.lisp
      ./t/test-engine.lisp
      ./t/test-structural.lisp
      ./t/test-indent.lisp
    ];
    expression = "(fiveam:run! 'paren-repair-tests::all)";
  };
}
