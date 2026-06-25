{ pkgs, lib, buildLisp, lisp }:

buildLisp.library {
  name = "let-over-lambda";
  deps = [
    lisp.alexandria
    lisp.cl-ppcre
    lisp.named-readtables
    lisp.fare-quasiquote-readtable
    lisp.trivia-quasiquote
  ];
  srcs = [
    ./package.lisp
    ./let-over-lambda.lisp
  ];
  # Vendored from thephoeron/let-over-lambda; rev drift-checks the qlfile pin.
  passthru = { rev = "9b3751213b1cb0bc1ddf11cd7a53f8f620fe9125"; };
}
