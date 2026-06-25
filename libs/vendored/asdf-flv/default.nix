{ pkgs, lib, buildLisp, lisp }:

buildLisp.library {
  name = "asdf-flv";
  deps = [ lisp.asdf ];
  srcs = [
    ./package.lisp
    ./asdf-flv.lisp
  ];
}
