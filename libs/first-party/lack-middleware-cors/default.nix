{ pkgs, lib, buildLisp, lisp }:

# Vendored — Lack upstream ships no cors middleware.
buildLisp.library {
  name = "lack-middleware-cors";
  deps = [ ];
  srcs = [ ./cors.lisp ];
}
