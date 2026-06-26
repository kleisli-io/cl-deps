;;;; paren-repair — parens-only parinfer indent-mode delimiter repair.

(defpackage :paren-repair
  (:use #:cl)
  (:export
   ;; Top-level API
   #:repair
   #:balanced-p
   #:repair-if-needed
   #:reader-verdict
   #:read-failure

   ;; Layers
   #:tokenize
   #:parse
   #:parse-faithful
   #:flatten

   ;; Structural editing
   #:replace-sexp
   #:source-line-col

   ;; Indentation
   #:indent-region

   ;; Token
   #:tok
   #:tok-p
   #:tok-type
   #:tok-text
   #:tok-line
   #:tok-col

   ;; Tree nodes
   #:leaf
   #:leaf-p
   #:make-leaf
   #:leaf-tok
   #:group
   #:group-p
   #:make-group
   #:group-open
   #:group-children
   #:group-close
   #:group-synthesized-p))

(in-package :paren-repair)
