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
   #:delimiter-imbalanced-p

   ;; Faithful parse layer — backs edit-sexp match-location labeling
   #:tokenize
   #:parse
   #:parse-faithful
   #:flatten

   ;; Source-form editing
   #:replace-source-form

   ;; Common Lisp source CST
   #:parse-cst
   #:source-form-count
   #:source-syntax-diagnostics
   #:source-syntax-valid-p
   #:source-position
   #:cst-node=
   #:cst-find-matches
   #:cst-parse
   #:cst-parse-p
   #:cst-parse-forms
   #:cst-parse-trivia
   #:cst-parse-diagnostics
   #:cst-node
   #:cst-node-p
   #:cst-node-kind
   #:cst-node-start
   #:cst-node-end
   #:cst-node-children
   #:cst-trivia
   #:cst-trivia-p
   #:cst-trivia-kind
   #:cst-trivia-start
   #:cst-trivia-end
   #:cst-diagnostic
   #:cst-diagnostic-p
   #:cst-diagnostic-kind
   #:cst-diagnostic-start
   #:cst-diagnostic-end
   #:cst-diagnostic-message

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
