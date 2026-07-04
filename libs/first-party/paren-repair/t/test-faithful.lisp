(in-package :paren-repair-tests)

;;; ---- parse-faithful ----

(def-suite faithful-suite :description "balance-based parser" :in all)
(in-suite faithful-suite)

(defun faithful-rt-p (s)
  (string= s (pr:flatten (pr:parse-faithful (pr:tokenize s)))))

(test faithful-roundtrips
  "FLATTEN of PARSE-FAITHFUL reproduces balanced input verbatim."
  (dolist (s '("(defun f (x) (+ x 1))"
               "(a (b (c (d))))"
               "(a #(1 2) \"s)tr\" #\\( ; cmt )
b)"
               "#| block ) |# (ok)"
               "  (leading ws) trailing  "))
    (is-true (faithful-rt-p s) "round-trip failed for ~S" s)))

(test faithful-dedent-not-mangled
  "Dedented subforms that indent-mode PARSE mangles round-trip and stay one form."
  (dolist (s '("(defun foo ()
(bar))"
               "(let ((x 1))
(list x)
)"))
    (is-true (faithful-rt-p s) "round-trip failed for ~S" s)
    (let ((forms (remove-if-not #'pr:group-p (pr:parse-faithful (pr:tokenize s)))))
      (is (= 1 (length forms)) "expected one top-level form for ~S" s))))
