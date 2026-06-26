(in-package :paren-repair-tests)

;;; ---- lexer ----

(def-suite lexer-suite :description "tokenize" :in all)
(in-suite lexer-suite)

(defun roundtrip-p (s)
  (string= s (apply #'concatenate 'string
                    (mapcar #'pr:tok-text (pr:tokenize s)))))

(defun open/close-count (s)
  (count-if (lambda (tk) (member (pr:tok-type tk) '(:open :close)))
            (pr:tokenize s)))

(test roundtrips
  "(concat token-texts) reproduces the input verbatim."
  (dolist (s '("(defun f (x) (+ x 1))"
               "(list #\\( #\\) \"a)b\" #(1 2))"
               "#| block ) comment |# (ok)"
               "(progn |weird(sym| (bar))"
               ";; comment ) with paren
(foo)"))
    (is-true (roundtrip-p s) "round-trip failed for ~S" s)))

(test isolates-real-delimiters
  "Parens inside string/char/comment/vbar/block are never tagged."
  (is (= 0 (open/close-count "\"(a (b )\"")))   ; string
  (is (= 0 (open/close-count "#\\( #\\)")))      ; chars
  (is (= 0 (open/close-count "; (a) )")))        ; line comment
  (is (= 0 (open/close-count "|a (b) c|")))      ; vbar
  (is (= 0 (open/close-count "#| ( ( ( |#"))))   ; block comment

(test tags-genuine-parens
  "( and #( open; ) closes."
  (is (= 4 (open/close-count "(a #(b) c)")))     ; ( #( ) )
  (is (= 2 (open/close-count "()"))))

;;; ---- judge ----

(def-suite judge-suite :description "reader-verdict" :in all)
(in-suite judge-suite)

(test verdict-three-way
  (is (eq :clean         (pr:reader-verdict "(a (b) c)")))
  (is (eq :clean         (pr:reader-verdict "#.(+ 1 2)")))      ; #. neutralized
  (is (eq :unbalanced    (pr:reader-verdict "(a (b")))          ; missing closer
  (is (eq :unbalanced    (pr:reader-verdict "(a))")))           ; extra closer
  (is (eq :indeterminate (pr:reader-verdict "no-such::sym"))))  ; unknown package

(test verdict-ignores-non-delimiters
  "Parens inside strings/comments/chars don't affect balance."
  (is (eq :clean (pr:reader-verdict "(format t \"~A)\" x)")))
  (is (eq :clean (pr:reader-verdict "(list #\\( #\\))")))
  (is (eq :clean (pr:reader-verdict "#| ) ) ) |# (ok)"))))

(test verdict-non-delimiter-read-errors
  "A read failure with balanced parens is :indeterminate, not :unbalanced —
parinfer can't and shouldn't repair it."
  (is (eq :indeterminate (pr:reader-verdict ":a,b")))                 ; stray comma, no parens
  (is (eq :indeterminate (pr:reader-verdict "(:k :grad-3,-4 v)")))    ; comma, balanced parens
  (is (eq :indeterminate (pr:reader-verdict "#<foo>")))               ; unreadable #-syntax
  (is (eq :unbalanced (pr:reader-verdict "(a,b))"))))                 ; comma but parens truly broken

(test read-failure-reports-the-condition
  "read-failure surfaces the underlying reader condition and a position."
  (is (null (pr:read-failure "(a (b) c)")))
  (multiple-value-bind (c offset) (pr:read-failure "(:k :grad-3,-4 v)")
    (is-true (typep c 'reader-error))
    (is-true (integerp offset))))

;;; ---- repair (exact goldens) ----

(def-suite repair-suite :description "repair exact cases" :in all)
(in-suite repair-suite)

(defmacro golden (name in out)
  `(test ,name (is (string= ,out (pr:repair ,in)))))

(golden truncation "(foo (bar" "(foo (bar))")

(golden extra-close-dropped "(foo))" "(foo)")

(golden vector-truncation "#(1 2 3" "#(1 2 3)")

(golden string-paren-untouched
  "(format t \"~A)\" x"
  "(format t \"~A)\" x)")

(golden binding-list-dedent
  "(let ((a 1)
      (b 2)
  body)"
  "(let ((a 1)
      (b 2))
  body)")

(golden binding-list-eof
  "(let ((a 1)
      (b 2)
  body"
  "(let ((a 1)
      (b 2))
  body)")

(golden full-dedent-to-toplevel
  "(defun f ()
  (when x
    (g (h
done"
  "(defun f ()
  (when x
    (g (h))))
done")

(golden partial-dedent
  "(defun f ()
  (when x
    (foo)
  (bar"
  "(defun f ()
  (when x
    (foo))
  (bar))")

(golden trailing-comment-preserved
  "(foo  ; note
  (bar"
  "(foo  ; note
  (bar))")

;;; ---- invariants ----

(def-suite invariant-suite :description "repair invariants" :in all)
(in-suite invariant-suite)

(defparameter +unbalanced+
  '("(foo (bar"
    "(foo))"
    "#(1 2 3"
    "(let ((a 1)
      (b 2)
  body"
    "(defun f ()
  (when x
    (g (h
done"))

(defparameter +clean+
  '("(defun f (x) (+ x 1))
"
    "(let ((a 1)
      (b 2))
  (+ a b))
"
    ";; a comment with ) paren
(foo)
"
    "(list #\\( #\\) \"a)b\" #(1 2))
"
    "#| block ) comment |# (ok)
"
    "(progn |weird(sym| (bar))
"))

(test output-always-clean
  "Repairing any unbalanced input yields reader-clean output."
  (dolist (s +unbalanced+)
    (is-true (pr:balanced-p (pr:repair s)) "not clean: ~S" s)))

(test idempotent
  "Repair is a fixpoint on its own output."
  (dolist (s (append +unbalanced+ +clean+))
    (let ((r (pr:repair s)))
      (is (string= r (pr:repair r)) "not idempotent: ~S" s))))

(test clean-is-identity
  "Already-balanced source is returned byte-for-byte."
  (dolist (s +clean+)
    (is (string= s (pr:repair s)) "clean mutated: ~S" s)))

;;; ---- repair-if-needed (the extension entry) ----

(def-suite gate-suite :description "repair-if-needed gating" :in all)
(in-suite gate-suite)

(test clean-untouched
  "A balanced file returns (values s nil)."
  (dolist (s +clean+)
    (multiple-value-bind (out changed) (pr:repair-if-needed s)
      (is-false changed)
      (is (string= s out)))))

(test unbalanced-repaired
  "An unbalanced file returns (values fixed t) with fixed reading clean."
  (dolist (s +unbalanced+)
    (multiple-value-bind (out changed) (pr:repair-if-needed s)
      (is-true changed)
      (is-true (pr:balanced-p out))
      (is (string/= s out)))))

(test indeterminate-untouched
  "Non-delimiter problems (e.g. unknown package) are never touched."
  (let ((s "(foo bad::sym"))
    (multiple-value-bind (out changed) (pr:repair-if-needed s)
      (declare (ignore out))
      ;; verdict is :indeterminate (package-error masks the missing closer),
      ;; so the gate declines rather than guessing.
      (is-false changed))))
