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

;;; ---- snode= ----

(def-suite snode-suite :description "surface-structural equality" :in all)
(in-suite snode-suite)

(defun one-node (s)
  (first (pr::significant-roots (pr:parse-faithful (pr:tokenize s)))))

(defun snode-eq (a b)
  (pr::snode= (one-node a) (one-node b)))

(test snode-whitespace-insensitive
  (is-true (snode-eq "(a   b   c)" "(a b c)"))
  (is-true (snode-eq "(a
 b)" "(a b)")))

(test snode-distinguishes-shape
  (is-false (snode-eq "(a b)" "(a c)"))
  (is-false (snode-eq "(a b)" "(a b c)"))
  (is-false (snode-eq "#(1 2)" "(1 2)"))     ; vector vs list
  (is-true  (snode-eq "#(1 2)" "#(1 2)")))

;;; ---- replace-sexp ----

(def-suite replace-suite :description "structural splice" :in all)
(in-suite replace-suite)

(defmacro with-replace ((out st info) args &body body)
  "Bind the three REPLACE-SEXP values for the literal ARGS list, then run BODY."
  `(multiple-value-bind (,out ,st ,info) (pr:replace-sexp ,@args)
     (declare (ignorable ,out ,st ,info))
     ,@body))

(test replace-unique
  (with-replace (out st info) ("(+ 1 2)" "(+ 1 2)" "(* 3 4)")
    (is (eq :ok st))
    (is (string= "(* 3 4)" out))))

(test replace-whitespace-insensitive-match
  (with-replace (out st info) ("(assoc   m  :k    v)" "(assoc m :k v)" "X")
    (is (eq :ok st))
    (is (string= "X" out))))

(test replace-ambiguous-rejects
  (with-replace (out st info) ("(list (a) (a))" "(a)" "Z")
    (is (eq :ambiguous st))
    (is (null out))
    (is (= 2 (length info)))))

(test replace-all
  (with-replace (out st info) ("(list (a) (a))" "(a)" "Z" :replace-all t)
    (is (eq :ok st))
    (is (string= "(list Z Z)" out))))

(test replace-not-found
  (with-replace (out st info) ("(foo bar)" "(nope)" "X")
    (is (eq :not-found st))
    (is (null out))))

(test replace-bad-match
  (with-replace (out st info) ("(foo)" "a b" "X")
    (is (eq :bad-match st))))

(test replace-delete-seam
  "Delete consumes one adjacent separator — no double space."
  (with-replace (out st info) ("(a b c)" "b" "")
    (is (string= "(a c)" out)))
  (with-replace (out st info) ("(a b c)" "c" "")
    (is (string= "(a b)" out))))

(test replace-byte-exact-untouched
  "Only the matched span changes; surrounding bytes are preserved verbatim."
  (with-replace (out st info)
      ("(defun a () 1)
(defun b () 2)
" "(defun b () 2)" "(defun b () 22)")
    (is (eq :ok st))
    (is (string= "(defun a () 1)
(defun b () 22)
" out))))

;;; ---- within scoping (CLOS) ----

(def-suite within-suite :description "within top-level form scoping" :in all)
(in-suite within-suite)

(defparameter +clos-src+
  "(defmethod area ((s square))
  (* (side s) (side s)))

(defmethod area ((s circle))
  (* pi (r s) (r s)))
")

(test within-scopes-by-specializer
  "Scoping to one CLOS method edits only that method, byte-exact elsewhere."
  (with-replace (out st info)
      (+clos-src+ "(r s)" "RR"
                  :within-type "defmethod" :within-name "area ((s circle))"
                  :replace-all t)
    (is (eq :ok st))
    (is (string= "(defmethod area ((s square))
  (* (side s) (side s)))

(defmethod area ((s circle))
  (* pi RR RR))
" out))))

(test within-not-found
  (with-replace (out st info)
      (+clos-src+ "(r s)" "RR"
                  :within-type "defmethod" :within-name "area ((s triangle))")
    (is (eq :within-not-found st))))

(test within-ambiguous
  "A name prefix matching several forms rejects with the candidate spans."
  (with-replace (out st info)
      (+clos-src+ "(side s)" "SS"
                  :within-type "defmethod" :within-name "area")
    (is (eq :within-ambiguous st))
    (is (= 2 (length info)))))
