(in-package :paren-repair-tests)

;;; ---- Common Lisp source CST ----

(def-suite cst-suite :description "Common Lisp reader-syntax CST" :in all)
(in-suite cst-suite)

(defun cst-one-form (source)
  (let ((parse (pr:parse-cst source)))
    (is-false (pr:cst-parse-diagnostics parse) "unexpected diagnostics for ~S: ~S"
              source (pr:cst-parse-diagnostics parse))
    (is (= 1 (length (pr:cst-parse-forms parse))) "expected one form for ~S" source)
    (first (pr:cst-parse-forms parse))))

(defun cst-node-text (source node)
  (subseq source (pr:cst-node-start node) (pr:cst-node-end node)))

(test cst-roundtrip-spans-reader-syntax
  "Standard reader-syntax forms are represented as one source-spanning form."
  (dolist (source '("(a (b) c)"
                    "#(1 2 3)"
                    "\"a\\\"b\""
                    "#\\Space"
                    "foo\\ bar"
                    "|Foo Bar|"
                    "'(a b)"
                    "`(a ,x ,@ys)"
                    "#'(lambda (x) x)"
                    "#.(+ 1 2)"
                    "#+sbcl (foo)"
                    "#-sbcl (bar)"
                    "#P\"/tmp/x\""
                    "#C(1 2)"
                    "#S(foo :x 1)"
                    "#A((1 2))"
                    "#2A((1 2) (3 4))"
                    "#B1010"
                    "#O17"
                    "#Xff"
                    "#36RZ"
                    "#*101001"
                    "#1=(a #1#)"))
    (let ((node (cst-one-form source)))
      (is (= 0 (pr:cst-node-start node)) "bad start for ~S" source)
      (is (= (length source) (pr:cst-node-end node)) "bad end for ~S" source)
      (is (string= source (cst-node-text source node)) "bad source slice for ~S" source))))

(test cst-comments-are-trivia
  "Line and nested block comments are trivia, not forms."
  (let* ((source (format nil "; leading~%#| outer #| inner |# done |# '(x)"))
         (parse (pr:parse-cst source))
         (forms (pr:cst-parse-forms parse))
         (trivia (pr:cst-parse-trivia parse)))
    (is-false (pr:cst-parse-diagnostics parse))
    (is (= 1 (length forms)))
    (is (eq :quote (pr:cst-node-kind (first forms))))
    (is (= (search "'(x)" source) (pr:cst-node-start (first forms))))
    (is-true (find :line-comment trivia :key #'pr:cst-trivia-kind))
    (is-true (find :block-comment trivia :key #'pr:cst-trivia-kind))))

(test cst-prefix-nodes-own-consumed-form
  "Reader prefix nodes span their marker and consumed payload form."
  (let* ((source "#'(lambda (x) x)")
         (node (cst-one-form source)))
    (is (eq :function (pr:cst-node-kind node)))
    (is (= 1 (length (pr:cst-node-children node))))
    (is (eq :list (pr:cst-node-kind (first (pr:cst-node-children node)))))
    (is (string= source (cst-node-text source node)))))

(test cst-feature-conditionals-are-source-syntax
  "Feature conditionals retain both the feature expression and following form."
  (let* ((source "#+(and sbcl x) (foo)")
         (node (cst-one-form source)))
    (is (eq :feature-positive (pr:cst-node-kind node)))
    (is (= 2 (length (pr:cst-node-children node))))
    (is (string= "(and sbcl x)"
                 (cst-node-text source (first (pr:cst-node-children node)))))
    (is (string= "(foo)"
                 (cst-node-text source (second (pr:cst-node-children node)))))))

(test cst-unknown-dispatch-is-bounded-diagnostic
  "Unknown dispatch syntax reports a diagnostic without swallowing later forms."
  (let* ((source "#<unreadable> (ok)")
         (parse (pr:parse-cst source))
         (forms (pr:cst-parse-forms parse))
         (diagnostics (pr:cst-parse-diagnostics parse)))
    (is (= 2 (length forms)))
    (is (eq :unknown-dispatch (pr:cst-node-kind (first forms))))
    (is (string= "#<unreadable>" (cst-node-text source (first forms))))
    (is (eq :list (pr:cst-node-kind (second forms))))
    (is (= 1 (length diagnostics)))
    (is (search "unknown dispatch" (pr:cst-diagnostic-message (first diagnostics))))))

(defun cst-single-node (source)
  (first (pr:cst-parse-forms (pr:parse-cst source))))

(defun cst-node-eq (left right)
  (pr:cst-node= left (cst-single-node left) right (cst-single-node right)))

(test cst-source-form-count-reader-syntax
  "Reader-valid source syntax that consumes payload forms counts as one form."
  (dolist (source '("'(a b)"
                    "#'(lambda (x) x)"
                    "#:foo"
                    "#P\"/x\""
                    "#C(1 2)"
                    "#B101"
                    "#*101"
                    "#2A((1 2))"))
    (is (= 1 (pr:source-form-count source)) "expected one source form for ~S" source)))

(test cst-structural-equality-ignores-comment-trivia
  "Comment trivia does not affect CST structural equality, but remains in spans."
  (let* ((source "(a ; line comment
 b #| block comment |# c)")
         (node (cst-single-node source)))
    (is-true (cst-node-eq source "(a b c)"))
    (is (search "; line comment" (cst-node-text source node)))
    (is (search "#| block comment |#" (cst-node-text source node)))))

(test cst-structural-equality-compares-raw-atom-text
  "Atom equality is source-token equality with no reader normalization."
  (dolist (pair '(("FOO" "foo")
                  ("1" "#x1")
                  ("|Foo|" "Foo")
                  ("pkg:x" "pkg::x")))
    (is-false (cst-node-eq (first pair) (second pair))
              "unexpected raw atom match for ~S and ~S" (first pair) (second pair))))

(test cst-find-matches-descends-through-reader-prefix-payloads
  "Matching can target a whole reader-prefix form or its consumed child payload."
  (let* ((source "(list '(a b) (a b))")
         (root (cst-single-node source))
         (whole (cst-single-node "'(a b)"))
         (payload (cst-single-node "(a b)")))
    (let ((whole-hits (pr:cst-find-matches source (list root) "'(a b)" whole))
          (payload-hits (pr:cst-find-matches source (list root) "(a b)" payload)))
      (is (= 1 (length whole-hits)))
      (is (string= "'(a b)" (cst-node-text source (first (first whole-hits)))))
      (is (= 2 (length payload-hits)))
      (is (equal '("(a b)" "(a b)")
                 (mapcar (lambda (hit) (cst-node-text source (first hit))) payload-hits))))))

(defmacro with-source-replace ((out st info) args &body body)
  `(multiple-value-bind (,out ,st ,info) (pr:replace-source-form ,@args)
     (declare (ignorable ,out ,st ,info))
     ,@body))

(test cst-source-replace-existing-statuses
  "CST source replacement preserves replace-source-form statuses and span info shapes."
  (with-source-replace (out st info) ("(+ 1 2)" "(+ 1 2)" "(* 3 4)")
    (is (eq :ok st))
    (is (string= "(* 3 4)" out))
    (is (equal '((0 . 7)) info)))
  (with-source-replace (out st info) ("(assoc   m  :k    v)" "(assoc m :k v)" "X")
    (is (eq :ok st))
    (is (string= "X" out)))
  (with-source-replace (out st info) ("(list (a) (a))" "(a)" "Z")
    (is (eq :ambiguous st))
    (is (null out))
    (is (= 2 (length info))))
  (with-source-replace (out st info) ("(list (a) (a))" "(a)" "Z" :replace-all t)
    (is (eq :ok st))
    (is (string= "(list Z Z)" out)))
  (with-source-replace (out st info) ("(foo bar)" "(nope)" "X")
    (is (eq :not-found st))
    (is (null out)))
  (with-source-replace (out st info) ("(foo)" "a b" "X")
    (is (eq :bad-match st))))

(test cst-source-replace-delete-seam-and-byte-exact-untouched
  "Deletion consumes one adjacent whitespace separator and preserves untouched bytes."
  (with-source-replace (out st info) ("(a b c)" "b" "")
    (is (eq :ok st))
    (is (string= "(a c)" out)))
  (with-source-replace (out st info) ("(a b c)" "c" "")
    (is (eq :ok st))
    (is (string= "(a b)" out)))
  (with-source-replace (out st info)
      ("; keep header
(list #| keep block |# (a b))" "(a b)" "X")
    (is (eq :ok st))
    (is (string= "; keep header
(list #| keep block |# X)" out))))

(defparameter +cst-clos-src+
  "(defmethod area ((s square))
  (* (side s) (side s)))

(defmethod area ((s circle))
  (* pi (r s) (r s)))
")

(test cst-source-replace-within-scoping
  "CST source replacement scopes searches to a unique top-level form."
  (with-source-replace (out st info)
      (+cst-clos-src+ "(r s)" "RR"
                      :within-type "defmethod" :within-name "area ((s circle))"
                      :replace-all t)
    (is (eq :ok st))
    (is (string= "(defmethod area ((s square))
  (* (side s) (side s)))

(defmethod area ((s circle))
  (* pi RR RR))
" out)))
  (with-source-replace (out st info)
      (+cst-clos-src+ "(r s)" "RR"
                      :within-type "defmethod" :within-name "area ((s triangle))")
    (is (eq :within-not-found st)))
  (with-source-replace (out st info)
      (+cst-clos-src+ "(side s)" "SS"
                      :within-type "defmethod" :within-name "area")
    (is (eq :within-ambiguous st))
    (is (= 2 (length info)))))

(test cst-source-replace-reader-syntax-forms
  "CST source replacement can replace prefix forms, payloads, and dispatch literals."
  (with-source-replace (out st info) ("(list '(a b) (a b))" "'(a b)" "Q")
    (is (eq :ok st))
    (is (string= "(list Q (a b))" out)))
  (with-source-replace (out st info) ("(list '(a b) (a b))" "(a b)" "Z" :replace-all t)
    (is (eq :ok st))
    (is (string= "(list 'Z Z)" out)))
  (with-source-replace (out st info) ("(list #P\"/x\" #C(1 2) #B101)" "#C(1 2)" "C")
    (is (eq :ok st))
    (is (string= "(list #P\"/x\" C #B101)" out)))
  (with-source-replace (out st info) ("(list #P\"/x\" #C(1 2) #B101)" "#B101" "B")
    (is (eq :ok st))
    (is (string= "(list #P\"/x\" #C(1 2) B)" out))))

(test cst-source-replace-rejects-syntax-diagnostics
  "Source syntax diagnostics are reported instead of using guessed spans."
  (with-source-replace (out st info) ("#<unreadable> (ok)" "(ok)" "X")
    (is (eq :syntax-error st))
    (is (null out))
    (is (find :unknown-dispatch info :key #'pr:cst-diagnostic-kind))))

(test cst-source-syntax-validity
  "Diagnostics-free source is valid; syntax faults report diagnostics and are invalid."
  (dolist (source (list "(defun f (x) (+ x 1))" "'(a b)" ""
                        (format nil "; only a comment~%")))
    (is-true (pr:source-syntax-valid-p source) "expected valid for ~S" source)
    (is-false (pr:source-syntax-diagnostics source) "unexpected diagnostics for ~S" source))
  (dolist (source '("#<unreadable>" "(defun f ()" "#| open comment"))
    (is-false (pr:source-syntax-valid-p source) "expected invalid for ~S" source)
    (is-true (pr:source-syntax-diagnostics source) "expected diagnostics for ~S" source)))

(test cst-source-position-line-and-column
  "SOURCE-POSITION reports 1-based line/column and clamps past the source end."
  (let ((source (format nil "(a b)~%(c~%  d)")))
    (flet ((pos (offset)
             (multiple-value-list (pr:source-position source offset))))
      (is (equal '(1 1) (pos 0)))
      (is (equal '(1 4) (pos 3)))                        ; the b atom
      (is (equal '(2 1) (pos (search "(c" source))))
      (is (equal '(3 3) (pos (search "d)" source))))
      (is (equal '(3 5) (pos (+ (length source) 100))))))) ; clamped to end
