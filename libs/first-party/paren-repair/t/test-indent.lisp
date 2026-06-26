(in-package :paren-repair-tests)

(def-suite indent-suite :description "canonical reindentation" :in all)
(in-suite indent-suite)

(defun ind (s &rest args) (apply #'pr:indent-region s args))

;;; ---- idempotency cornerstone ----

(test indent-canonical-identity
  "Canonically-indented source is a fixed point: reindent changes nothing."
  (dolist (s (list (format nil "(defun foo (x)~%  (+ x 1))")
                   (format nil "(let ((x 1)~%      (y 2))~%  (+ x y))")
                   (format nil "(defpackage :demo~%  (:use #:cl)~%  (:export #:run))")
                   (format nil "(defun run (xs)~%  (let ((acc 0))~%    (dolist (x xs)~%      (incf acc x))~%    acc))")))
    (is (string= s (ind s)) "not a fixed point: ~s -> ~s" s (ind s))))

(test indent-fixpoint
  "indent . indent = indent on arbitrary (messy) input."
  (let* ((messy (format nil "(defun foo (x)~%(let ((y (* x 2)))~%(when (> y 0)~%(format t \"~~a\" y))~%y))"))
         (once (ind messy)))
    (is (string= once (ind once)))))

;;; ---- dedent fix (the finding-#1 motivation) ----

(test indent-dedent-fix
  "A fully dedented body is reindented to canonical columns."
  (is (string= (format nil "(defun foo ()~%  (bar))")
               (ind (format nil "(defun foo ()~%(bar))")))))

;;; ---- distinguished tier vs body ----

(test indent-distinguished-mvbind
  "multiple-value-bind: bindings/values distinguished (open+4), body open+2."
  (is (string= (format nil "(multiple-value-bind (a b)~%    (values 1 2)~%  body)")
               (ind (format nil "(multiple-value-bind (a b)~%(values 1 2)~%body)")))))

(test indent-defmethod-qualifier
  "A method qualifier is a distinguished slot; body still lands at open+2."
  (is (string= (format nil "(defmethod foo :around ((x t))~%  body)")
               (ind (format nil "(defmethod foo :around ((x t))~%body)")))))

;;; ---- table heads ----

(test indent-table-heads
  (is (string= (format nil "(let ((x 1))~%  (foo x))")
               (ind (format nil "(let ((x 1))~%(foo x))"))))
  (is (string= (format nil "(cond~%  (a 1)~%  (b 2))")
               (ind (format nil "(cond~%(a 1)~%(b 2))"))))
  (is (string= (format nil "(when test~%  body)")
               (ind (format nil "(when test~%body)")))))

;;; ---- default rule ----

(test indent-default-align-under-arg1
  "With an arg on the head line, continuations align under that first arg."
  (is (string= (format nil "(foo bar~%     baz)")
               (ind (format nil "(foo bar~%baz)")))))

(test indent-default-head-alone
  "Head alone on its line: args fall at open+1."
  (is (string= (format nil "(foo~% bar)")
               (ind (format nil "(foo~%bar)")))))

;;; ---- name heuristic ----

(test indent-name-heuristic
  "Unregistered with-/def- macros indent conventionally."
  (is (string= (format nil "(with-open-file (s p)~%  body)")
               (ind (format nil "(with-open-file (s p)~%body)"))))
  (is (string= (format nil "(define-foo bar (x)~%  body)")
               (ind (format nil "(define-foo bar (x)~%body)")))))

;;; ---- comments ----

(test indent-comments
  "A comment-only line gets the structural column and is transparent to accounting
(the body after it stays at the body column, not the distinguished column)."
  (is (string= (format nil "(progn~%  ;; note~%  body)")
               (ind (format nil "(progn~%;; note~%body)")))))

;;; ---- tabs ----

(test indent-tabs-out
  "Leading tabs are rewritten to spaces; a mid-line tab is preserved verbatim."
  (is (string= (format nil "(progn~%  body)")
               (ind (format nil "(progn~%~Cbody)" #\Tab))))
  (let ((got (ind (format nil "(progn~%~C(a~Cb))" #\Tab #\Tab))))
    (is (search (format nil "a~Cb" #\Tab) got) "mid-line tab lost: ~s" got)
    (is (search (format nil "~%  (a") got) "leading tab not spaced: ~s" got)))

;;; ---- multiline strings ----

(test indent-multiline-string-untouched
  "Newlines inside a string are part of one atom; never reindented."
  (let ((s (format nil "(defun f ()~%  \"line1~%line2\"~%  body)")))
    (is (string= s (ind s)))))

;;; ---- scoped :lines ----

(test indent-scoped-lines
  "Only in-range lines are rewritten; out-of-range whitespace is verbatim."
  (is (string= (format nil "(progn~%foo~%  bar)")
               (ind (format nil "(progn~%foo~%bar)") :lines '(3 . 3)))))

(test indent-scoped-multiple-ranges
  "A list of ranges reindents each disjoint region in one pass; gaps stay verbatim."
  (is (string= (format nil "(progn~%  a~%zz~%  b)")
               (ind (format nil "(progn~%a~%zz~%b)") :lines '((2 . 2) (4 . 4))))))

;;; ---- totality ----

(test indent-total
  "Degenerate / unbalanced input never errors."
  (dolist (s (list "" "(((" ")" "a" (format nil "~%~%") "(foo"))
    (finishes (ind s))))

;;; ---- loop clause machine (SLIME geometry) ----

(def-suite loop-indent-suite :description "loop clause machine" :in indent-suite)
(in-suite loop-indent-suite)

(test indent-loop-canonical-identity
  "Canonical SLIME-indented loop forms are fixed points."
  (dolist (s (list
              (format nil "(loop for x in xs~%      collect x)")
              (format nil "(loop for i from 0 below 10~%      for x in list~%      when (evenp i)~%        collect x~%      else~%        collect (- x)~%      finally (return result))")
              (format nil "(loop for x~%        from 0~%      collect x)")
              (format nil "(loop for x in xs~%      do (foo x)~%         (bar x))")
              (format nil "(loop for x in xs~%      do~%         (foo x))")
              (format nil "(loop for x in xs~%      finally~%         (return r))")
              (format nil "(loop (foo)~%      (bar))")
              (format nil "(loop~%  (foo)~%  (bar))")
              (format nil "(loop~%  for x in xs~%  collect x)")))
    (is (string= s (ind s)) "not a fixed point: ~s -> ~s" s (ind s))))

(test indent-loop-clause-and-conditional
  "Clause keywords at the clause column; conditional consequents +2; else aligns."
  (is (string= (format nil "(loop for i from 0 below 10~%      when (evenp i)~%        collect i~%      else~%        collect (- i)~%      finally (return r))")
               (ind (format nil "(loop for i from 0 below 10~%when (evenp i)~%collect i~%else~%collect (- i)~%finally (return r))")))))

(test indent-loop-subclause
  "An indented-subclause keyword indents one step past the clause column."
  (is (string= (format nil "(loop for x~%        from 0~%      collect x)")
               (ind (format nil "(loop for x~%from 0~%collect x)")))))

(test indent-loop-body-introducing
  "do/finally with a form on the keyword line align continuations under that form;
a body-introducing keyword alone on its line indents bodies at keyword-col + 3."
  (is (string= (format nil "(loop for x in xs~%      do (foo x)~%         (bar x))")
               (ind (format nil "(loop for x in xs~%do (foo x)~%(bar x))"))))
  (is (string= (format nil "(loop for x in xs~%      finally~%         (return r))")
               (ind (format nil "(loop for x in xs~%      finally~%(return r))")))))

(test indent-loop-simple
  "A simple loop (first clause is a list) bodies at open+6; split form at open+2."
  (is (string= (format nil "(loop (foo)~%      (bar))")
               (ind (format nil "(loop (foo)~%(bar))"))))
  (is (string= (format nil "(loop~%  (foo)~%  (bar))")
               (ind (format nil "(loop~%(foo)~%(bar))")))))

(test indent-loop-clause-joining-and
  "A clause-joining `and` at line start steals the matching clause's column."
  (is (string= (format nil "(loop for x in xs~%      when (p x)~%        collect x~%        and collect (f x))")
               (ind (format nil "(loop for x in xs~%when (p x)~%collect x~%and collect (f x))")))))

(test indent-loop-nested-form-default
  "A line nested deeper than a direct loop child uses the default rule, not loop."
  (let ((s (format nil "(loop for x in xs~%      collect (list x~%                    (g x)))")))
    (is (string= s (ind s)))))
