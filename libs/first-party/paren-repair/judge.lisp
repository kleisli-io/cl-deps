;;;; SBCL reader = balance authority. Hardened: *read-eval* nil, throwaway
;;;; package, #.-neutralized readtable.

(in-package :paren-repair)

(defvar *scratch-package*
  (or (find-package :paren-repair/read-scratch)
      (make-package :paren-repair/read-scratch :use nil)))

(defvar *judge-readtable*
  (let ((rt (copy-readtable nil)))
    ;; #. under *read-eval* nil errors and would misflag a balanced file.
    ;; Consume the form (keeps balance / real EOF) but never evaluate.
    (set-dispatch-macro-character #\# #\.
      (lambda (stream char arg)
        (declare (ignore char arg))
        (read stream t nil t)
        :paren-repair/read-eval-placeholder)
      rt)
    rt))

(defun delimiter-imbalanced-p (string)
  "True when STRING's real parens (parens-only lexer) don't nest back to zero —
an extra closer at some prefix, or an unclosed opener at the end. The authority
on whether a read failure is actually a delimiter problem."
  (let ((depth 0))
    (dolist (tok (tokenize string) (plusp depth))
      (case (tok-type tok)
        (:open (incf depth))
        (:close (when (minusp (decf depth))
                  (return-from delimiter-imbalanced-p t)))))))

(defun reader-verdict (string)
  "Balance verdict: :clean | :unbalanced | :indeterminate.
The reader decides clean vs. not. On a read failure the parens-only lexer is the
authority on the cause: genuinely mismatched parens are :unbalanced; everything
else — stray reader syntax (a bare comma, malformed #-dispatch), an unterminated
token, an unknown package — is :indeterminate, not a repair target. Clause order
matters: a package-error reads :indeterminate even when parens also differ."
  (handler-case
      (let ((*read-eval* nil)
            (*package* *scratch-package*)
            (*readtable* *judge-readtable*))
        (with-input-from-string (in string)
          (loop for f = (read in nil :eof) until (eq f :eof)))
        :clean)
    (package-error () :indeterminate)
    (error () (if (delimiter-imbalanced-p string) :unbalanced :indeterminate))))

(defun read-failure (string)
  "Re-read STRING under the judge readtable; on the first read error return
(values condition offset), OFFSET the 0-based position the reader reached. NIL
when STRING reads clean."
  (let ((*read-eval* nil)
        (*package* *scratch-package*)
        (*readtable* *judge-readtable*))
    (with-input-from-string (in string)
      (handler-case
          (progn (loop for f = (read in nil :eof) until (eq f :eof)) nil)
        (error (c) (values c (file-position in)))))))
