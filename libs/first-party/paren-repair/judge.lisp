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

(defun reader-verdict (string)
  "Balance verdict: :clean | :unbalanced | :indeterminate.
end-of-file = missing closers; reader-error = extra/unmatched; package/other =
not a delimiter problem. Clause order matters."
  (handler-case
      (let ((*read-eval* nil)
            (*package* *scratch-package*)
            (*readtable* *judge-readtable*))
        (with-input-from-string (in string)
          (loop for f = (read in nil :eof) until (eq f :eof)))
        :clean)
    (end-of-file () :unbalanced)
    (package-error () :indeterminate)
    (reader-error () :unbalanced)
    (error () :indeterminate)))
