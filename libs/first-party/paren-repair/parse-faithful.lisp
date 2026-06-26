;;;; Balance-based tree builder: open on :open, close on the matching :close,
;;;; columns ignored — so a balanced file reads exactly, unlike indent-mode PARSE.

(in-package :paren-repair)

(defstruct (%fbuilder (:constructor %mk-fbuilder (open)))
  open (children-rev '()))

(defun parse-faithful (tokens)
  "Balance-based tree of TOKENS; root nodes as a list. ws/comment/newline are
interspersed leaves, so FLATTEN round-trips byte-exact. Unmatched :open at EOF
closes :synthesized, a root-level :close becomes a leaf — total on any input."
  (let* ((root (%mk-fbuilder nil))
         (stack (list root)))
    (labels ((top () (car stack))
             (push-child (node) (push node (%fbuilder-children-rev (top))))
             (close-top (close)
               (let ((b (pop stack)))
                 (push-child (make-group (%fbuilder-open b)
                                         (nreverse (%fbuilder-children-rev b))
                                         close)))))
      (dolist (tk tokens)
        (ecase (tok-type tk)
          (:open (push (%mk-fbuilder tk) stack))
          (:close
           (if (cdr stack)
               (close-top tk)
               (push-child (make-leaf tk))))
          ((:atom :ws :comment :newline) (push-child (make-leaf tk)))))
      (loop while (cdr stack) do (close-top :synthesized))
      (nreverse (%fbuilder-children-rev root)))))
