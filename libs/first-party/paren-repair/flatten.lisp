;;;; Tree -> source. A :synthesized close emits ")".

(in-package :paren-repair)

(defun %flatten-node (node out)
  (etypecase node
    (leaf (write-string (tok-text (leaf-tok node)) out))
    (group
     (write-string (tok-text (group-open node)) out)
     (dolist (child (group-children node)) (%flatten-node child out))
     (let ((close (group-close node)))
       (write-string (if (eq close :synthesized) ")" (tok-text close)) out)))))

(defun flatten (nodes)
  "Concatenate the source text of NODES (a list of tree nodes)."
  (with-output-to-string (out)
    (dolist (node nodes) (%flatten-node node out))))
