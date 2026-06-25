;;;; Top-level API: balanced-p, repair, repair-if-needed.

(in-package :paren-repair)

(defun balanced-p (string)
  "True when the reader judges STRING balanced."
  (eq (reader-verdict string) :clean))

(defun repair (string)
  "Rebalance STRING with parinfer indent-mode. Pure transform; callers gate."
  (flatten (parse (tokenize string))))

(defun repair-if-needed (string)
  "Return (values new-string changed-p). Repairs only when STRING reads
unbalanced AND the result reads clean AND differs; else returns STRING / NIL."
  (if (eq (reader-verdict string) :unbalanced)
      (let ((fixed (repair string)))
        (if (and (eq (reader-verdict fixed) :clean) (string/= fixed string))
            (values fixed t)
            (values string nil)))
      (values string nil)))
