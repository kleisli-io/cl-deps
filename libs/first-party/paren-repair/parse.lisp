;;;; Parinfer indent-mode tree builder. A paren opened at column C closes before
;;;; the next line whose first token sits at column <= C; extra closers dropped;
;;;; EOF closes the rest. LEAF = one token; GROUP = open/children/close, close
;;;; being the real token or :synthesized (an inserted ")").

(in-package :paren-repair)

(defstruct (leaf (:constructor make-leaf (tok)))
  tok)

(defstruct (group (:constructor make-group (open children close)))
  open children close)

(defun group-synthesized-p (group)
  (eq (group-close group) :synthesized))

(defstruct (%builder (:constructor %mk-builder (open col)))
  open (children-rev '()) col)

(defun parse (tokens)
  "Build the repaired tree from TOKENS; return the list of root nodes."
  (let* ((root (%mk-builder nil -1))
         (stack (list root))
         (trail '())
         (at-line-start t))
    (labels ((top () (car stack))
             (push-child (node) (push node (%builder-children-rev (top))))
             (flush-trail ()
               (dolist (node (nreverse trail)) (push-child node))
               (setf trail '()))
             (close-top (close)
               ;; closers precede pending trail: attach without flushing
               (let ((b (pop stack)))
                 (push-child (make-group (%builder-open b)
                                         (nreverse (%builder-children-rev b))
                                         close))))
             (dedent-check (col)
               (when at-line-start
                 (loop while (and (cdr stack) (>= (%builder-col (top)) col))
                       do (close-top :synthesized))
                 (setf at-line-start nil))))
      (dolist (tk tokens)
        (let ((type (tok-type tk)) (col (tok-col tk)))
          (ecase type
            (:open
             (dedent-check col)
             (flush-trail)
             (push (%mk-builder tk col) stack))
            (:close
             (dedent-check col)
             (when (cdr stack)
               (flush-trail)        ; real close keeps trail as inner children
               (close-top tk)))
            (:atom
             (dedent-check col)
             (flush-trail)
             (push-child (make-leaf tk)))
            (:ws      (push (make-leaf tk) trail))
            (:comment (push (make-leaf tk) trail))
            (:newline (push (make-leaf tk) trail) (setf at-line-start t)))))
      (loop while (cdr stack) do (close-top :synthesized))
      (flush-trail)
      (nreverse (%builder-children-rev root)))))
