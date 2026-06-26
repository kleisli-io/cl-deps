;;;; REPLACE-SEXP: locate a surface-structural match and splice NEW over its
;;;; char span. Untouched regions stay byte-exact (no reindentation).

(in-package :paren-repair)

(defun %edit-span (entry spans new)
  "Absolute (start . end) to replace for match ENTRY. A delete (NEW \"\") also
consumes one adjacent whitespace separator so no double-space is left behind."
  (destructuring-bind (node siblings index) entry
    (let* ((s (gethash node spans))
           (start (car s))
           (end (cdr s)))
      (if (string= new "")
          (let ((after (nth (1+ index) siblings))
                (before (and (plusp index) (nth (1- index) siblings))))
            (cond ((and after (ws-leaf-p after))
                   (cons start (cdr (gethash after spans))))
                  ((and before (ws-leaf-p before))
                   (cons (car (gethash before spans)) end))
                  (t (cons start end))))
          (cons start end)))))

(defun %splice (source edits new)
  "Replace each disjoint (start . end) span in EDITS with NEW, right-to-left so
earlier offsets stay valid."
  (let ((result source))
    (dolist (e (sort (copy-list edits) #'> :key #'car) result)
      (setf result (concatenate 'string
                                (subseq result 0 (car e))
                                new
                                (subseq result (cdr e)))))))

(defun replace-sexp (source match new &key within-type within-name replace-all)
  "Splice NEW over the node SNODE= to MATCH in SOURCE. Returns (values new-source
status info):
  :ok                 new-source spliced; info = replaced spans (in SOURCE)
  :bad-match          MATCH is not exactly one form
  :not-found          no match in scope
  :ambiguous          >1 match and not REPLACE-ALL; info = candidate spans
  :within-not-found   WITHIN scope matched no top-level form
  :within-ambiguous   WITHIN scope matched >1 form; info = candidate spans
MATCH is surface-structural; NEW is spliced verbatim. WITHIN-TYPE/WITHIN-NAME
scope the search to one top-level form."
  (let* ((roots (parse-faithful (tokenize source)))
         (spans (span-map roots))
         (pattern-nodes (significant-roots (parse-faithful (tokenize match)))))
    (unless (= 1 (length pattern-nodes))
      (return-from replace-sexp (values nil :bad-match nil)))
    (let ((pattern (first pattern-nodes))
          (scope-children roots))
      (when (or within-type within-name)
        (multiple-value-bind (form status wspans)
            (resolve-within roots within-type within-name spans)
          (ecase status
            (:not-found (return-from replace-sexp
                          (values nil :within-not-found nil)))
            (:ambiguous (return-from replace-sexp
                          (values nil :within-ambiguous wspans)))
            (:ok (setf scope-children (group-children form))))))
      (let ((hits (find-matches scope-children pattern)))
        (cond
          ((null hits) (values nil :not-found nil))
          ((and (cdr hits) (not replace-all))
           (values nil :ambiguous
                   (mapcar (lambda (e) (gethash (first e) spans)) hits)))
          (t (let ((edits (mapcar (lambda (e) (%edit-span e spans new)) hits)))
               (values (%splice source edits new) :ok edits))))))))
