;;;; Surface-structural matching over PARSE-FAITHFUL trees: paren shape + verbatim
;;;; atom texts, whitespace/comments ignored. Addresses nodes by absolute char
;;;; span and scopes searches to one top-level form.

(in-package :paren-repair)

(defun ws-leaf-p (node)
  (and (leaf-p node)
       (member (tok-type (leaf-tok node)) '(:ws :newline :comment))))

(defun significant-children (group)
  (remove-if #'ws-leaf-p (group-children group)))

(defun significant-roots (nodes)
  (remove-if #'ws-leaf-p nodes))

(defun atom-text (node)
  (and (leaf-p node) (tok-text (leaf-tok node))))

(defun snode= (a b)
  "Surface-structural equality: group-open text + significant children
recursively, atoms by verbatim text. Whitespace and comments are ignored."
  (cond
    ((and (leaf-p a) (leaf-p b))
     (string= (tok-text (leaf-tok a)) (tok-text (leaf-tok b))))
    ((and (group-p a) (group-p b))
     (and (string= (tok-text (group-open a)) (tok-text (group-open b)))
          (let ((ca (significant-children a))
                (cb (significant-children b)))
            (and (= (length ca) (length cb))
                 (every #'snode= ca cb)))))
    (t nil)))

(defun span-map (roots)
  "Hash-table node -> (start . end) absolute char offsets, summed from token
texts in document order — consistent with FLATTEN by construction."
  (let ((table (make-hash-table :test #'eq))
        (pos 0))
    (labels ((walk (node)
               (let ((start pos))
                 (etypecase node
                   (leaf (incf pos (length (tok-text (leaf-tok node)))))
                   (group
                    (incf pos (length (tok-text (group-open node))))
                    (dolist (c (group-children node)) (walk c))
                    (incf pos (length (if (eq (group-close node) :synthesized)
                                          ")"
                                          (tok-text (group-close node)))))))
                 (setf (gethash node table) (cons start pos)))))
      (dolist (n roots) (walk n)))
    table))

(defun map-nodes (siblings fn)
  "Call FN with (node siblings index) for every node under SIBLINGS, pre-order."
  (loop for index from 0
        for node in siblings
        do (funcall fn node siblings index)
           (when (group-p node)
             (map-nodes (group-children node) fn))))

(defun find-matches (scope-children pattern)
  "Entries (node siblings index) for every node SNODE= to PATTERN, in document
order. Matches are never nested (equality forces equal size), so spans disjoin."
  (let ((hits '()))
    (map-nodes scope-children
               (lambda (node siblings index)
                 (when (snode= node pattern)
                   (push (list node siblings index) hits))))
    (nreverse hits)))

(defun group-head (group)
  (first (significant-children group)))

(defun top-forms (roots)
  (remove-if-not #'group-p roots))

(defun within-form-match-p (form within-type name-nodes)
  "FORM matches when its head atom = WITHIN-TYPE (when given) and its significant
children after the head begin with NAME-NODES surface-structurally (when given)."
  (let ((sig (significant-children form)))
    (and (or (null within-type)
             (let ((head (atom-text (first sig))))
               (and head (string= head within-type))))
         (or (null name-nodes)
             (let ((after (rest sig)))
               (and (>= (length after) (length name-nodes))
                    (every #'snode= after name-nodes)))))))

(defun resolve-within (roots within-type within-name spans)
  "The unique top-level form scoped by WITHIN-TYPE / WITHIN-NAME. Returns (values
group status candidate-spans): :ok with the form, else :not-found / :ambiguous."
  (let* ((name-nodes (when within-name
                       (significant-roots
                        (parse-faithful (tokenize within-name)))))
         (matches (loop for form in (top-forms roots)
                        when (within-form-match-p form within-type name-nodes)
                        collect form)))
    (cond ((null matches) (values nil :not-found nil))
          ((cdr matches)
           (values nil :ambiguous
                   (mapcar (lambda (f) (gethash f spans)) matches)))
          (t (values (first matches) :ok nil)))))

(defun source-line-col (source offset)
  "1-based line and column of OFFSET within SOURCE."
  (let ((line 1) (col 1))
    (dotimes (i (min offset (length source)) (values line col))
      (if (char= (char source i) #\Newline)
          (setf line (1+ line) col 1)
          (incf col)))))
