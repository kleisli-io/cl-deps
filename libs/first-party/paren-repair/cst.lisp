;;;; Lossless Common Lisp reader-syntax CST over source text.

(in-package :paren-repair)

(defstruct (cst-node (:constructor make-cst-node (kind start end &key children)))
  kind start end (children '()))

(defstruct (cst-trivia (:constructor make-cst-trivia (kind start end)))
  kind start end)

(defstruct (cst-diagnostic (:constructor make-cst-diagnostic (kind start end message)))
  kind start end message)

(defstruct (cst-parse (:constructor make-cst-parse (forms trivia diagnostics)))
  forms trivia diagnostics)

(defun %cst-whitespace-p (ch)
  (member ch '(#\Space #\Tab #\Return #\Page #\Newline)))

(defun %cst-token-terminator-p (ch)
  (or (%cst-whitespace-p ch)
      (member ch '(#\( #\) #\" #\' #\` #\, #\;))))

(defun %cst-scan-string (source start)
  (let ((i (1+ start))
        (n (length source)))
    (loop while (< i n) do
      (let ((ch (char source i)))
        (cond ((char= ch #\\) (incf i 2))
              ((char= ch #\") (incf i) (return))
              (t (incf i)))))
    (min i n)))

(defun %cst-scan-bar (source start)
  (let ((i (1+ start))
        (n (length source)))
    (loop while (< i n) do
      (let ((ch (char source i)))
        (cond ((char= ch #\\) (incf i 2))
              ((char= ch #\|) (incf i) (return))
              (t (incf i)))))
    (min i n)))

(defun %cst-scan-token (source start)
  (let ((i start)
        (n (length source)))
    (loop while (< i n) do
      (let ((ch (char source i)))
        (cond
          ((%cst-token-terminator-p ch) (return))
          ((char= ch #\\) (setf i (min n (+ i 2))))
          ((char= ch #\|) (setf i (%cst-scan-bar source i)))
          (t (incf i)))))
    (max (1+ start) i)))

(defun %cst-alnum-p (ch)
  (or (alpha-char-p ch) (digit-char-p ch)))

(defun %cst-scan-character (source start)
  (let ((i (+ start 2))
        (n (length source)))
    (when (< i n) (incf i))
    (loop while (and (< i n) (%cst-alnum-p (char source i))) do (incf i))
    i))

(defun %cst-scan-block-comment-end (source start)
  (let ((i (+ start 2))
        (n (length source))
        (depth 1))
    (loop while (and (< i n) (> depth 0)) do
      (cond ((and (< (1+ i) n)
                  (char= (char source i) #\#)
                  (char= (char source (1+ i)) #\|))
             (incf depth)
             (incf i 2))
            ((and (< (1+ i) n)
                  (char= (char source i) #\|)
                  (char= (char source (1+ i)) #\#))
             (decf depth)
             (incf i 2))
            (t (incf i))))
    (values i (zerop depth))))

(defun parse-cst (source)
  "Parse SOURCE into a lossless reader-syntax CST.
The parser recognizes Common Lisp source syntax without package lookup,
read-time evaluation, feature evaluation, or readtable-case normalization.
Whitespace and comments are returned as trivia rather than form nodes."
  (let ((n (length source))
        (i 0)
        (forms '())
        (trivia '())
        (diagnostics '()))
    (labels
        ((peek (&optional (offset 0))
           (let ((pos (+ i offset)))
             (when (< pos n) (char source pos))))
         (add-diagnostic (kind start end message)
           (push (make-cst-diagnostic kind start end message) diagnostics))
         (add-trivia (kind start end)
           (push (make-cst-trivia kind start end) trivia))
         (skip-trivia ()
           (loop while (< i n) do
             (let ((ch (char source i)))
               (cond
                 ((%cst-whitespace-p ch)
                  (let ((start i))
                    (loop while (and (< i n) (%cst-whitespace-p (char source i)))
                          do (incf i))
                    (add-trivia :whitespace start i)))
                 ((char= ch #\;)
                  (let ((start i))
                    (loop while (and (< i n) (char/= (char source i) #\Newline))
                          do (incf i))
                    (add-trivia :line-comment start i)))
                 ((and (char= ch #\#) (char= (or (peek 1) #\Null) #\|))
                  (let ((start i))
                    (multiple-value-bind (end closed) (%cst-scan-block-comment-end source i)
                      (setf i end)
                      (add-trivia :block-comment start end)
                      (unless closed
                        (add-diagnostic :unterminated-block-comment start end
                                        "unterminated block comment")))))
                 (t (return))))))
         (parse-prefix (kind marker-length)
           (let ((start i))
             (incf i marker-length)
             (skip-trivia)
             (let ((child (parse-form)))
               (if child
                   (make-cst-node kind start (cst-node-end child)
                                  :children (list child))
                   (progn
                     (add-diagnostic :missing-form start i
                                     "reader prefix is missing its form")
                     (make-cst-node kind start i))))))
         (parse-feature (kind)
           (let ((start i))
             (incf i 2)
             (skip-trivia)
             (let ((feature (parse-form)))
               (skip-trivia)
               (let ((payload (parse-form)))
                 (cond ((and feature payload)
                        (make-cst-node kind start (cst-node-end payload)
                                       :children (list feature payload)))
                       (feature
                        (add-diagnostic :missing-form start (cst-node-end feature)
                                        "feature conditional is missing its payload form")
                        (make-cst-node kind start (cst-node-end feature)
                                       :children (list feature)))
                       (t
                        (add-diagnostic :missing-form start i
                                        "feature conditional is missing its feature expression")
                        (make-cst-node kind start i)))))))
         (parse-list (kind open-length)
           (let ((start i)
                 (children '()))
             (incf i open-length)
             (loop
               (skip-trivia)
               (cond
                 ((>= i n)
                  (add-diagnostic :unterminated-list start i
                                  "list is missing a closing delimiter")
                  (return))
                 ((char= (char source i) #\))
                  (incf i)
                  (return))
                 (t
                  (let ((child (parse-form)))
                    (when child (push child children))))))
             (make-cst-node kind start i :children (nreverse children))))
         (parse-dispatch ()
           (let* ((start i)
                  (j (1+ i)))
             (loop while (and (< j n) (digit-char-p (char source j))) do (incf j))
             (if (>= j n)
                 (progn
                   (setf i j)
                   (add-diagnostic :unknown-dispatch start i "incomplete dispatch macro")
                   (make-cst-node :unknown-dispatch start i))
                 (let ((dispatch (char-upcase (char source j))))
                   (cond
                     ((and (= j (1+ start)) (char= (char source j) #\())
                      (parse-list :vector 2))
                     ((and (= j (1+ start)) (char= (char source j) #\'))
                      (parse-prefix :function 2))
                     ((and (= j (1+ start)) (char= (char source j) #\.))
                      (parse-prefix :read-eval 2))
                     ((and (= j (1+ start)) (char= (char source j) #\+))
                      (parse-feature :feature-positive))
                     ((and (= j (1+ start)) (char= (char source j) #\-))
                      (parse-feature :feature-negative))
                     ((and (= j (1+ start)) (char= (char source j) #\:))
                      (let ((end (%cst-scan-token source start)))
                        (setf i end)
                        (make-cst-node :atom start end)))
                     ((and (= j (1+ start)) (char= (char source j) #\\))
                      (let ((end (%cst-scan-character source start)))
                        (setf i end)
                        (make-cst-node :character start end)))
                     ((member dispatch '(#\P #\C #\S))
                      (setf i (1+ j))
                      (parse-dispatch-payload
                       (case dispatch
                         (#\P :pathname)
                         (#\C :complex)
                         (#\S :structure))
                       start))
                     ((char= dispatch #\A)
                      (setf i (1+ j))
                      (parse-dispatch-payload :array start))
                     ((member dispatch '(#\B #\O #\X #\R))
                      (let ((end (%cst-scan-token source start)))
                        (setf i end)
                        (make-cst-node :radix-number start end)))
                     ((and (= j (1+ start)) (char= (char source j) #\*))
                      (let ((end (%cst-scan-token source start)))
                        (setf i end)
                        (make-cst-node :bit-vector start end)))
                     ((char= (char source j) #\=)
                      (setf i (1+ j))
                      (parse-dispatch-payload :label-definition start))
                     ((char= (char source j) #\#)
                      (setf i (1+ j))
                      (make-cst-node :label-reference start i))
                     (t
                      (parse-unknown-dispatch start)))))))
         (parse-dispatch-payload (kind start)
           (skip-trivia)
           (let ((child (parse-form)))
             (if child
                 (make-cst-node kind start (cst-node-end child)
                                :children (list child))
                 (progn
                   (add-diagnostic :missing-form start i
                                   "dispatch macro is missing its payload form")
                   (make-cst-node kind start i)))))
         (parse-unknown-dispatch (start)
           (let ((end (if (and (< (1+ start) n)
                               (char= (char source (1+ start)) #\<))
                          (let ((close (position #\> source :start (+ start 2))))
                            (if close (1+ close) (%cst-scan-token source start)))
                          (%cst-scan-token source start))))
             (setf i end)
             (add-diagnostic :unknown-dispatch start end "unknown dispatch macro")
             (make-cst-node :unknown-dispatch start end)))
         (parse-atom ()
           (let ((start i)
                 (end (%cst-scan-token source i)))
             (setf i end)
             (make-cst-node :atom start end)))
         (parse-form ()
           (skip-trivia)
           (when (< i n)
             (let ((ch (char source i)))
               (cond
                 ((char= ch #\() (parse-list :list 1))
                 ((char= ch #\))
                  (let ((start i))
                    (incf i)
                    (add-diagnostic :unmatched-close start i
                                    "unmatched closing delimiter")
                    (make-cst-node :unmatched-close start i)))
                 ((char= ch #\")
                  (let ((start i)
                        (end (%cst-scan-string source i)))
                    (setf i end)
                    (unless (and (> end start)
                                 (char= (char source (1- end)) #\"))
                      (add-diagnostic :unterminated-string start end
                                      "unterminated string literal"))
                    (make-cst-node :string start end)))
                 ((char= ch #\') (parse-prefix :quote 1))
                 ((char= ch #\`) (parse-prefix :quasiquote 1))
                 ((char= ch #\,)
                  (if (char= (or (peek 1) #\Null) #\@)
                      (parse-prefix :unquote-splicing 2)
                      (parse-prefix :unquote 1)))
                 ((char= ch #\#) (parse-dispatch))
                 (t (parse-atom)))))))
      (loop
        (skip-trivia)
        (when (>= i n) (return))
        (let ((form (parse-form)))
          (when form (push form forms))))
      (make-cst-parse (nreverse forms)
                      (nreverse trivia)
                      (nreverse diagnostics)))))

(defun source-form-count (source)
  "Return the number of top-level CST forms in SOURCE, ignoring trivia."
  (length (cst-parse-forms (parse-cst source))))

(defun source-syntax-diagnostics (source)
  "Common Lisp source-syntax CST diagnostics for SOURCE, in source order."
  (cst-parse-diagnostics (parse-cst source)))

(defun source-syntax-valid-p (source)
  "True when SOURCE is diagnostics-free Common Lisp source syntax."
  (null (source-syntax-diagnostics source)))

(defun source-position (source offset)
  "1-based line and column of OFFSET within SOURCE."
  (let ((line 1) (col 1))
    (dotimes (i (min offset (length source)) (values line col))
      (if (char= (char source i) #\Newline)
          (setf line (1+ line) col 1)
          (incf col)))))

(defun %cst-node-source-text (source node)
  (subseq source (cst-node-start node) (cst-node-end node)))

(defun cst-node= (left-source left right-source right)
  "CST structural equality over source syntax.
Trivia is absent from CST children, so whitespace and comments are ignored.
Node kind and significant child structure must match; leaf-like nodes compare by
their exact source token text without reader normalization or package lookup."
  (and (eq (cst-node-kind left) (cst-node-kind right))
       (let ((left-children (cst-node-children left))
             (right-children (cst-node-children right)))
         (if (or left-children right-children)
             (and (= (length left-children) (length right-children))
                  (every (lambda (left-child right-child)
                           (cst-node= left-source left-child right-source right-child))
                         left-children right-children))
             (string= (%cst-node-source-text left-source left)
                      (%cst-node-source-text right-source right))))))

(defun cst-map-nodes (siblings fn)
  "Call FN with (node siblings index) for every CST node under SIBLINGS."
  (loop for index from 0
        for node in siblings
        do (funcall fn node siblings index)
           (cst-map-nodes (cst-node-children node) fn)))

(defun cst-find-matches (source roots pattern-source pattern)
  "Entries (node siblings index) for nodes CST-NODE= to PATTERN, in source order."
  (let ((hits '()))
    (cst-map-nodes roots
                   (lambda (node siblings index)
                     (when (cst-node= source node pattern-source pattern)
                       (push (list node siblings index) hits))))
    (nreverse hits)))

(defun %cst-span (node)
  (cons (cst-node-start node) (cst-node-end node)))

(defun %cst-atom-text (source node)
  (when (eq :atom (cst-node-kind node))
    (%cst-node-source-text source node)))

(defun %cst-within-form-match-p (source form within-type name-source name-nodes)
  (let ((children (cst-node-children form)))
    (and (eq :list (cst-node-kind form))
         (or (null within-type)
             (let ((head (first children)))
               (and head (string= within-type (%cst-atom-text source head)))))
         (or (null name-nodes)
             (let ((after (rest children)))
               (and (>= (length after) (length name-nodes))
                    (every (lambda (node name-node)
                             (cst-node= source node name-source name-node))
                           after name-nodes)))))))

(defun %cst-resolve-within (source roots within-type within-name)
  "Return (values form status candidate-spans) for the CST top-level scope."
  (let* ((name-parse (when within-name (parse-cst within-name)))
         (name-nodes (when name-parse (cst-parse-forms name-parse))))
    (when (and name-parse (cst-parse-diagnostics name-parse))
      (return-from %cst-resolve-within
        (values nil :syntax-error (cst-parse-diagnostics name-parse))))
    (let ((matches (loop for form in roots
                         when (%cst-within-form-match-p source form within-type
                                                        within-name name-nodes)
                         collect form)))
      (cond ((null matches) (values nil :not-found nil))
            ((cdr matches) (values nil :ambiguous (mapcar #'%cst-span matches)))
            (t (values (first matches) :ok nil))))))

(defun %cst-whitespace-string-p (string)
  (loop for ch across string always (%cst-whitespace-p ch)))

(defun %cst-edit-span (source entry new)
  "Absolute source span for a CST match. Deletion consumes one adjacent whitespace separator."
  (destructuring-bind (node siblings index) entry
    (let ((start (cst-node-start node))
          (end (cst-node-end node)))
      (if (string= new "")
          (let ((after (nth (1+ index) siblings))
                (before (and (plusp index) (nth (1- index) siblings))))
            (cond ((and after
                        (%cst-whitespace-string-p
                         (subseq source end (cst-node-start after))))
                   (cons start (cst-node-start after)))
                  ((and before
                        (%cst-whitespace-string-p
                         (subseq source (cst-node-end before) start)))
                   (cons (cst-node-end before) end))
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

(defun replace-source-form (source match new &key within-type within-name replace-all)
  "Splice NEW over CST-structural matches of MATCH in SOURCE.
Returns values compatible with REPLACE-SEXP: new source, status, and status-specific
span or diagnostic info. Source, match, and non-empty replacement text are parsed
as Common Lisp source syntax without reader evaluation or package lookup."
  (let* ((source-parse (parse-cst source))
         (match-parse (parse-cst match))
         (new-parse (unless (string= new "") (parse-cst new))))
    (when (cst-parse-diagnostics source-parse)
      (return-from replace-source-form
        (values nil :syntax-error (cst-parse-diagnostics source-parse))))
    (when (cst-parse-diagnostics match-parse)
      (return-from replace-source-form
        (values nil :syntax-error (cst-parse-diagnostics match-parse))))
    (when (and new-parse (cst-parse-diagnostics new-parse))
      (return-from replace-source-form
        (values nil :syntax-error (cst-parse-diagnostics new-parse))))
    (unless (= 1 (length (cst-parse-forms match-parse)))
      (return-from replace-source-form (values nil :bad-match nil)))
    (let ((pattern (first (cst-parse-forms match-parse)))
          (scope-children (cst-parse-forms source-parse)))
      (when (or within-type within-name)
        (multiple-value-bind (form status wspans)
            (%cst-resolve-within source (cst-parse-forms source-parse)
                                 within-type within-name)
          (ecase status
            (:syntax-error (return-from replace-source-form
                             (values nil :syntax-error wspans)))
            (:not-found (return-from replace-source-form
                          (values nil :within-not-found nil)))
            (:ambiguous (return-from replace-source-form
                          (values nil :within-ambiguous wspans)))
            (:ok (setf scope-children (cst-node-children form))))))
      (let ((hits (cst-find-matches source scope-children match pattern)))
        (cond
          ((null hits) (values nil :not-found nil))
          ((and (cdr hits) (not replace-all))
           (values nil :ambiguous (mapcar (lambda (hit) (%cst-span (first hit))) hits)))
          (t (let ((edits (mapcar (lambda (hit) (%cst-edit-span source hit new)) hits)))
               (values (%splice source edits new) :ok edits))))))))
