;;;; INDENT-REGION: rewrite each line's leading whitespace to canonical CL
;;;; indentation. One forward pass over TOKENIZE with a frame stack; only
;;;; :newline trailing whitespace is rewritten, every other byte preserved.
;;;; Columns/lines are tracked dynamically (not from TOK-COL): reindenting a
;;;; line shifts the open parens on it, which anchor the lines nested inside.

(in-package :paren-repair)

(defstruct (iframe (:constructor mk-iframe (open-col open-line)))
  open-col open-line head
  (sig-count 0) arg1-col arg1-line arg1-type
  (qualifiers 0) lambda-seen
  lrecs)

(defparameter *indent-table*
  (let ((h (make-hash-table :test #'equal)))
    (dolist (name '("progn" "return" "with-standard-io-syntax" "cond"))
      (setf (gethash name h) 0))
    (dolist (name '("block" "catch" "eval-when" "locally" "multiple-value-prog1"
                    "prog1" "throw" "unless" "when"
                    "let" "let*" "symbol-macrolet" "compiler-let" "handler-bind"
                    "restart-bind" "flet" "labels" "macrolet" "generic-flet"
                    "generic-labels" "dolist" "dotimes" "multiple-value-call"
                    "return-from" "unwind-protect" "with-compilation-unit"
                    "defvar" "defparameter" "defconstant" "defcustom" "defconst"
                    "defpackage" "multiple-value-setq" "multiple-value-setf"
                    "with-output-to-string" "pprint-logical-block"
                    "case" "ccase" "ecase" "typecase" "etypecase" "ctypecase"
                    "handler-case" "restart-case" "lambda" "named-lambda"
                    "prog" "prog*" "tagbody"))
      (setf (gethash name h) 1))
    (dolist (name '("defun" "defmacro" "defsubst" "deftype" "defgeneric"
                    "define-compiler-macro" "define-modify-macro"
                    "define-setf-method" "define-setf-expander"
                    "destructuring-bind" "multiple-value-bind" "with-slots"
                    "with-accessors" "with-condition-restarts" "progv" "prog2"
                    "defclass" "define-condition" "do" "do*" "defsetf"))
      (setf (gethash name h) 2))
    h)
  "Head name -> distinguished-arg count N (= position of the body), derived from
the cl-indent/SLIME standard methods. DEFMETHOD is dynamic; see IFRAME-N. LOOP is
not here: it rides its own clause machine (COMPUTE-LOOP-INDENT).")

(defun normalize-head (text)
  "Downcase TEXT and drop any package qualifier (through the last colon)."
  (let ((colon (position #\: text :from-end t)))
    (string-downcase (if colon (subseq text (1+ colon)) text))))

(defun head-prefix-p (prefix h)
  (let ((lp (length prefix)))
    (and (>= (length h) lp) (string= prefix h :end2 lp))))

(defun name-heuristic (h)
  "cl-indent's name fallback for unregistered heads: def... -> N2;
with-/without-/do-... -> N1. NIL when neither matches."
  (cond ((head-prefix-p "def" h) 2)
        ((or (head-prefix-p "with-" h)
             (head-prefix-p "without-" h)
             (head-prefix-p "do-" h))
         1)))

(defun iframe-n (f)
  "Distinguished-arg count for F's head, or :DEFAULT for the align-under-arg1 rule."
  (let ((h (iframe-head f)))
    (cond ((null h) :default)
          ((string= h "defmethod") (+ 2 (iframe-qualifiers f)))
          ((gethash h *indent-table*))
          ((name-heuristic h))
          (t :default))))

;;;; ---- LOOP clause machine (SLIME common-lisp-indent-loop-macro-1) ----
;;;; Keyword sets are SLIME's verbatim, matched case-insensitively against the
;;;; package-stripped atom. Geometry constants (2/3/6) are SLIME's, not STEP.

(defparameter *loop-body-introducing* '("do" "doing" "finally" "initially"))
(defparameter *loop-prefix* '("and" "else"))
(defparameter *loop-conditional* '("when" "unless" "if"))
(defparameter *loop-indented-subclause*
  '("from" "upfrom" "downfrom" "to" "upto" "downto" "below" "above"
    "in" "into" "on" "=" "then" "across" "being" "each" "the" "of"
    "using" "symbol" "symbols" "present-symbol" "present-symbols"
    "external-symbol" "external-symbols" "fixnum" "float" "t" "nil" "of-type"))

(defun loop-kw-p (h set) (and h (member h set :test #'string=)))

;; A content line is recorded as (LINE-NO . reversed-token-recs); each token-rec
;; is (COL TEXT TYPE). Within COMPUTE-LOOP-INDENT lines run forward.
(defun lrec-col (r) (first r))
(defun lrec-type (r) (third r))
(defun loop-line-toks (line) (reverse (cdr line)))
(defun loop-norm (r)
  (and (eq (lrec-type r) :atom) (normalize-head (second r))))
(defun tok-after (rec toks) (cadr (member rec toks :test #'eq)))

(defun skip-loop-prefixes (toks)
  "Advance past AND/ELSE prefixes that have a following token on the line;
return the effective keyword token-rec (the prefix itself if it stands alone)."
  (loop for cur = (first toks)
        for h = (loop-norm cur)
        do (if (and (loop-kw-p h *loop-prefix*) (cdr toks))
               (setf toks (cdr toks))
               (return cur))))

(defun steal-loop-and-col (lines body-indent)
  "Clause-joining AND at line start: scan content lines upward for a keyword whose
column < BODY-INDENT; steal it (advancing past a leading ELSE). NIL if none."
  (dolist (line lines nil)
    (let* ((toks (loop-line-toks line))
           (lf (first toks))
           (col (lrec-col lf))
           (h (loop-norm lf)))
      (when (and h (< col body-indent))
        (return (if (string= h "else")
                    (let ((a (tok-after lf toks))) (if a (lrec-col a) col))
                    col))))))

(defun compute-loop-indent (frame next)
  "Target column for a line opening directly inside a LOOP frame; NEXT is that
line's first token. Ports SLIME's lisp-indent-loop: simple/simple-split bodies,
plus the extended clause machine (clause column, subclause +2, conditional +2,
body-introducing, clause-joining AND)."
  (let* ((open (iframe-open-col frame))
         (oline (iframe-open-line frame))
         (a1col (iframe-arg1-col frame))
         (a1line (iframe-arg1-line frame))
         (a1type (iframe-arg1-type frame))
         (clause-col (if (and a1col (= a1line oline)) a1col (+ open 2))))
    (cond
      ;; Simple loop: the first clause is a list, not a keyword.
      ((eq a1type :group)
       (if (and a1line (= a1line oline)) (+ open 6) (+ open 2)))
      ;; Extended loop -> clause machine.
      (t
       (let ((lines (iframe-lrecs frame)))
         (if (null lines)
             clause-col                 ; no clause seen yet (SLIME cop-out)
             (let* ((m-toks (loop-line-toks (car lines)))
                    (pe (car (last m-toks)))      ; previous expression
                    (lf (first m-toks))           ; first keyword on that line
                    (loop-body-p nil)
                    (body-indent nil)
                    (default-value clause-col)
                    (indented-clause (+ clause-col 2))
                    (pe-kw (loop-norm pe)))
               ;; Context from the most-recent content line.
               (if (loop-kw-p pe-kw *loop-body-introducing*)
                   ;; A body-introducing keyword standing alone at end of line.
                   (let ((kwpos (lrec-col pe)) (lfcol (lrec-col lf)))
                     (setf loop-body-p t
                           body-indent (if (/= lfcol kwpos) (+ lfcol 2) (+ kwpos 3))))
                   (let* ((ek (skip-loop-prefixes m-toks))
                          (ek-text (loop-norm ek)))
                     (cond
                       ((eq (lrec-type ek) :group)
                        (setf loop-body-p t body-indent (lrec-col ek)))
                       ((loop-kw-p ek-text *loop-body-introducing*)
                        (let ((a (tok-after ek m-toks)))
                          (setf loop-body-p t
                                body-indent (if a (lrec-col a) (+ (lrec-col ek) 3)))))
                       (t
                        (when (or (loop-kw-p ek-text *loop-conditional*)
                                  (loop-kw-p ek-text *loop-prefix*))
                          (setf default-value (+ (lrec-col ek) 2)))
                        (setf indented-clause (+ (lrec-col ek) 2)
                              body-indent (lrec-col pe))))))
               ;; Classify the line being indented.
               (let ((nl (and (eq (tok-type next) :atom)
                              (normalize-head (tok-text next)))))
                 (cond
                   ((eq (tok-type next) :open) body-indent)
                   ((eq (tok-type next) :comment)
                    (if loop-body-p body-indent default-value))
                   ((loop-kw-p nl *loop-indented-subclause*) indented-clause)
                   ((and nl (string= nl "and"))
                    (or (steal-loop-and-col lines body-indent) default-value))
                   (t default-value))))))))))

(defun compute-indent (frame step next)
  "Target column for a line opening inside FRAME (NIL = top level -> 0). NEXT is
the line's first token, used only by the LOOP clause machine."
  (cond
    ((null frame) 0)
    ((equal (iframe-head frame) "loop") (compute-loop-indent frame next))
    (t
     (let ((n (iframe-n frame))
           (open (iframe-open-col frame)))
       (if (eq n :default)
           (if (and (iframe-arg1-col frame)
                    (= (iframe-arg1-line frame) (iframe-open-line frame)))
               (iframe-arg1-col frame)
               (1+ open))
           (let ((p (iframe-sig-count frame)))
             (if (<= 1 p n)
                 (+ open (* 2 step))
                 (+ open step))))))))

(defun indent-region (source &key lines (step 2))
  "Return SOURCE with each line's leading whitespace rewritten to canonical CL
indentation. LINES, when non-NIL, restricts which lines are rewritten; lines
outside keep their whitespace verbatim. It is a 1-based inclusive (MIN . MAX)
range, or a list of such ranges (for several disjoint regions in one pass — line
numbers are stable since reindentation never adds or removes lines). STEP is the
per-level width (default 2). Reindented lines emit spaces only; column accounting
expands tabs to 8-col stops. Total on any input."
  (let ((ranges (cond ((null lines) nil)
                      ((consp (car lines)) lines)
                      (t (list lines))))
        (toks (tokenize source))
        (out (make-string-output-stream))
        (stack '())
        (cur-line 0) (cur-col 0))
    (labels ((advance (text)
               (loop for ch across text do
                 (cond ((char= ch #\Newline) (incf cur-line) (setf cur-col 0))
                       ((char= ch #\Tab)
                        (setf cur-col (* 8 (1+ (floor cur-col 8)))))
                       (t (incf cur-col)))))
             (emit (text) (write-string text out) (advance text))
             (note-loop-line (f type text)
               "Record this clause token onto F's most-recent content line."
               (let ((cur (car (iframe-lrecs f))))
                 (if (and cur (= (car cur) cur-line))
                     (setf (cdr cur) (cons (list cur-col text type) (cdr cur)))
                     (push (list cur-line (list cur-col text type))
                           (iframe-lrecs f)))))
             (note-sig (type text)
               (let ((f (car stack)))
                 (when f
                   (incf (iframe-sig-count f))
                   (let ((p (iframe-sig-count f)))
                     (cond ((and (= p 1) (eq type :atom))
                            (setf (iframe-head f) (normalize-head text)))
                           ((= p 2)
                            (setf (iframe-arg1-col f) cur-col
                                  (iframe-arg1-line f) cur-line
                                  (iframe-arg1-type f) type)))
                     (let ((h (iframe-head f)))
                       (when (and h (string= h "defmethod") (>= p 3))
                         (if (eq type :group)
                             (setf (iframe-lambda-seen f) t)
                             (unless (iframe-lambda-seen f)
                               (incf (iframe-qualifiers f)))))
                       (when (and h (>= p 2) (string= h "loop"))
                         (note-loop-line f type text)))))))
             (reindent (next)
               (if (or (null next) (eq (tok-type next) :newline))
                   (progn (write-char #\Newline out)
                          (incf cur-line) (setf cur-col 0))
                   (let ((col (compute-indent (car stack) step next)))
                     (write-char #\Newline out)
                     (dotimes (i col) (write-char #\Space out))
                     (incf cur-line) (setf cur-col col)))))
      (loop for cell on toks
            for tk = (car cell)
            for next = (cadr cell)
            do (ecase (tok-type tk)
                 (:newline
                  (let ((governed (+ cur-line 2)))
                    (if (or (null lines)
                            (some (lambda (r) (<= (car r) governed (cdr r))) ranges))
                        (reindent next)
                        (emit (tok-text tk)))))
                 (:open
                  (note-sig :group nil)
                  (push (mk-iframe cur-col cur-line) stack)
                  (emit (tok-text tk)))
                 (:close
                  (emit (tok-text tk))
                  (when stack (pop stack)))
                 (:atom
                  (note-sig :atom (tok-text tk))
                  (emit (tok-text tk)))
                 ((:ws :comment) (emit (tok-text tk)))))
      (get-output-stream-string out))))
