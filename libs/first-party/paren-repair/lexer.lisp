;;;; Parens-only lexer. Real delimiters only ("(" "#(" ")"); parens in strings,
;;;; chars, |vbar|, #|comments|#, ;comments stay atoms. (concat texts) = input.

(in-package :paren-repair)

(defstruct (tok (:constructor mk-tok (type text line col)))
  type text line col)

(defun %scan-string (s i n)
  (let ((j (1+ i)))
    (loop while (< j n) do
      (let ((c (char s j)))
        (cond ((char= c #\\) (incf j 2))
              ((char= c #\") (incf j) (return))
              (t (incf j)))))
    (subseq s i (min j n))))

(defun %scan-vbar (s i n)
  (let ((j (1+ i)))
    (loop while (< j n) do
      (let ((c (char s j)))
        (cond ((char= c #\\) (incf j 2))
              ((char= c #\|) (incf j) (return))
              (t (incf j)))))
    (subseq s i (min j n))))

(defun %scan-block-comment (s i n)
  (let ((j (+ i 2)) (depth 1))
    (loop while (and (< j n) (> depth 0)) do
      (cond ((and (< (1+ j) n) (char= (char s j) #\#) (char= (char s (1+ j)) #\|))
             (incf depth) (incf j 2))
            ((and (< (1+ j) n) (char= (char s j) #\|) (char= (char s (1+ j)) #\#))
             (decf depth) (incf j 2))
            (t (incf j))))
    (subseq s i (min j n))))

(defun %scan-char (s i n)
  (let ((j (+ i 2)))
    (when (< j n) (incf j))
    (loop while (and (< j n) (alphanumericp (char s j))) do (incf j))
    (subseq s i (min j n))))

(defparameter +atom-terminators+
  '(#\( #\) #\# #\Space #\Tab #\Return #\Page #\Newline #\" #\| #\; #\\))

(defun %scan-atom (s i n)
  (let ((j i))
    (loop while (and (< j n) (not (member (char s j) +atom-terminators+))) do (incf j))
    (subseq s i (max (1+ i) j))))

(defun tokenize (s)
  "Lex S into TOKs carrying type, verbatim text, 0-based line and column.
Types: :open :close :atom :ws :newline :comment."
  (let ((n (length s)) (i 0) (line 0) (col 0) (toks '()))
    (labels ((emit (type text)
               (push (mk-tok type text line col) toks)
               (loop for ch across text do
                 (incf i)
                 (if (char= ch #\Newline) (setf line (1+ line) col 0) (incf col)))))
      (loop while (< i n) do
        (let ((c (char s i)) (d (when (< (1+ i) n) (char s (1+ i)))))
          (cond
            ((char= c #\Newline)
             (let ((j (1+ i)))
               (loop while (and (< j n) (member (char s j) '(#\Space #\Tab))) do (incf j))
               (emit :newline (subseq s i j))))
            ((member c '(#\Space #\Tab #\Return #\Page))
             (let ((j i))
               (loop while (and (< j n) (member (char s j) '(#\Space #\Tab #\Return #\Page))) do (incf j))
               (emit :ws (subseq s i j))))
            ((char= c #\;)
             (let ((j i))
               (loop while (and (< j n) (char/= (char s j) #\Newline)) do (incf j))
               (emit :comment (subseq s i j))))
            ((char= c #\") (emit :atom (%scan-string s i n)))
            ((char= c #\|) (emit :atom (%scan-vbar s i n)))
            ((char= c #\#)
             (cond ((eql d #\|) (emit :atom (%scan-block-comment s i n)))
                   ((eql d #\\) (emit :atom (%scan-char s i n)))
                   ((eql d #\() (emit :open "#("))
                   (t (emit :atom "#"))))
            ((char= c #\() (emit :open "("))
            ((char= c #\)) (emit :close ")"))
            ((char= c #\\) (emit :atom (subseq s i (min n (+ i 2)))))
            (t (emit :atom (%scan-atom s i n))))))
      (nreverse toks))))
