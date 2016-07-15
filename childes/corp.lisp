; (load (compile-file "corp.lisp"))

(defvar *sources* nil) ;; loaded from sources.lisp
(defvar *age->word->count* (make-hash-table :test #'equal))

(defun scrape-sources ()
  (loop for (subdir . mo+files) in *sources*
	do (print subdir)
	(loop for (mo fn) in mo+files
	      as file = (format nil "sources/~a/~a" subdir fn)
	      as word->count = (gethash mo *age->word->count* (make-hash-table :test #'equal))
	      do (setf (gethash mo *age->word->count*)
		       (scrape-file file word->count)))))

(defparameter *c->n*
  '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4) (#\5 . 5) (#\6 . 6) (#\7 . 7) (#\8 . 8) (#\9 . 9)))

(defun fnnx (fn n)
  (cdr (assoc (aref fn n) *c->n* :test #'equal)))

(defun pathname->months (file)
  (let* ((name (pathname-name file))
	(l (length name)))
    (if (= 4 l)
	(+ (* (fnnx name 2) 12) (fnnx name 3)))))

(defun scrape-file (file word->count)
  (print file)
  (with-open-file
   (i file)
   (loop for line = (read-childes-line i)
	 until (null line)
	 do (loop for word in (mapcar #'(lambda (w) (clean-word (clean-word w))) (string-split line :delimiter #\space))
		  unless (or (zerop (length word)) (member word *stop-words* :test #'string-equal))
		  do (incf (gethash word word->count 0)))))
  word->count)

;;; This finds the next line in the file beginning with an asterisk
;;; (which could be *mot *chi, etc) and then includes any lines that
;;; have nothing (whitespace) together (beacuse the CHILDES folks have
;;; a completely stupid representation!) If you hit the end of the
;;; file, return nil.

(defun read-childes-line (i)
  ;; For the moment we cheat and only read a line beginning with * and
  ;; only ONE such line, so if there's several lines, we drop the rest
  ;; on the floor. FFF WWW XXX
  (let ((line (read-line i nil nil)))
    (cond ((null line) nil)
	  ((char-equal #\* (aref line 0)) line)
	  (t ""))))

;;; Unfolds deep directories into direct pointers that can go into the
;;; sources.lisp file

(defun string-split (string &key (delimiter #\,))
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring (i)
             (push (subseq string last i) substrings)))
      (dotimes (i length)
        (when (eq (char string i) delimiter)
          (add-substring i)
          (setq last (1+ i))))
      (add-substring length)
      (nreverse substrings))))

(defun clean-word (word)
  (let* ((c1 (string-downcase (string-trim " [	]$0123456789_- ,.;:'\"-+-/\\`~?<>@#$%^&*()<>|&=" word)))
	 (pvbar (position #\| c1)))
    (if pvbar 
	(subseq c1 (1+ pvbar))
      c1)))

(defun dht (table &optional (n 10))
  (maphash #'(lambda (key value)
	       (when (zerop (decf n)) (return-from dht))
	       (format t "~s: ~s~%" key value)	       
	       )
	   table))

(defparameter *stop-words* '("chi" "mot" "act" "!"))
(defparameter *target-words* '("one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "plus" "count" "many"))

(defun report (&key (targets *target-words*) (min-count 1000))
  (with-open-file
   (o "words.xls" :direction :output :if-exists :supersede)
   (loop for months being the hash-keys of *age->word->count*
	 using (hash-value word->count)
	 as total-words = (loop for c being the hash-values of word->count sum c)
	 when months
	 do (loop for (count . word) in 
	       (if targets
		   (loop for word in targets
			 as count = (gethash word word->count)
			 collect (cons count word))
		 (sort 
		  (loop for word being the hash-keys of word->count
			using (hash-value count)
			when (<= min-count count)
			collect (cons count word))
		  #'> :key #'car))
	       do (format o "~a	~a	~a	~a	~a~%" 
			  months total-words word count (when (and word count) (/ (float count) total-words)))
	       ))))

(defun inverted-report (&key (targets *target-words*) (min-count 1000))
  (with-open-file
   (o "wordsinv.xls" :direction :output :if-exists :supersede)
   (format o "months	")
   (loop for word in targets do (format o "~a	" word))
   (format o "~%")
   (loop for months being the hash-keys of *age->word->count*
	 using (hash-value word->count)
	 as total-words = (loop for c being the hash-values of word->count sum c)
	 when months
	 do 
	 (format o "~a	" months)
	 (loop for word in targets
	       as count = (gethash word word->count)
	       do (format o "~a	" (when (and word count) (/ (float count) total-words))))
	 (format o "~%"))))

(defun get-file-age (file)
  (with-open-file 
   (i file)
   (loop for line = (read-line i nil nil)
	 as pid = (and line (search "@ID:" line :test #'char-equal))
	 as chipos = (and line (search "|CHI|" line :test #'char-equal))
	 do (cond ((null line) (format t "File ~a doesn't have an identifiable age entry!" file) (return nil))
		  ((and pid (zerop pid) chipos)
		   (return (subseq line (+ 5 chipos) (position #\| line :start (+ chipos 5)))))))))

(defun xlate-childes-age-rep-to-months (a)
  ;; The CHILDES says something like: "4;1.09"
  (let* ((spos (position #\; a))
	 (dpos (position #\. a))
	 (y (parse-integer (subseq a 0 spos)))
	 (m (parse-integer (subseq a (1+ spos) dpos))))
    (+ (* 12 y) m)))

(defun sourceify (file)
  (let ((a (get-file-age file)))
    (when a (print `(,(xlate-childes-age-rep-to-months a) ,file)))))

(defun test (&key reset)
  (when (or reset (zerop (hash-table-count *age->word->count*)))
    (load "sources.lisp")
    (clrhash *age->word->count*)
    (scrape-sources)
    )
  (inverted-report)
  ;(report) ;; reports only *target-words*
  ;(report :targets nil) ;; Reports all words
  )

(untrace)
;(trace fnnx)
;(test :reset t)
(test)
