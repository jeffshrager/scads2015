;;; Before running an analysis, you probably want to change these
;;; variables. Below are examples of fns. that set these and do
;;; various anlayses.

(defparameter *low* 20150810110130) ;; Filename (no .ext) of the FIRST file to analyze -- nil to start with lowest filenumber.
(defparameter *high* nil) ;; Filename (no .ext) of the LAST file to analyze -- nil to do all from *low*

#|
                  How to use the analyzer

The first time you run the analyzer in a lisp session you should do this:

(ext::cd "c:\\....")

   (load (compile-file "lhstats.lisp"))

This ensures that the stats package is recompiled and loaded. (The
compiler might complain about format problems in lhstats. Don't worry
about those.)

After you've done that once (per session), all you need to do is to do
a new analysis is

1. Change *low*, and possibly *high*, as described just below this
   block comment.

2. Do: 

   (load (compile-file "analyzer.lisp"))

It starts by itself and will create a new summary stats tsv file in
sumstats/ dir.

|#

;;; ================================================================
;;; Data from Siegler and Shrager 1984 -- Note that this data is under
;;; overt strategy supression instruction!

(defparameter *sns84-data*
  ;; All these are hundreths:
  ;; 0 1 2 3 4 5 6 7 8 9 10 11 other
  '(
    ((1 . 1) (0 5 86 0 2 0 2 0 0 0 0 2 4))
    ((1 . 2) (0 0 9 70 2 0 4 0 0 7 2 2 5))
    ((1 . 3) (0 2 0 11 71 5 2 2 0 0 0 0 7))
    ((1 . 4) (0 0 0 0 11 61 9 7 0 0 0 2 11))
    ((1 . 5) (0 0 0 0 13 16 50 11 0 2 2 0 5))
    ((2 . 1) (0 7 5 79 5 0 0 0 0 0 0 0 4))
    ((2 . 2) (2 0 4 5 80 4 0 5 0 0 0 0 0))
    ((2 . 3) (0 0 4 7 38 34 9 2 2 2 0 0 4))
    ((2 . 4) (0 2 0 7 2 43 29 7 7 0 0 0 4))
    ((2 . 5) (0 2 0 5 2 16 43 13 0 0 2 0 18))
    ((3 . 1) (0 2 0 9 79 4 0 4 0 0 0 0 4))
    ((3 . 2) (0 0 9 11 11 55 7 0 0 0 0 0 7))
    ((3 . 3) (4 0 0 5 21 9 48 0 2 2 2 0 7))
    ((3 . 4) (0 0 0 5 11 23 14 29 2 0 0 0 16))
    ((3 . 5) (0 0 0 7 0 13 23 14 18 0 5 0 20))
    ((4 . 1) (0 0 4 2 9 68 2 2 7 0 0 0 7))
    ((4 . 2) (0 0 7 9 0 20 36 13 7 0 2 0 7))
    ((4 . 3) (0 0 0 5 18 9 9 38 9 0 2 0 11))
    ((4 . 4) (4 0 0 2 2 29 7 7 34 0 4 0 13))
    ((4 . 5) (0 0 0 0 4 9 16 9 11 18 11 4 20))
    ((5 . 1) (0 0 4 0 4 7 71 4 4 0 4 0 4))
    ((5 . 2) (0 0 5 20 2 18 27 25 2 0 2 0 0))
    ((5 . 3) (0 0 2 11 9 18 5 16 23 0 5 0 11))
    ((5 . 4) (0 0 0 0 11 21 16 5 11 16 4 0 16))
    ((5 . 5) (4 0 0 0 0 7 25 11 2 4 34 4 11))
    ))

;;; =============================================================
;;; Globals

(defvar *resultsum* nil)
(defvar *results-version* nil)
(defvar *params->ccs* (make-hash-table :test #'equal))
(defvar *file->data* (make-hash-table :test #'equal))

;;; =============================================================
;;; Parser

;;; FFF Make sure that all the files read have the same version
;;; number. Break if not!

(defun load-result-file (file)
  (format t "Loading ~a~%" file)
  (with-open-file 
   (i file)
   (let* ((vline (read-line i nil nil))
	  ;; 1- bcs of the return on the end
	  (version (parse-integer (subseq vline  (1+ (position #\, vline)) (1- (length vline))))))
     (if (null *results-version*)
	 (setq *results-version* version)
       (unless (equal version *results-version*)
	 (break "Uh oh. Some of the data is from version ~a, and some is from version ~a of the results format. I don't know how to handle this situation!"
		*results-version* version)))
     (case version
	   (20150807 (parse-20150807-data i))
	   (t (break "Unknown results version, vline=~s" vline))))))

(defun parse-20150807-data (i)
  `((:results-version . ,*results-version*) ;; NNN This is actually un-necessary since everything will have the same version 
    (:strategy-use-log 
     .
     ,(reduce-log
      (loop for l = (read-line i nil nil)
	     until (search "===========" l)
	     ;; 1- bcs of the return on the end
	     collect (interpret-log-entry (string-split (subseq l 0 (1- (length l))))))))
    (:params . ,(parse-params i))
    (:results-predictions .
     ,(loop for a from 1 to 5
	    append (loop for b from 1 to 5
			 collect (cons (cons a b)
				       (mapcar #'read-from-string 
					       ;; Drop the first thing, which is just the problem statement
					       (cdr (string-split (read-line i nil nil))))))))))

;;; This is max ugly (UUU) we should go through and parse the properly.

(defun parse-params (i)
  (loop for line = (read-line i nil nil)
	until (search  "========" line)
	collect (let ((p (position #\, line)))
		  (when p
		    (cons (subseq line 0 p)
			  ;; 1- bcs of the return on the end
			  (subseq line (+ p 1) (1- (length line))))))))

;;; =============================================================
;;; Log Analysis

(defparameter *function-name-substitutions*
  '(("count_from_either_strategy" . :cfe)
    ("min_strategy" . :min)
    ("count_from_one_once_strategy" . :cf1x1)
    ("count_from_one_twice_strategy" . :cf1x2)
    ("random_strategy" . :rand)
    ("retrieval" . :ret)
    ("dynamic_retrival" . :dynaret)
    ("used" . :used)
    ("trying" . :trying)
    ("!" . :!)
    ))

(defun simplify-strategy-string (s)
  ;; They come in like this: "<function count_from_either_strategy at 0x10ebcb848>"
  ;; and go out like this: :cfe
  (or 
   (cdr (assoc (if (char-equal #\< (aref s 0))
		   (subseq s 10 (search " at " s :test #'char-equal))
		 s)
	       *function-name-substitutions* :test #'string-equal))
   s))

(defun interpret-log-entry (entry)
  ;; Understand anything in the log entry that we can.
  (mapcar #'(lambda (i) 
	      (or (let ((n (ignore-errors (parse-integer i))))
		    (when (numberp n) n))
		  (simplify-strategy-string i)
		  i))
	  entry))

;;; The log either contains singltons: (:USED :RET 4 3 6) 
;;; or pairs: (:TRYING :CFE 5 2) (:USED :CFE 5 2 7) 
;;; or triples: (:TRYING :CFE 5 2) (:! :DYNARET 5 2 7) (:USED :CFE 5 2 7) 
;;; This combines them into singlton entries that all look like: (:RET 4 3 6), or (:CFE 5 2 7), or: ((:CFE :DYNARET) 5 2 7)

;;; FFF This should have some checks in it for potential problems
;;; 'cause it can make a real mess if it gets out of synch! FFF

(defun reduce-log (log)
  (loop for entry+ on log 
	as k from 1 by 1
	as entry = (car entry+)
	with r = nil
	do (if (eq :used (car entry))
	       (if (eq :ret (second entry))
		   (push (cdr entry) r)
		 :skip)
	     (if (eq :trying (car entry))
		 (if (eq :used (car (second entry+)))
		     (push `(,(second entry) ,@(cddr (second entry+))) r)
		   (if (eq :! (car (second entry+)))
		       (push `((,(second entry) :DYNARET) ,@(cddr (second entry+))) r)
		     (break "Something's wrong (A) with the log at ~a: ~s" k entry)))
	       :skip))
	finally (return 
		 (reverse 
		  (loop for e in r
			when (not (member (car e) '(:! :trying :used)))
			collect e)))))

;;; =============================================================
;;; Math

(defun compare (result-set)
  (let* ((result-set (cdr (assoc :results-predictions result-set)))
	 (pairs (loop for a in (loop for (nil obs) in *sns84-data*
				     append obs)
		      for b in (loop for (problem) in *sns84-data*
				     as sim = (report-sim-results-as-100ths problem result-set)
				     append sim)
		     collect (list a b))))
    (stats::correlation-coefficient pairs)))

(defun report-sim-results-as-100ths (problem result-set)
  (loop for r in (cdr (assoc problem result-set :test #'equal))
	collect (* 100.0 r)))

;;; =============================================================
;;; Utils

(defun string-split (string &key (delimiter #\,) (copy t))
  (let ((substrings '())
        (length (length string))
        (last 0))
    (flet ((add-substring (i)
			  (push (if copy
				    (subseq string last i)
				  (make-array (- i last)
					      :element-type 'character
					      :displaced-to string
					      :displaced-index-offset last))
				substrings)))
	  (dotimes (i length)
	    (when (eq (char string i) delimiter)
	      (add-substring i)
	      (setq last (1+ i))))
	  (add-substring length)
	  (nreverse substrings))))

(defun string-substitute (in from to)
  (loop with start2 = 0 
        with lfrom = (length from)
        with lto = (length to)
        as p = (search from in :start2 start2)
        if p
        do (setq in (format nil "~a~a~a" (subseq in 0 p) to (subseq in (+ p lfrom)))
                 start2 (+ p lto))
        else do (return in)))

(defun dht (table &optional (n 10000))
  (maphash #'(lambda (key value)
	       (when (zerop (decf n)) (return-from dht))
	       (format t "~s: ~s~%" key value)	       
	       )
	   table))

;;; =============================================================
;;; Main

(defun analyze (&key (low *low*)
		     (high *high*)
		     &aux first-fno last-fno label)
  (setq *results-version* nil)
  (if (null low) (setq low 0))
  (if (null high) (setq high 99999999999999))
  (clrhash *file->data*)
  (clrhash *params->ccs*)
  (loop for file in (directory "test_csv/*.csv")
	as fno = (parse-integer (pathname-name file))
	when (and (>= fno low) (<= fno high))
	do
	(let* ((r (ignore-errors (load-result-file file))))
	  (if r
	      (let* ((c (compare r))
		     (p (cdr (assoc :params r))))
		;; Save data for later
		(setf (gethash file *file->data*) r)
		;; Extract and check label
		(let ((new-label (cdr (assoc "settings.experiment_label" p :test #'string-equal))))
		  (if (null label)
		      (setq label new-label)
		    (if (string-equal label new-label)
			:ok
		      (progn 
			(format t "!!! WARNING: New label: ~s doesn't match old label: ~s.~%!!! You are probably incorrectly data from different runs!~%!!! Did you forget to set *low* in the analyzer to the number of the just-above csv file?~%" 
				new-label label)
			(setq label new-label)))))
		(setq last-fno fno)
		(if (null first-fno) (setq first-fno fno))
		(push c (gethash p *params->ccs*))
		) ;; Let*
	    (format t "~a seems to be broken -- ignoring it!~%" file)
	    )))
  (with-open-file 
   (*resultsum* (format nil "sumstats/~a-~a-sumstats.xls" (get-universal-time) (substitute #\_ #\space label))
		:direction :output :if-exists :supersede) 
   (format *resultsum* "~a~%from	f~a~%to	f~a~%" label first-fno last-fno)
   (loop for p being the hash-keys of *params->ccs*
	 using (hash-value cs)
	 with header-shown? = nil
	 when (cdr cs)
	 do 
	 (unless header-shown?
	   ;;(mapcar #'print p)
	   (mapcar #'(lambda (r) (format *resultsum* "~a	" (car r))) p)
	   (format *resultsum* "n	meancc	stderr~%")
	   (setq header-shown? t))
	 (mapcar #'(lambda (r) (format *resultsum* "~a	" (cdr r))) p)
	 (format *resultsum* "~a	~a	~a~%"
		 (length cs)
		 (STATISTICS:MEAN cs)
		 (STATISTICS:STANDARD-ERROR-OF-THE-MEAN cs)
		 ))))

(untrace)
;(trace parse-params)
(analyze) 
