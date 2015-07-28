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

1. Consider changing *low*, *high*, *filename-key*, and *label* as 
   described just below this block comment.

2. Do: 

   (load (compile-file "analyzer.lisp"))

It starts by itself and will create a new summary stats tsv file in
sumstats/ dir.

|#

;;; Before running an analysis, you probably want to change these
;;; variables:

(defparameter *low* 20150727111454) ;; Filename (no .ext) of the FIRST file to analyze -- nil to start with lowest filenumber.
(defparameter *high* nil) ;; Filename (no .ext) of the LAST file to analyze -- nil to do all from *low*
(defparameter *filename-key* "someanalysis123") ;; A quick reminder of the analysis -- this will become part of the filename!
(defparameter *label* "Whatever you want to say about what this analysis is about.") ;; A longer description -- this goes in the file

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

(defvar *resultsum* nil)

(defun load-result-file (file)
   (with-open-file 
    (i file)
    (cons (parse-params i)
	  ;; Each line from here on should have a problem and then a bunch of results
	  ;; cols are ,0,1,2,3,4,5,6,7,8,9,10,11,OTHER
	  (loop for a from 1 to 5
		append (loop for b from 1 to 5
			     collect (cons (cons a b)
					   (mapcar #'read-from-string 
						   ;; Drop the first thing, which is just the problem statement
						   (cdr (string-split (read-line i nil nil))))))))))

(defun parse-params (i)
  (let* ((params (loop for line = (read-line i nil nil)
		       as k from 1 by 1
		       until (search  "OTHER" line)
		       do (length line) (if (> k 10) (break)) ;; Avoid hard looping in case of problems.
		       collect line)))
    (if (search "N_PROBLEMS" (car params))
	(loop for param in '(np ep lr ip st)
	      as line in params
	      as v = (second (string-split line))
	      collect 
	      (cons param
		    (case param
			  (st (subseq v 10 (search " at " v)))
			  (t (read-from-string v)))))
      (let* ((l1 (string-split (first params)))
	     (l2 (string-split (second params))))
	`((np . 1000) ;; Old version didn't include this, but they were all 1000
	  (ep . ,(parse-integer (second l1)))
	  (lr . ,(read-from-string (fourth l1)))
	  (ir . ,(read-from-string (second l2)))
	  (st . ,(subseq (fourth l2) 10 (search " at " (fourth l2)))))))))

(defun compare (result-set)
  (let* ((result-set (cdr result-set)) ;; Drop the parameters
	 (pairs (loop for a in (loop for (problem obs) in *sns84-data*
				     append obs)
		      for b in (loop for (problem) in *sns84-data*
				     as sim = (report-sim-results-as-100ths problem result-set)
				     append sim)
		     collect (list a b))))
    (format t "~%")
    #+nil
    (loop with p2 = (copy-list pairs)
	  for i from 1 to 5 
	  do (loop for j from 1 to 5
		   do 
		   (format t "~a + ~a: " i j)
		   (loop for r from 0 to 12
			    do 
			    (when (= r (+ i j)) (format t "**"))
			    (format t "~a: ~a, " r (pop p2)))
		   (format t "~%")))
    (stats::correlation-coefficient pairs)))

(defun report-sim-results-as-100ths (problem result-set)
  (loop for r in (cdr (assoc problem result-set :test #'equal))
	collect (* 100.0 r)))

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

(defvar *params->ccs* (make-hash-table :test #'equal))

(defun dht (table &optional (n 10000))
  (maphash #'(lambda (key value)
	       (when (zerop (decf n)) (return-from dht))
	       (format t "~s: ~s~%" key value)	       
	       )
	   table))

(defun test (&key filename-key label low high
		  &aux first-fno last-fno)
  (if (null low) (setq low 0))
  (if (null high) (setq high 99999999999999))
  (clrhash *params->ccs*)
  (with-open-file 
   (*resultsum* "allresults.xls" :direction :output :if-exists :supersede) 
   (format *resultsum* "Epochs	LearnRate	CorrectIncr	Srategy	File	CorrCoef~%")
   (loop for file in (directory "test_csv/*.csv")
	 as fno = (parse-integer (pathname-name file))
	 when (and (>= fno low) (<= fno high))
	 do
	 (let* ((r (load-result-file file))
		(c (compare r))
		(p (car r)))
	   (setq last-fno fno)
	   (if (null first-fno) (setq first-fno fno))
	   (format t "~a [~a] --> ~a~%" p (pathname-name file) c)
	   (mapcar #'(lambda (r) (format *resultsum* "~a	" (cdr r))) p)
	   (format *resultsum* "~a	~a~%" (pathname-name file) c)
	   (push c (gethash p *params->ccs*))
	   )))
  (format t "Summary stats (only examples with multiple runs are displayed here):~%")
  (with-open-file 
   (*resultsum* (format nil "sumstats/~a-~a-sumstats.xls" (get-universal-time) filename-key)
		:direction :output :if-exists :supersede) 
   (format *resultsum* "~a~%from	f~a~%to	f~a~%" (or label filename-key) first-fno last-fno)
   (format *resultsum* "NProblems	Epochs	LearnRate	CorrectIncr	Srategy	n	meancc	stderr~%")
  (loop for p being the hash-keys of *params->ccs*
	using (hash-value cs)
	when (cdr cs)
	do 
	(mapcar #'(lambda (r) (format *resultsum* "~a	" (cdr r))) p)
	(format *resultsum* "~a	~a	~a~%"
		   (length cs)
		   (STATISTICS:MEAN cs)
		   (STATISTICS:STANDARD-ERROR-OF-THE-MEAN cs)
		   ))))

(untrace)
;(trace report-sim-results-as-100ths)
(test :low *low* :high *high* :filename-key *filename-key*)
