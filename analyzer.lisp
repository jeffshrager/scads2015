; (load (compile-file "analyzer.lisp"))
(unless (ignore-errors (symbol-function 'correlation)) (load "~/Desktop/etc/cllib/stats.dx32fsl"))
(unless (ignore-errors (symbol-function 'correlation-coefficient)) (load "~/Desktop/etc/cllib/lhstats.dx32fsl"))

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

(defvar *tsv* nil)

(defun load-result-file (file)
   (with-open-file 
    (i file)
    (cons (parse-params (loop for k below 5 collect (read-line i nil nil)))
	  ;; Each line from here on should have a problem and then a bunch of results
	  ;; cols are ,0,1,2,3,4,5,6,7,8,9,10,11,OTHER
	  (loop for a from 1 to 5
		append (loop for b from 1 to 5
			     collect (cons (cons a b)
					   (mapcar #'read-from-string 
						   ;; Drop the first thing, which is just the problem statement
						   (cdr (string-split (read-line i nil nil))))))))))

(defun parse-params (params)
  (let* ((l1 (string-split (first params)))
	 (l2 (string-split (second params))))
    `((ep . ,(parse-integer (second l1)))
      (lr . ,(read-from-string (fourth l1)))
      (ir . ,(read-from-string (second l2)))
      (st . ,(subseq (fourth l2) 10 (search " at " (fourth l2)))))))

(defun compare (result-set)
  (let* ((result-set (cdr result-set)) ;; Drop the parameters
	 (pairs (loop for a in (loop for (problem obs) in *sns84-data*
				     append obs)
		      for b in (loop for (problem) in *sns84-data*
				     as sim = (report-sim-results-as-100ths problem result-set)
				     append sim)
		     collect (list a b))))
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

(defun test ()
  (with-open-file 
   (*tsv* "resultsum.xls" :direction :output :if-exists :supersede) 
   (format *tsv* "Epochs	LearnRate	CorrectIncr	Srategy	File	CorrCoef~%")
  (loop for file in (directory "test_csv/*.csv")
	as r = (load-result-file file)
	as c = (compare r)
	as p = (car r)
	do 
	(format t "~a [~a] --> ~a~%" (car r) (pathname-name file) c)
	(mapcar #'(lambda (r) (format *tsv* "~a		" (cdr r))) p)
	(format *tsv* "~a	~a~%" (pathname-name file) c)
	)))

(untrace)
;(trace report-sim-results-as-100ths)
(test)
