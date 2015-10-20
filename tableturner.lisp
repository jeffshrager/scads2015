; (load (compile-file "tableturner.lisp"))

(defun readtable (file delimiter skip-lines)
  (with-open-file 
   (i file)
   (loop for l below skip-lines do (read-line i nil nil))
   (loop for line = (read-line i nil nil)
	 until (null line)
	 collect (string-split line :delimiter delimiter))))

(defun string-split (string &key (delimiter #\space))
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

(defun files-in-range (&key (directory-and-pattern "sumstats/*.xls")
			    (number-extraction-pattern "xxxxxxxxxxx##############")
			    (low 0) (high 99999999999999))
  (loop for file in (directory directory-and-pattern)
	as n = (extract-number (pathname-name file) number-extraction-pattern)
	when (and n (>= n low) (<= n high))
	collect (cons n file)))
	
(defun extract-number (s p)
  (ignore-errors 
  (parse-integer 
   (coerce (loop for fc across s
		 as pc across p
		 when (char-equal pc #\#)
		 collect fc)
	   'string))))

(defun read-files (&key (directory-and-pattern "sumstats/*.xls")
			(number-extraction-pattern "xxxxxxxxxxx##############")
			(low 0) (high 99999999999999)
			(skip-lines 1) (delimiter #\tab))
  (loop for (n . file) in (files-in-range :directory-and-pattern directory-and-pattern
					  :number-extraction-pattern number-extraction-pattern
					  :low low :high high)
	collect `(,n ,file ,(readtable file delimiter skip-lines))))

(defparameter *nan-tokens* '("nan" "" "	" " " "x" "." "-"))

;;; For the moment the only thing we know how to do is stats on
;;; identical tables. The data must be exaclty compatible, and have
;;; come in from read-files.

(defun transform (data outfile pattern)
  (with-open-file
   (o outfile :direction :output :if-exists :supersede)
   (let* ((example-data (third (first data))) ;; Uses the first one as the model
	  (rows (1- (length (cdr example-data))))
	  (headers (car example-data)))
     (loop for pathdr in pattern
	   do (format o "n:~a	mean:~a	sd:~a	" pathdr  pathdr  pathdr ))
     (format o "~%")
     (loop for row below rows
	   do (loop for pathdr in pattern
		    as col = (1+ (loop for dathdr in headers until (string-equal dathdr pathdr) sum 1))
		    do (when (null col) (error "In TRANSFORM, ~s appears not to be a valid header among: ~s!" pathdr headers))
		    (let* ((values 
			    (loop for (n file (hdr . data)) in data
				  as v = (nth col (nth row data))
				  when (not (member v *nan-tokens* :test #'string-equal))
				  collect (read-from-string v)))
			   (sum (float (reduce #'+ values)))
			   (n (length values))
			   (mean (if (not (zerop n)) (/ sum n) "nan")))
		      (format o "~a	~a	~a	" n mean (if (numberp mean) 
								     (sqrt (/ (loop for v in values sum (expt (- v mean) 2)) n))
								   "nan"))))
	   (format o "~%")))))
	  
(transform (read-files :low 20151019082621 :high 20151019083216)
	   "test.xls" '("ALLRET_n" "ALLRET_+"))
