;(load (compile-file "dca.lisp"))

;;; === ToDo ===
;;; Analyze the strategy logs.

(eval-when 
 (compile load)
 (unless (probe-file #+clisp "../lib/lhstats.fas" #+ccl "../lib/lhstats.dx32fsl")
   (compile-file "../lib/lhstats.lisp"))
 (unless (find-package 'STATISTICS)
   (load "../lib/lhstats")))

;;; !!! WWW If at least *low* isn't set, the analyzer will try to find
;;; the files to analyze by the latest set of matching experiment
;;; lables.
(defparameter *low* nil) ;; Filename (no .ext) of the FIRST file to analyze -- nil to start with lowest filenumber.
(defparameter *high* nil) ;; Filename (no .ext) of the LAST file to analyze -- nil to do all from *low*

(defvar *heuristicated-experiment-label* nil)

;;; =============================================================
;;; Globals

(defvar *resultsum* nil)
(defvar *results-version* nil)
(defvar *params->ccs* (make-hash-table :test #'equal))
(defvar *file->log* (make-hash-table :test #'equal))

;;; =============================================================
;;; Utils

(defun dht (table &optional (n 10000))
  (maphash #'(lambda (key value)
	       (when (zerop (decf n)) (return-from dht))
	       (format t "~s: ~s~%" key value)       
	       )
	   table))

;;; =============================================================
;;; Main

(defvar *logs* nil) ;; This is just a drop so that we can look at an
					 ;; example of the data after the fact.

(defvar *file->summary* (make-hash-table :test #'equal))

;;; This is the order in which they'll show up in the analyzed
;;; results.

(defun analyze (&key (low *low*) (high *high*) &aux  (ts (get-universal-time)))
  (setq *results-version* nil)
  (clrhash *file->log*)
  (clrhash *params->ccs*)
  (load-data low high ts)
  (summarize ts)
  )

(defun load-data (low high ts &aux first-fno last-fno label temp constrain-by-label)
  (setq *logs* nil)
  (setf constrain-by-label (if (or low high) nil t))
  (if (null low) (setq low 0))
  (if (null high) (setq high 99999999999999))
  (if (> low high) (setf temp high high low low temp)) ;; Idiot correction
  ;; Load all the data, do some preliminary analysis, and
  ;; store partial results for report production; make a compressed
  ;; log while we're here.
  (with-open-file 
   (logsum (print (format nil "sumstats/~a-logsum.lisp" ts))
	   :direction :output :if-exists :supersede) 
   (block load-data-inner ;; Prevent inner return-from from dissappearing the logsum file.
   (loop for file in (downsorted-directory "runlogs/*.lisp")
	 with target-label = nil
	 as fno = (parse-integer (pathname-name file))
	 as log = (with-open-file (i file) (cdr (read i)))
	 do
	 (let* ((params (cdr (assoc :params log)))
		(this-label (second (assoc :experiment_label params)))
		(store-log
		 (if constrain-by-label
		     (if target-label
			 (if (string-equal this-label target-label) 
			     log ;; Match
			   ;; As soon as you find one that doesn't match, give up!
			   (return-from load-data-inner))
		       (progn (setf target-label this-label)
			      (format t "~%Only reading logs with label: ~s~%" target-label)
			      (setf *heuristicated-experiment-label* target-label)
			      log))
		   (if (and (>= fno low) (<= fno high)) log))))
	   (when store-log 
	     (pprint `((:fn ,(pathname-name file))
		      (:params ,params)
		      ,(assoc :head log))
		    logsum))
	   (format t "Using ~a~%" file)
	   (clean-up store-log) ;; This smashes the :run entry
	   (setf (gethash file *file->log*) store-log))))
   ))
					
(defun downsorted-directory (p)
  (mapcar #'cdr
	  (sort
	   (loop for f in (directory p)
		 collect (cons (parse-integer (pathname-name f)) f))
	   #'> :key #'car)))

;; Turns out that for various Obiwan reasons there are problem blocks
;; with the wrong number of problems, and missing table dumps, usually
;; at the beginning and end. This removes these just so that the rest
;; of the code can run generically. UUU FFF This should be fixed up in
;; the sim, not here! FFF

(defun clean-up (log)
  (let ((pbs (second (assoc :problem-bin-size (cdr (assoc :head log))))))
    (setf (cdr (assoc :run log))
					  (loop for (nil . pb) in (cdr (assoc :run log))
					collect pb))))

(defvar *p->last-wrong-pos* (make-hash-table :test #'equal))
(defun summarize (ts)
  (clrhash *file->summary*)
  (with-open-file 
   (o (print (format nil "sumstats/~a-dcsum.xls" ts))
      :direction :output :if-exists :supersede) 
   ;; Headers -- only roles once!
   (loop for log being the hash-value of *file->log*
	 do 	     
	 (let ((params (cdr (assoc :params log))))
	   (format o "file	")
	   (loop for (n nil) in params 
		 do (format o "~a	" n))
	   (loop for i from 1 to 5 
		 do (format o "~a	" i))
	   (format o "~%"))
	 (return t))
   ;; Data
   (loop for file being the hash-key of *file->log*
	 using (hash-value log)
	 do 
	 (let ((params (cdr (assoc :params log)))
	       (training (loop for ((nil . d*)) in (cdr (assoc :run log)) append d*)))
	   (format o "~a	" (pathname-name file))
	   (loop for (nil v) in params 
		 do (format o "~a	" v))
	   (clrhash *p->last-wrong-pos*)
	   ;; ((:ENCODING :-WRONG- (5 => 3) ... )
	   (loop for enc in training
		 as ((nil r/w? i/o)) = enc
		 as pos from 1 by 1
		 if (eq :-wrong- r/w?)
		 do (setf (gethash (car i/o) *p->last-wrong-pos*) pos))
	   (loop for i from 1 to 5
		 as pos = (gethash i *p->last-wrong-pos*)
		 do (format o "~a	" pos))
	   (if *heuristicated-experiment-label*
	       (format o "~a	~%" *heuristicated-experiment-label*))
	   ))))

;;; Analysis on a per-parameter basis of the most used strategy for
;;; each problem.

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

(untrace)
;(trace find-sum)
(analyze)

