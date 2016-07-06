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
  (load-data low high)
;  (summarize-logs ts)
;  (summarize-final-strategy-prefs ts)
;  (summarize-coefs ts)
  )

(defun load-data (low high &aux first-fno last-fno label temp constrain-by-label)
  (setq *logs* nil)
  (setf constrain-by-label (if (or low high) nil t))
  (if (null low) (setq low 0))
  (if (null high) (setq high 99999999999999))
  (if (> low high) (setf temp high high low low temp)) ;; Idiot corrector
  ;; Load all the data, do some preliminary analysis, and store
  ;; partial results for report production
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
			  (return-from load-data))
		      (progn (setf target-label this-label)
			     (format t "~%Only reading logs with label: ~s~%" target-label)
			     (setf *heuristicated-experiment-label* target-label)
			     log))
		  (if (and (>= fno low) (<= fno high)) log))))
	  (when store-log 
	    (format t "Using ~a~%" file)
	    (clean-up store-log) ;; This smashes the :run entry
	    (setf (gethash file *file->log*) store-log)))
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

(defun summarize-logs (ts)
  ;; Report the retrieval fractions and % correct mean and serr for each dataset seprately
  ;; meanwhile storing the overall stats for summarization
  (clrhash *file->summary*)
  (loop for file being the hash-keys of *file->log*
	using (hash-value log)
	do 
	(with-open-file 
	 (o (print (format nil "sumstats/~a-~a-logsummary.xls" ts (substitute #\_ #\space (pathname-name file))))
	    :direction :output :if-exists :supersede) 
	 ;; output headers
	 (if *heuristicated-experiment-label*
	     (format o "# ~a~%" *heuristicated-experiment-label*)
	   (format o "# low=~a, high=~a~%" *low* *high*))
	 (format o "i	n")
	 (format o "~%")
	 ;; The problem-block looks like this:
	 ;; (:problem-block
	 ;;  (:problems
	 ;;   ((:used retrieval 3 + 3 = 6) )
	 ;;   ((:trying count_from_one_once 2 + 3) (:used count_from_one_once 2 + 3 = 4) )
	 ;;   ...) ;; close problems
         ;;  (:results-prediction-table
	 ;;    (1 + 1 = 9 (0.66801  0.43552  0.95849  -0.91478  -0.14068  0.99803  -0.87101  0.95384  0.89992  1.0  -0.54585  0.99997  -0.84957))
	 ;;    (1 + 2 = 9 (-0.5814  -0.7126  -0.4912  -0.95835  -0.91221  0.99846  -0.91654  0.98572  -0.04236  0.99999  -0.90574  0.99792  -0.959))
	 ;;    ...) ;; close results-prediction-table
         ;;  (:strategy-prediction-table
	 ;;    (1 + 1 = min (0.40504  -0.95853  -0.08923  0.65204))
	 ;;    (1 + 2 = count_from_either (-0.02833  -0.61649  0.61381  -0.91041))
	 ;;    ...) ;; close strategy-prediction-table
	 ;;  ) ;; close :problem-block
	 (loop for pb in (cdr (assoc :run log))
	       as i from 1 by 1
	       as ps = (cdr (assoc :problems pb))
	       as np = (length ps)
	       as corcoefs = (loop for (key data) in *comparator-datasets* 
			      collect `(,key ,(compare (cdr (assoc :Results-prediction-table pb)) data)))
	       do 
	       (push `((:i ,i) (:corcoefs ,corcoefs)) (gethash file *file->summary*))
	       (format o "~a	~a" i np)
	       (loop for (nil cc) in corcoefs do (format o "	~a" cc))
	       ;; Init pairs for (correct . incorrect) counts...
	       (clrhash *strat-key->correct+incorrect*)
	       (loop for s in *strat-keys* do (setf (gethash s *strat-key->correct+incorrect*) (cons 0 0)))
	       ;; ...and count 'em up!
	       (loop for p in ps
		     as (nil strat-name a1 nil a2 nil r) = (assoc :used p) ;; ((:used count_from_one_twice 5 + 3 = 8) )
		     as strat-key = (cdr (assoc strat-name *function-name-substitutions* :test #'string-equal))
		     do 
		     (let ((c/ic-pair (gethash strat-key *strat-key->correct+incorrect*))
			   (real-r (+ a1 a2)))
		       (if (= r real-r)
			   (incf (car c/ic-pair))
			 (incf (cdr c/ic-pair)))))
	       ;; Reporting
	       (loop for s in *strat-keys*
		     as pair = (gethash s *strat-key->correct+incorrect*)
		     as nc = (car pair)
		     as nw = (cdr pair)
		     as ns = (+ nc nw)
		     do (format o "	~a	~a	~a	~a" 
				ns 
				(if (zerop np) "x" (/ (float ns) np))
				nc
				(if (zerop ns) "x" (/ (float nc) ns)))
		     )
	       (format o "~%")
	       ))))

;;; Analysis on a per-parameter basis of the most used strategy for
;;; each problem.

(defvar *params->problem-blocks* (make-hash-table :test #'equal))
(defun summarize-final-strategy-prefs (ts)
  (clrhash *params->problem-blocks*)
  (loop for file being the hash-keys of *file->log*
	using (hash-value log)
	as params = (assoc :params log)
	do (push (list file (cdr (assoc :Strategy-prediction-table (car (last (cdr (assoc :run log)))))))
		 (gethash params *params->problem-blocks*)))
  (with-open-file 
   (o (print (format nil "sumstats/~a-finalstrategyprefs.xls" ts)) :direction :output :if-exists :supersede)
   (if *heuristicated-experiment-label*
       (format o ";;; ~a~%(~%" *heuristicated-experiment-label*)
	   (format o ";;; low=~a, high=~a~%" *low* *high*))
   ;; Output headers: one col per file in param set order
   (format o "Param/Problem")
   (loop for params being the hash-keys of *params->problem-blocks*
	 using (hash-value pbs)
	 do (loop for pb in pbs
		  do (format o "	~a" (pathname-name (car pb))))) ;; output the filename
   (format o "~%")
   ;; Now the params
   (loop for params being the hash-keys of *params->problem-blocks*
	 using (hash-value pbs)
	 do (loop for (param nil) in (cdr params)
		  do (format o "~a" param)
		  (loop for params being the hash-keys of *params->problem-blocks*
			using (hash-value pbs)
			do (loop for pb in pbs
				 do (format o "	~a" (second (assoc param (cdr params))))))
		  (format o "~%")))
   ;; And finally the problems
   (loop for a1 from 1 to 5
	 do (loop for a2 from 1 to 5
		  do (format o "~a+~a" a1 a2)
		  (loop for params being the hash-keys of *params->problem-blocks*
			using (hash-value pbs)
			do (loop for pb in pbs
				 do (format o "	~a" (loop for (b1 nil b2 = result . nil) in (cadr pb)
							  when (and (= a1 b1) (= a2 b2))
							  do (return result)))))
		  (format o "~%")))
   ))

(defparameter *param-reporting-order* 
  '(
    ("strategy_hidden_units" . strategy_hidden_units)
    ("results_hidden_units" . results_hidden_units)
    ("initial_counting_network_burn_in_epochs" . initial_counting_network_burn_in_epochs)
    ("initial_counting_network_learning_rate" . initial_counting_network_learning_rate)
    ("n_problems" . n_problems)
    ("DR_threshold" . DR_threshold)
    ("PERR" . PERR)
    ("addends_matrix_offby1_delta" . addends_matrix_offby1_delta)
    ("RETRIEVAL_LOW_CC" . RETRIEVAL_LOW_CC)
    ("RETRIEVAL_HIGH_CC" . RETRIEVAL_HIGH_CC)
    ("STRATEGY_LOW_CC" . STRATEGY_LOW_CC)
    ("STRATEGY_HIGH_CC" . STRATEGY_HIGH_CC)
    ("non_result_y_filler" . non_result_y_filler)
    ("INCR_on_RIGHT" . INCR_on_RIGHT)
    ("DECR_on_WRONG" . DECR_on_WRONG)
    ("INCR_the_right_answer_on_WRONG" . INCR_the_right_answer_on_WRONG)
    ("strategy_learning_rate" . strategy_learning_rate)
    ("results_learning_rate" . results_learning_rate)
    ("in_process_training_epochs" . in_process_training_epochs)
    ))

(defvar *params->final-coefs* (make-hash-table :test #'equal))
(defvar *params->all-values* (make-hash-table :test #'equal)) ;; Let's us tell which ones actually changed.

;;; Combinalyzer takes several files created above, each called, for
;;; example, "3656687231-FinalPivotforR.csv" and a single variable to
;;; summarize, and does the stats for them.

(defvar *c->ms* (make-hash-table :test #'equal))

(defun combinalyze (file-numbers cvar mvar)
  (clrhash *c->ms*)
  (loop for (c . m) in 
	(loop for file in (directory "sumstats/*-FinalPivotforR.csv")
	      as file-name = (pathname-name file)
	      when (member (parse-integer (subseq file-name 0 (position #\- file-name))) file-numbers)
	      append (load-FinalPivotforR-csv-file file cvar mvar))
	do (push m (gethash c *c->ms*)))
  (with-open-file 
   (o (format nil "sumstats/~a-combined-analysis.csv" (get-universal-time)) :direction :output)
   (format o "# Combined anlaysis of ~a on ~a over ~a~%" file-numbers cvar mvar)
   (format o "~a,mean-~a,stderr-~a~%" cvar mvar mvar)
   (loop for c being the hash-keys of *c->ms*
	 using (hash-value ms)
	 do (format o "~a,~a,~a~%"
		    c (statistics::mean ms) (statistics::standard-error-of-the-mean ms))))
  )
	
(defun load-FinalPivotforR-csv-file (file cvar mvar)
  (with-open-file 
   (i file)
   (read-line i nil nil) ;; Skip # header
   (let* ((hline (string-split (read-line i nil nil)))
	  (ccol (position cvar hline :test #'string-equal))
	  (mcol (position mvar hline :test #'string-equal)))
   (loop for line = (read-line i nil nil)
	 until (null line)
	 collect (let ((sline (string-split line)))
		   (cons (read-from-string (nth ccol sline))
			 (read-from-string (nth mcol sline))))))))

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

