;(load (compile-file "analyzer.lisp"))

;;; === ToDo ===
;;; Analyze the strategy logs.

(eval-when 
 (compile load)
 (unless (probe-file #+clisp "lhstats.fas" #+ccl "lhstats.dx32fsl")
   (compile-file "lhstats.lisp"))
 (unless (find-package 'STATISTICS)
   (load "lhstats")))

;;; !!! WWW If at least *low* isn't set, the analyzer will try to find
;;; the files to analyze by the latest set of matching experiment
;;; lables.
(defparameter *low* nil) ;; Filename (no .ext) of the FIRST file to analyze -- nil to start with lowest filenumber.
(defparameter *high* nil) ;; Filename (no .ext) of the LAST file to analyze -- nil to do all from *low*

(defvar *heuristicated-experiment-label* nil)

;;; ================================================================
;;; Data from Siegler and Shrager 1984 -- Note that this data is under
;;; overt strategy supression instruction!

(defparameter *comparator-datasets* 
  '(
    (:sns84
     ;; All these are hundreths:
     ;; 0 1 2 3 4 5 6 7 8 9 10 11 other
     (
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

;; This represents primacy, recency, and next biases. We assign 33%
;; to each addend, then 16% to the next of each. (You might think
;; that by recency, the latter addend should get a little more, but
;; this is counter-balanced by primacy, where the first one
;; should...so we just call it even.)

    (:base-p/r/c
     (
      ((1 . 1) (0 66 33 0 0 0 0 0 0 0 0 0 0))
      ((1 . 2) (0 33 33 33 0 0 0 0 0 0 0 0))
      ((1 . 3) (0 33 0 33 33 0 0 0 0 0 0 0 0))
      ((1 . 4) (0 33 16 0 33 16 0 0 0 0 0 0 0))
      ((1 . 5) (0 33 16 0 0 33 16 0 0 0 0 0 0))
      ((2 . 1) (0 33 49 16 0 0 0 0 0 0 0 0 0))
      ((2 . 2) (0 0 66 33 0 0 0 0 0 0 0 0 0))
      ((2 . 3) (0 0 33 33 33 0 0 0 0 0 0 0 0))
      ((2 . 4) (0 0 33 16 33 16 0 0 0 0 0 0 0))
      ((2 . 5) (0 0 33 16 0 33 16 0 0 0 0 0 0))
      ((3 . 1) (0 33 16 33 16 0 0 0 0 0 0 0 0))
      ((3 . 2) (0 0 0 0 0 0 0 0 0 0 0 0 0))
      ((3 . 3) (0 0 0 66 33 0 0 0 0 0 0 0 0))
      ((3 . 4) (0 0 0 33 33 33 0 0 0 0 0 0 0))
      ((3 . 5) (0 0 0 33 16 33 16 0 0 0 0 0 0))
      ((4 . 1) (0 33 16 0 33 16 0 0 0 0 0 0 0))
      ((4 . 2) (0 0 33 16 33 16 0 0 0 0 0 0 0))
      ((4 . 3) (0 0 0 16 49 16 0 0 0 0 0 0 0))
      ((4 . 4) (0 0 0 0 66 33 0 0 0 0 0 0 0))
      ((4 . 5) (0 0 0 0 33 33 33 0 0 0 0 0 0))
      ((5 . 1) (0 33 16 0 0 33 16 0 0 0 0 0 0))
      ((5 . 2) (0 0 33 16 33 16 0 0 0 0 0 0 0))
      ((5 . 3) (0 0 0 33 16 33 16 0 0 0 0 0 0))
      ((5 . 4) (0 0 0 0 33 49 16 0 0 0 0 0 0))
      ((5 . 5) (0 0 0 0 0 66 33 0 0 0 0 0 0))
      ))

  (:adult 
   (
    ((1 . 1) (0 0 100 0 0 0 0 0 0 0 0 0 0 ))
    ((1 . 2) (0 0 0 100 0 0 0 0 0 0 0 0 0 ))
    ((1 . 3) (0 0 0 0 100 0 0 0 0 0 0 0 0 ))
    ((1 . 4) (0 0 0 0 0 100 0 0 0 0 0 0 0 ))
    ((1 . 5) (0 0 0 0 0 0 100 0 0 0 0 0 0 ))
    ((2 . 1) (0 0 0 100 0 0 0 0 0 0 0 0 0 ))
    ((2 . 2) (0 0 0 0 100 0 0 0 0 0 0 0 0 ))
    ((2 . 3) (0 0 0 0 0 100 0 0 0 0 0 0 0 ))
    ((2 . 4) (0 0 0 0 0 0 100 0 0 0 0 0 0 ))
    ((2 . 5) (0 0 0 0 0 0 0 100 0 0 0 0 0 ))
    ((3 . 1) (0 0 0 0 100 0 0 0 0 0 0 0 0 ))
    ((3 . 2) (0 0 0 0 0 100 0 0 0 0 0 0 0 ))
    ((3 . 3) (0 0 0 0 0 0 100 0 0 0 0 0 0 ))
    ((3 . 4) (0 0 0 0 0 0 0 100 0 0 0 0 0 ))
    ((3 . 5) (0 0 0 0 0 0 0 0 100 0 0 0 0 ))
    ((4 . 1) (0 0 0 0 0 100 0 0 0 0 0 0 0 ))
    ((4 . 2) (0 0 0 0 0 0 100 0 0 0 0 0 0 ))
    ((4 . 3) (0 0 0 0 0 0 0 100 0 0 0 0 0 ))
    ((4 . 4) (0 0 0 0 0 0 0 0 100 0 0 0 0 ))
    ((4 . 5) (0 0 0 0 0 0 0 0 0 100 0 0 0 ))
    ((5 . 1) (0 0 0 0 0 0 100 0 0 0 0 0 0 ))
    ((5 . 2) (0 0 0 0 0 0 0 100 0 0 0 0 0 ))
    ((5 . 3) (0 0 0 0 0 0 0 0 100 0 0 0 0 ))
    ((5 . 4) (0 0 0 0 0 0 0 0 0 100 0 0 0 ))
    ((5 . 5) (0 0 0 0 0 0 0 0 0 0 100 0 0 ))
    ))
  ))

;;; =============================================================
;;; Globals

(defvar *resultsum* nil)
(defvar *results-version* nil)
(defvar *params->ccs* (make-hash-table :test #'equal))
(defvar *file->log* (make-hash-table :test #'equal))

;;; =============================================================
;;; Log Analysis

(defparameter *function-name-substitutions*
  '(("count_from_either" . :cfe)
    ("min" . :min)
    ("count_from_one_once" . :cf1x1)
    ("count_from_one_twice" . :cf1x2)
    ("random" . :rand)
    ("retrieval" . :ret)
    ("dynamic_retrival" . :dynaret)
    ("used" . :used)
    ("trying" . :trying)
    ("!" . :!)
    ))

;;; =============================================================
;;; Math

(defun compare (prediction-set data-set)
  ;; WWW assumes that the problems are in the same order !!!
  (let* ((pairs (loop for a in (loop for prediction in prediction-set append (mapcar #'(lambda (v) (* v 100)) (sixth prediction)))
		      for b in (loop for data in data-set append (second data))
		      collect (list a b))))
    (stats::correlation-coefficient pairs)))

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
(defparameter *strat-keys* '(:ret :cfe :min :cf1x1 :cf1x2 :rand :dynaret :allret)) ;; :allret is the computed sum of :ret + :dynaret
(defvar *strat-key->correct+incorrect* (make-hash-table :test #'equal))

(defun analyze (&key (low *low*) (high *high*) &aux  (ts (get-universal-time)))
  (setq *results-version* nil)
  (clrhash *file->log*)
  (clrhash *params->ccs*)
  (load-data low high)
  (summarize-logs ts)
  (summarize-coefs ts)
  )

(defun load-data (low high &aux first-fno last-fno label temp constrain-by-label)
  (setq *logs* nil)
  (setf constrain-by-label (if (or low high) nil t))
  (if (null low) (setq low 0))
  (if (null high) (setq high 99999999999999))
  (if (> low high) (setf temp high high low low temp)) ;; Idiot corrector
  ;; Load all the data, do some preliminary analysis, and store
  ;; partial results for report production
  (loop for file in (directory "runlogs/*.lisp")
        with target-label = nil
	as fno = (parse-integer (pathname-name file))
	as log = (with-open-file (i file) (cdr (read i)))
	do
	(let* ((params (cdr (assoc :params log)))
	       (this-label (second (assoc :experiment_label params)))
	       (store-log
		(if constrain-by-label
		    (if target-label
			(if (string-equal this-label target-label) log)
		      (progn (setf target-label this-label) log))
		  (if (and (>= fno low) (<= fno high)) log))))
	  (when store-log 
	    (clean-up store-log) ;; This smashes the :run entry
	    (setf (gethash file *file->log*) store-log)))
	finally (setf *heuristicated-experiment-label* target-label)
	))
	
;; Turns out that for various Obiwon reasons there are problem blocks
;; with the wrong number of problems, and missing table dumps, usually
;; at the beginning and end. This removes these just so that the rest
;; of the code can run generically. UUU FFF This should be fixed up in
;; the sim, not here! FFF

(defun clean-up (log)
  (let ((pbs (second (assoc :problem-bin-size (cdr (assoc :head log))))))
    (setf (cdr (assoc :run log))
	  (loop for (nil . pb) in (cdr (assoc :run log))
		as ps = (cdr (assoc :problems pb))
		as np = (length ps)
		when (and (= np pbs) ;; Right number of problems?
			  ;; And has all the right content?
			  (assoc :results-prediction-table pb)
			  (assoc :strategy-prediction-table pb))
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
	 (format o "# ~a~%" *heuristicated-experiment-label*) 
	 (format o "i	n")
	 (loop for (key) in *comparator-datasets* do (format o "	~a" key))
	 (loop for s in *strat-keys* do (format o "	~a_n	~a_log_%	~a_+	~a_+%" s s s s))
	 (format o "~%")
	 ;; A problem-block entry looks like this:
	 ;; (:problem-block
	 ;;  (:problems
	 ;;   ((:used retrieval 3 + 3 = 6) )
	 ;;   ((:trying count_from_one_once 2 + 3) (:used count_from_one_once 2 + 3 = 4) )
	 ;;   ((:trying count_from_one_once 3 + 2) (:used count_from_one_once 3 + 2 = 5) )
	 ;;   ((:used retrieval 5 + 3 = 5) )
	 ;;   ((:trying min 1 + 3) (:used min 1 + 3 = 4) )
	 ;;   ((:used retrieval 3 + 5 = 8) )
	 ;;   ((:trying count_from_one_once 1 + 4) (:used count_from_one_once 1 + 4 = 4) )
	 ;;   ((:trying min 3 + 1) (:used min 3 + 1 = 3) )
	 ;;   ((:trying count_from_one_once 3 + 4) (:used count_from_one_once 3 + 4 = 6) )
	 ;;   ((:trying min 4 + 1) (:used min 4 + 1 = 5) )
	 ;;   ...
	 ;;   ) ; close problems
         ;;  (:results-prediction-table
	 ;;    (1 + 1 = 9 (0.66801  0.43552  0.95849  -0.91478  -0.14068  0.99803  -0.87101  0.95384  0.89992  1.0  -0.54585  0.99997  -0.84957))
	 ;;    (1 + 2 = 9 (-0.5814  -0.7126  -0.4912  -0.95835  -0.91221  0.99846  -0.91654  0.98572  -0.04236  0.99999  -0.90574  0.99792  -0.959))
	 ;;    (1 + 3 = 9 (0.69823  0.11624  0.81191  -0.95802  -0.39655  0.99689  -0.98875  0.98272  0.95301  0.99994  -0.96618  0.99949  -0.92495))
	 ;;    ...
         ;;    )
         ;;  (:strategy-prediction-table
	 ;;    (1 + 1 = min (0.40504  -0.95853  -0.08923  0.65204))
	 ;;    (1 + 2 = count_from_either (-0.02833  -0.61649  0.61381  -0.91041))
	 ;;    (1 + 3 = count_from_one_twice (0.71503  -0.85172  0.08936  -0.81898))
	 ;;    ...
         ;;    )
         ;;  ) ;; close :problem-block
	 (loop for pb in (cdr (assoc :run log))
	       as i from 1 by 1
	       as ps = (cdr (assoc :problems pb))
	       as np = (length ps)
	       as rnnpt = (cdr (assoc :Results-prediction-table pb))
	       as ccs = (loop for (key data) in *comparator-datasets* 
			      collect `(,key ,(compare rnnpt data)))
	       do 
	       (push `((:i ,i) (:ccs ,ccs)) (gethash file *file->summary*))
	       (format o "~a	~a" i np)
	       (loop for (nil cc) in ccs do (format o "	~a" cc))
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
	       ;; Special computation for :allret (+ :ret :dynaret)
	       #| WILL HAVE TO REPAIR 
	       (let ((r (gethash :ret *strat-key->correct+incorrect*))
		     (d (gethash :dynaret *strat-key->correct+incorrect*))
		     (a (gethash :allret *strat-key->correct+incorrect*)))
		 (setf (car a) (+ (car r) (car d)))		 
		 (setf (cdr a) (+ (cdr r) (cdr d))))
	       |# 
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

(defparameter *param-reporting-order* 
  '(
    ("DECR_on_WRONG" . DECR_on_WRONG)
    ("non_result_y_filler" . non_result_y_filler)
    ("initial_counting_network_burn_in_epochs" . initial_counting_network_burn_in_epochs)
    ("DR_threshold " . DR_threshold )
    ("initial_counting_network_learning_rate" . initial_counting_network_learning_rate)
    ("experiment_label" . experiment_label)
    ("RETRIEVAL_HIGH_CC" . RETRIEVAL_HIGH_CC)
    ("INCR_the_right_answer_on_WRONG" . INCR_the_right_answer_on_WRONG)
    ("STRATEGY_LOW_CC" . STRATEGY_LOW_CC)
    ("STRATEGY_HIGH_CC" . STRATEGY_HIGH_CC)
    ("addend_matrix_offby1_delta" . addend_matrix_offby1_delta)
    ("RETRIEVAL_LOW_CC" . RETRIEVAL_LOW_CC)
    ("PERR" . PERR)
    ("learning_rate" . learning_rate)
    ("in_process_training_epochs" . in_process_training_epochs)
    ("INCR_on_RIGHT" . INCR_on_RIGHT)
    ("n_problems" . n_problems)
    ))

(defvar *params->final-coefs* (make-hash-table :test #'equal))
(defvar *params->all-values* (make-hash-table :test #'equal)) ;; Let's us tell which ones actually changed.

(defun summarize-coefs (ts &aux file->coefs)
  ;; Dump summaries.
  (clrhash *params->all-values*)
  ;; As we go along we carry forward the data required to make a pivot
  ;; csv of the final results for statistical analysis in R.
  (clrhash *params->final-coefs*)
  ;; Dump params:
  (with-open-file 
   (o (print (format nil "sumstats/~a-mastersummary.xls" ts)) :direction :output :if-exists :supersede)
   ;; Report params
   (format o "# ~a~%" *heuristicated-experiment-label*) 
   (loop for (ps . pn) in *param-reporting-order*
	 do 
	 (format o "~a" (print pn))
	 (loop for file being the hash-keys of *file->summary*
	       as params = (cdr (assoc :params (gethash file *file->log*)))
	       as pv = (second (assoc ps params :test #'string-equal))
	       ;; These are repeated once for each dataset bcs there'll be that many coefs
	       do 
	       (pushnew pv (gethash pn *params->all-values*) :test (if (stringp pv) #'string-equal #'equal))
	       (loop for (nil) in *comparator-datasets*
		     do (format o "	~a" pv)))
	 (format o "~%"))
   ;; Report coefs -- WWW THIS DEPENDS UPON HASH TABLES SCANNNING DETERMINISTICALLY !!!
   ;; Sub Header to distinguish datasets
   (loop for file being the hash-keys of *file->summary*
	 do (loop for (key) in *comparator-datasets* do (format o "	~a" key)))
   (format o "~%")
   ;; (FFF %%% This is sooooooooooo inefficient -- scanning these
   ;; tables over and over and over again, but there's no a whole lot
   ;; of data here, so what the hey!)
   (loop for file being the hash-keys of *file->summary*
	 as nn = (substitute #\_ #\space (pathname-name file))
	 do (loop for (nil) in *comparator-datasets* do (format o "	_~a_" nn))) ;; _..._ so that excel doesn't turn large numbers to E-notation
   (format o "~%")
   ;; Find the highest value
   (let ((maxi (loop for data being the hash-values of *file->summary*
		     with max = 0
		     as newmax = (reduce #'max (loop for d in data collect (second (assoc :i d))))
		     do (setf max (max max newmax))
		     finally (return max))))
     ;; Now print each i for each file
     (loop for i from 1 to maxi
	   do 
	   (format o "~a" i)
	   ;; UUU This is mega ugly, pushing the final values from
	   ;; each loop through all data into the table for pivot
	   ;; recovery later.
	   (loop for file being the hash-keys of *file->summary*
		 using (hash-value data)
		 as params = (cdr (assoc :params (gethash file *file->log*)))
		 as idata = (find i data :test #'(lambda (a b) (= a (second (assoc :i b)))))
		 as ccs = (second (assoc :ccs idata))
		 do 
		 (loop for (nil cc) in ccs
		       do (format o "	~a" cc))
		 ;; Store the final value for pivot reporting later. 
		 (when (= i maxi)
		   (let ((coefs (mapcar #'second ccs)))
		     (push coefs (gethash params *params->final-coefs*))
		     (push (cons coefs file) file->coefs) ;; This is so ugly it makes me cry!
		     )))
	   (format o "~%"))))
  ;; Finally, dump the R-ready csv file.
  ;; Figure out which parameters actually change!
  (let ((pns-that-change 
	 (loop for pn being the hash-keys of *params->all-values*
	       using (hash-value pvs)
	       when (cdr pvs)
	       collect pn)))
    (with-open-file 
     (o (print (format nil "sumstats/~a-FinalPivotforR.csv" ts)) :direction :output :if-exists :supersede) 
     (format o "# ~a~%" *heuristicated-experiment-label*) 
     (format o "file")
     (loop for (nil . pn) in *param-reporting-order*
	   when (member pn pns-that-change)
	   do (format o ",~a" pn))
     (loop for (key) in *comparator-datasets* do (format o ",~a" key))
     (format o "~%")
     (loop for p* being the hash-keys of *params->final-coefs*
	   using (hash-value coefs)
	   do 
	   (loop for coef in coefs
		 do 
		 (format o "_~a_" (substitute #\_ #\space (pathname-name (cdr (assoc coef file->coefs)))))
		 (loop for (ps . pn) in *param-reporting-order*
		       when (member pn pns-that-change)
		       do 
		       (format o ",~a" (second (assoc ps p* :test #'string-equal))))
		 (loop for c in coef do (format o ",~a" c))
		 (format o "~%")))
     ))
  )

(untrace)
;(trace compare stats::correlation-coefficient)
(analyze)
