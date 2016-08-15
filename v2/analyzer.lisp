;(load (compile-file "analyzer.lisp"))

(defparameter *param-reporting-order* 
  '(
    ("experiment_label" . experiment_label)
    ("read_input_from_file" . read_input_from_file)
    ("n_problems" . n_problems)
    ("strategy_hidden_units" . strategy_hidden_units)
    ("results_hidden_units" . results_hidden_units)
    ("initial_counting_network_burn_in_epochs" . initial_counting_network_burn_in_epochs)
    ("initial_counting_network_learning_rate" . initial_counting_network_learning_rate)
    ("DR_threshold" . DR_threshold)
    ("PERR" . PERR)
    ("RETRIEVAL_LOW_CC" . RETRIEVAL_LOW_CC)
    ("RETRIEVAL_HIGH_CC" . RETRIEVAL_HIGH_CC)
    ("STRATEGY_LOW_CC" . STRATEGY_LOW_CC)
    ("STRATEGY_HIGH_CC" . STRATEGY_HIGH_CC)
    ("initial_weight_narrowing_divisor" . initial_weight_narrowing_divisor)
    ("strategy_learning_rate" . strategy_learning_rate)
    ("results_learning_rate" . results_learning_rate)
    ("per_problem_training_epochs" . per_problem_training_epochs)
    ("anti_1_bit" . anti_1_bit)
    ("ndups" . ndups)
    ("pbs" . pbs)
    ("dynamic_retrieval_on" . dynamic_retrieval_on)
    ("addend_representation" . addend_representation)
    ("n_addend_bits" . n_addend_bits)
    ("addend_one_bits" . addend_one_bits)
    ("results_representation" . results_representation)
    ("n_results_bits" . n_results_bits)
    ("results_one_bits" . results_one_bits)
    ("n_strat_bits" . n_strat_bits)
    ("strat_one_bits" . strat_one_bits)
    ))

(defparameter *function-name-substitutions*
  '(("count_from_either" . :cfe)
    ("count_up_by_one_from_second_addend" . :upx1)
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

;;; ================================================================
;;; Data from Siegler and Shrager 1984 -- Note that this data is under
;;; overt strategy supression instruction!

(defvar *current-comparator-datasets* nil)

(defparameter *all-comparator-datasets* 
  '(
    (:sns84
     ;; All these are ranged between 0 and 100:
     (
      ;;         1  2  3  4  5  6  7  8  9 10
      ((1 . 1) (05 86 00 02 00 02 00 00 00 00))
      ((1 . 2) (00 09 70 02 00 04 00 00 07 02))
      ((1 . 3) (02 00 11 71 05 02 02 00 00 00))
      ((1 . 4) (00 00 00 11 61 09 07 00 00 00))
      ((1 . 5) (00 00 00 13 16 50 11 00 02 02))
      ((2 . 1) (07 05 79 05 00 00 00 00 00 00))
      ((2 . 2) (00 04 05 80 04 00 05 00 00 00))
      ((2 . 3) (00 04 07 38 34 09 02 02 02 00))
      ((2 . 4) (02 00 07 02 43 29 07 07 00 00))
      ((2 . 5) (02 00 05 02 16 43 13 00 00 02))
      ((3 . 1) (02 00 09 79 04 00 04 00 00 00))
      ((3 . 2) (00 09 11 11 55 07 00 00 00 00))
      ((3 . 3) (00 00 05 21 09 48 00 02 02 02))
      ((3 . 4) (00 00 05 11 23 14 29 02 00 00))
      ((3 . 5) (00 00 07 00 13 23 14 18 00 05))
      ((4 . 1) (00 04 02 09 68 02 02 07 00 00))
      ((4 . 2) (00 07 09 00 20 36 13 07 00 02))
      ((4 . 3) (00 00 05 18 09 09 38 09 00 02))
      ((4 . 4) (00 00 02 02 29 07 07 34 00 04))
      ((4 . 5) (00 00 00 04 09 16 09 11 18 11))
      ((5 . 1) (00 04 00 04 07 71 04 04 00 04))
      ((5 . 2) (00 05 20 02 18 27 25 02 00 02))
      ((5 . 3) (00 02 11 09 18 05 16 23 00 05))
      ((5 . 4) (00 00 00 11 21 16 05 11 16 04))
      ((5 . 5) (00 00 00 00 07 25 11 02 04 34))
      ))

    (:base-p/r/c
     ;; This represents primacy, recency, and next biases. We assign 33%
     ;; to each addend, then 16% to the next of each. (You might think
     ;; that by recency, the latter addend should get a little more, but
     ;; this is counter-balanced by primacy, where the first one
     ;; should...so we just call it even.)
     (
      ;;         1  2  3  4  5  6  7  8  9 10
      ((1 . 1) (66 33 00 00 00 00 00 00 00 00))
      ((1 . 2) (33 33 33 00 00 00 00 00 00 00))
      ((1 . 3) (33 00 33 33 00 00 00 00 00 00))
      ((1 . 4) (33 16 00 33 16 00 00 00 00 00))
      ((1 . 5) (33 16 00 00 33 16 00 00 00 00))
      ((2 . 1) (33 49 16 00 00 00 00 00 00 00))
      ((2 . 2) (00 66 33 00 00 00 00 00 00 00))
      ((2 . 3) (00 33 33 33 00 00 00 00 00 00))
      ((2 . 4) (00 33 16 33 16 00 00 00 00 00))
      ((2 . 5) (00 33 16 00 33 16 00 00 00 00))
      ((3 . 1) (33 16 33 16 00 00 00 00 00 00))
      ((3 . 2) (00 00 66 33 00 00 00 00 00 00))
      ((3 . 3) (00 00 66 33 00 00 00 00 00 00))
      ((3 . 4) (00 00 33 33 33 00 00 00 00 00))
      ((3 . 5) (00 00 33 16 33 16 00 00 00 00))
      ((4 . 1) (33 16 00 33 16 00 00 00 00 00))
      ((4 . 2) (00 33 16 33 16 00 00 00 00 00))
      ((4 . 3) (00 00 16 49 16 00 00 00 00 00))
      ((4 . 4) (00 00 00 66 33 00 00 00 00 00))
      ((4 . 5) (00 00 00 33 33 33 00 00 00 00))
      ((5 . 1) (33 16 00 00 33 16 00 00 00 00))
      ((5 . 2) (00 33 16 33 16 00 00 00 00 00))
      ((5 . 3) (00 00 33 16 33 16 00 00 00 00))
      ((5 . 4) (00 00 00 33 49 16 00 00 00 00))
      ((5 . 5) (00 00 00 00 66 33 00 00 00 00))
      ))

    (:base-exact
     ;; This preload a, b, and b+1, which is exactly what the training
     ;; does.
     (
      ;;         1  2  3  4  5  6  7  8  9 10
      ((1 . 1) (33 66 00 00 00 00 00 00 00 00))
      ((1 . 2) (33 33 33 00 00 00 00 00 00 00))
      ((1 . 3) (33 00 33 33 00 00 00 00 00 00))
      ((1 . 4) (33 00 00 33 33 00 00 00 00 00))
      ((1 . 5) (33 00 00 00 33 33 00 00 00 00))
      ((2 . 1) (33 66 00 00 00 00 00 00 00 00))
      ((2 . 2) (00 66 33 00 00 00 00 00 00 00))
      ((2 . 3) (00 33 33 33 00 00 00 00 00 00))
      ((2 . 4) (00 33 00 33 33 00 00 00 00 00))
      ((2 . 5) (00 33 00 00 33 33 00 00 00 00))
      ((3 . 1) (33 33 33 00 00 00 00 00 00 00))
      ((3 . 2) (00 33 66 00 00 00 00 00 00 00))
      ((3 . 3) (00 00 66 33 00 00 00 00 00 00))
      ((3 . 4) (00 00 33 33 33 00 00 00 00 00))
      ((3 . 5) (00 00 33 00 33 33 00 00 00 00))
      ((4 . 1) (33 33 00 33 00 00 00 00 00 00))
      ((4 . 2) (00 33 33 33 00 00 00 00 00 00))
      ((4 . 3) (00 00 33 66 00 00 00 00 00 00))
      ((4 . 4) (00 00 00 66 33 00 00 00 00 00))
      ((4 . 5) (00 00 00 33 33 33 00 00 00 00))
      ((5 . 1) (33 00 00 00 33 00 00 00 00 00))
      ((5 . 2) (33 33 00 00 33 00 00 00 00 00))
      ((5 . 3) (00 00 33 33 33 00 00 00 00 00))
      ((5 . 4) (00 00 00 33 66 00 00 00 00 00))
      ((5 . 5) (00 00 00 00 66 33 00 00 00 00))
      ))

    (:adult 
     (
      ;;         1  2  3  4  5  6  7  8  9 10
      ((1 . 1) (00 99 00 00 00 00 00 00 00 00))
      ((1 . 2) (00 00 99 00 00 00 00 00 00 00))
      ((1 . 3) (00 00 00 99 00 00 00 00 00 00))
      ((1 . 4) (00 00 00 00 99 00 00 00 00 00))
      ((1 . 5) (00 00 00 00 00 99 00 00 00 00))
      ((2 . 1) (00 00 99 00 00 00 00 00 00 00))
      ((2 . 2) (00 00 00 99 00 00 00 00 00 00))
      ((2 . 3) (00 00 00 00 99 00 00 00 00 00))
      ((2 . 4) (00 00 00 00 00 99 00 00 00 00))
      ((2 . 5) (00 00 00 00 00 00 99 00 00 00))
      ((3 . 1) (00 00 00 99 00 00 00 00 00 00))
      ((3 . 2) (00 00 00 00 99 00 00 00 00 00))
      ((3 . 3) (00 00 00 00 00 99 00 00 00 00))
      ((3 . 4) (00 00 00 00 00 00 99 00 00 00))
      ((3 . 5) (00 00 00 00 00 00 00 99 00 00))
      ((4 . 1) (00 00 00 00 99 00 00 00 00 00))
      ((4 . 2) (00 00 00 00 00 99 00 00 00 00))
      ((4 . 3) (00 00 00 00 00 00 99 00 00 00))
      ((4 . 4) (00 00 00 00 00 00 00 99 00 00))
      ((4 . 5) (00 00 00 00 00 00 00 00 99 00))
      ((5 . 1) (00 00 00 00 00 99 00 00 00 00))
      ((5 . 2) (00 00 00 00 00 00 99 00 00 00))
      ((5 . 3) (00 00 00 00 00 00 00 99 00 00))
      ((5 . 4) (00 00 00 00 00 00 00 00 99 00))
      ((5 . 5) (00 00 00 00 00 00 00 00 00 99))
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

;;; =============================================================
;;; Math

;;; The incoming value sets (sixth prediction) is as: ((1 0.28451) (2
;;; 0.21634) ... (8 0.22031) (9 0.1906) (10 0.16163)) The second of
;;; each is what we want. And since these are ERROR averages, we need
;;; to substract them from 1 to get correct probabilities, which is
;;; what the correlation wants to see (otherwise good rs will be
;;; negative!)

(defun compare (prediction-set data-set)
  ;; WWW assumes that the problems are in the same order !!!
  (let* ((pairs (loop for a in (loop for prediction in prediction-set append (mapcar #'(lambda (n&v) (* (- 1 (second n&v)) 100)) (sixth prediction)))
		      for b in (loop for data in data-set append (second data))
		      collect (list a b))))
    (stats::correlation-coefficient pairs)))

;;; =============================================================
;;; Utils

(defun dht (table &optional (n 10))
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
(defparameter *strat-keys* '(:ret :cfe :upx1 :min :cf1x1 :cf1x2 :rand :dynaret :allret)) ;; :allret is the computed sum of :ret + :dynaret
(defvar *strat-key->correct+incorrect* (make-hash-table :test #'equal))

(defun analyze (&key (low *low*) (high *high*) its/p/v comps &aux r (ts (get-universal-time)))
  (setf *current-comparator-datasets* nil)
  (setf *current-comparator-datasets*
	(cond ((member comps '(:all nil)) *all-comparator-datasets*)
	      (t (loop for key in comps
		       as v = (assoc key *all-comparator-datasets*)
		       if v
		       collect v
		       else 
		       do (break "Missing comp ~a" key)))))
  (setq *results-version* nil)
  (clrhash *file->log*)
  (clrhash *params->ccs*)
  (load-data :low low :high high :its/p/v its/p/v :ts ts)
  (summarize-logs ts)
  (summarize-final-strategy-prefs ts)
  (summarize-coefs ts)
  )

(defvar *d* nil)
(defun ltdftcfociatltd (file) ;; load-the-data-from-the-compiled-file-or-compile-it-and-then-load-the-data
  (let ((cfn (format nil "runlogs/~a.dx32fsl" (pathname-name file))))
    (unless (probe-file cfn) 
      (format t "~a doesn't exist, compiling from ~a~%" file cfn)
      (compile-file file))
    (load (print cfn))
    *d*))

(defun load-data (&key ts low high its/p/v for-indexing? &aux first-fno last-fno label temp constrain-by-label)
  (setq *logs* nil)
  (setf constrain-by-label (if (or low high) nil t))
  (if (null low) (setq low 0))
  (if (null high) (setq high 99999999999999))
  (if (> low high) (setf temp high high low low temp)) ;; Idiot corrector
  ;; Load all the data, do some preliminary analysis, and
  ;; store partial results for report production; make a compressed
  ;; log while we're here.
  (with-open-file 
   (logsum (print (format nil "sumstats/~a-logsum.lisp" ts))
	   :direction :output :if-exists :supersede) 
   (block load-data-inner ;; If you don't trap this, the logsum file gets disappeared!
	  (loop for file in (or (and its/p/v (its/p/v->files its/p/v)) (downsorted-directory "runlogs/*.lisp"))
		with target-label = nil
		as fno = (parse-integer (pathname-name file))
		as log = (when (and (>= fno low) (<= fno high))
			   (cdr (ltdftcfociatltd file)))
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
				     (format t "Only reading logs with label: ~s~%" target-label)
				     (setf *heuristicated-experiment-label* target-label)
				     log))
			  (if (and (>= fno low) (<= fno high)) log))))
		  (when store-log 
		    (pprint `((:fn ,(pathname-name file))
			      (:params ,params)
			      ,(assoc :head log))
			    logsum)
		    (if for-indexing?
			(setq store-log (assoc :params log)) ;; This smashes the :run entry
		      ;; Drop everything but the params
		      (clean-up store-log))
		    (setf (gethash file *file->log*) store-log)))
		))))
	
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
  (let ((pbs (second (assoc :pbs (cdr (assoc :params log))))))
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
	 ;; output headers
	 (if *heuristicated-experiment-label*
	     (format o "# ~a~%" *heuristicated-experiment-label*)
	   (format o "# low=~a, high=~a~%" *low* *high*))
	 (format o "i	n")
	 (loop for (key) in *current-comparator-datasets* do (format o "	~a" key))
	 (loop for s in *strat-keys* do (format o "	~a_n	~a_log_%	~a_+	~a_+%" s s s s))
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
	       as corcoefs = (loop for (key data) in *current-comparator-datasets* 
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
   (if *heuristicated-experiment-label*
       (format o "# ~a~%" *heuristicated-experiment-label*)
     (format o "# low=~a, high=~a~%" *low* *high*))
   (loop for (ps . pn) in *param-reporting-order*
	 do 
	 (format o "~a" (print pn))
	 (loop for file being the hash-keys of *file->summary*
	       as params = (cdr (assoc :params (gethash file *file->log*)))
	       as pv = (second (assoc ps params :test #'string-equal))
	       ;; These are repeated once for each dataset bcs there'll be that many coefs
	       do 
	       (pushnew pv (gethash pn *params->all-values*) :test (if (stringp pv) #'string-equal #'equal))
	       (loop for (nil) in *current-comparator-datasets*
		     do (format o "	~a" pv)))
	 (format o "~%"))
   ;; Report coefs -- WWW THIS DEPENDS UPON HASH TABLES SCANNNING DETERMINISTICALLY !!!
   ;; Sub Header to distinguish datasets
   (loop for file being the hash-keys of *file->summary*
	 do (loop for (key) in *current-comparator-datasets* do (format o "	~a" key)))
   (format o "~%")
   ;; (FFF %%% This is sooooooooooo inefficient -- scanning these
   ;; tables over and over and over again, but there's no a whole lot
   ;; of data here, so what the hey!)
   (loop for file being the hash-keys of *file->summary*
	 as nn = (substitute #\_ #\space (pathname-name file))
	 do (loop for (nil) in *current-comparator-datasets* do (format o "	_~a_" nn))) ;; _..._ so that excel doesn't turn large numbers to E-notation
   (format o "~%")
   ;; Find the highest value.
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
		 as corcoefs = (second (assoc :corcoefs idata))
		 do 
		 (loop for (nil cc) in corcoefs
		       do (format o "	~a" cc))
		 ;; Store the final value for pivot reporting later. 
		 (when (= i maxi)
		   (let ((coefs (mapcar #'second corcoefs)))
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
     (loop for (key) in *current-comparator-datasets* do (format o ",~a" key))
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

#| I don't think that this works anymore; it hasn't been used in a while:

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
|#

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

;;; When For dealing with large set of results, say > 300, you can
;;; only analyze subset at a time, these let you pivot the dataset on
;;; a particular variable. First you have to make a dataset index.

(defvar *index-file-name* "")
(defvar *file->params* (make-hash-table :test #'equal))

(defun index-dataset (&aux lfn) ;; Assumes you're doing all the same experiment label
  (load-data :for-indexing? t)
  (let ((ts (get-universal-time)))
    (with-open-file
     (o (setf *index-file-name* (format nil "runlogs/~a.index" ts))
	:direction :output)
     (loop for path being the hash-keys of *file->log*
	   using (hash-value params)
	   do (print (cons (pathname-name path) params) o)))
    (load-index ts)
    ))

(defun load-index (&optional ts)
  (unless (or ts *index-file-name*)
    (break "You need to either have just created an index, or else give an index timestamp."))
  (if ts (setf *index-file-name* (format nil "runlogs/~a.index" ts)))
  (format t "Loading index from ~a~%" *index-file-name*)
  (clrhash *file->params*)
  (with-open-file
   (i *index-file-name*)
   (loop for entry = (read i nil nil)
	 until (null entry)
	 do (setf (gethash (car entry) *file->params*) (cdr entry))))
  (invert-index)
  *index-file-name*
  )

(defvar *param->values/files* (make-hash-table :test #'equal))
(defun invert-index () 
  (clrhash *param->values/files*)
  (loop for tss being the hash-keys of *file->params* ;; ts is a string, thus tss
	using (hash-value params)
	do (loop for (p v) in (cdr params) ;; Gets the :params off
		 as curs = (gethash p *param->values/files*) ;; This will be like (...(5 f1 f2 f3...)...)
		 as cvfs = (assoc v curs :test #'equal)
		 do (if cvfs (setf (cdr cvfs) (cons tss (cdr cvfs)))
		      (push (list v tss) (gethash p *param->values/files*))))))
		 
(defun report-index-diffs ()
  (loop for p being the hash-keys of *param->values/files*
	using (hash-value v/fs-sets)
	when (cdr v/fs-sets)
	do (format t "~a has: " p)
	(loop for (v . fs) in v/fs-sets
	      do (format t " :~a @ ~a, " (length fs) v))
	(format t "~%")))

(defun get-index-files (param value)
  (cdr (assoc value (gethash param *param->values/files*) :test #'equal)))

;;; ITS/P/V stands for index(i.e.,time stamp)/param/value which allows
;;; you to pull the runs that have a particular get of values.

(defun its/p/v->files (its/p/v)
  (if (and (= 3 (length its/p/v))
	     (numberp (first its/p/v))
	     (keywordp (second its/p/v))
	     (numberp (third its/p/v))
	     (load-index (first its/p/v)))
      (get-index-files (second its/p/v) (third its/p/v))
    (break "In its/p/v->files: ~a isn't a valid index/param/value triple!" its/p/v)))

(untrace)
;(trace compare)
; Possible :comps (defined at the top of the file) are: :sns84 :base-p/r/c :base-exact :adult
;(analyze :its/p/v '(3676977173 :INITIAL_COUNTING_NETWORK_LEARNING_RATE 0.3) :comps '(:base-exact :adult))
;(analyze :its/p/v '(3676977173 :initial_counting_network_burn_in_epochs 5000) :comps '(:base-exact :adult))
;(analyze :low 20160709154946  :comps '(:base-exact :adult))
;(analyze :comps '(:adult))
;(analyze :low 20160814182645 :high 20160814191640 :comps '(:base-exact :adult))
(analyze :comps '(:base-exact :adult))
