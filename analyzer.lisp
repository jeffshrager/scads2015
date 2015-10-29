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
(defvar *file->data* (make-hash-table :test #'equal))

;;; =============================================================
;;; File Reader and Parser

;;; FFF Make sure that all the files read have the same version
;;; number. Break if not!

;;; Line length differs in different lisps bcs they differntially
;;; compress the crlf to one char.
(defun lline (l)
  #+ccl (1- (length l))
  #+clisp (length l))

(defun load-result-file (file)
  (format t "~%Loading ~a~%" file)
  (with-open-file 
   (i file)
   (let* ((vline (read-line i nil nil))
	  ;; 1- bcs of the return on the end
	  (version (parse-integer (subseq vline  (1+ (position #\, vline)) (lline vline)))))
     (if (null *results-version*)
	 (setq *results-version* version)
       (unless (equal version *results-version*)
	 (break "Uh oh. Some of the data is from version ~a, and some is from version ~a of the results format. I don't know how to handle this situation!"
		*results-version* version)))
     (case version
	   (20150813 (parse-20150807-data i))
	   (t (break "Unknown results version, vline=~s" vline))))))

(defun parse-20150807-data (i)
  (loop for line = (read-line i nil nil)
	with n = 0 
	with logs = nil
	with local = nil
	with rnnpt = nil
	until (null line)
	finally (break "parse-20150807-data hit the end of the stream: ~s" i)
       	do (if (search "Results NN Prediction table" line)
	       (progn 
		 (setq rnnpt (parse-rnnp-table i))
		 (push `((:n ,(incf n))
			 (:log ,(reduce-log (reverse local)))
			 (:rnnpt ,rnnpt))
		       logs)
		 (setq local nil rnnpt nil))
	     (if (search "Run Parameters" line) ;; should follow an rnnpt, so don't have to push held log entries
		 (progn 
		   (if (or local rnnpt)
		       (format t "WARNING: In parse-20150807-data, rnnpt seems to be followed by log entries incorrectly. These will be lost!"))
		   (return-from 
		    parse-20150807-data 
		    `((:params ,(let ((params (parse-params i)))
				  (setq *heuristicated-experiment-label* 
					(cdr (assoc "settings.experiment_label" params :test #'string-equal)))
				  params))
		      (:logs ,(reverse logs))
		      (:rd-table ,(parse-rd-table i)))))
	       (push (interpret-log-entry (string-split (subseq line 0 (lline line)))) local)))))

(defun parse-rnnp-table (i)
  (prog1 
  (loop for a from 1 to 5
	    append (loop for b from 1 to 5
			 collect (cons (cons a b)
				       (mapcar #'read-from-string 
					       ;; Drop the first thing, which is just the problem statement
					       (cdr (string-split (read-line i nil nil)))))))
  (read-line i nil nil) ;; Skip the tail line
  ))

(defun parse-rd-table (i)
  (loop for a from 1 to 5
	    append (loop for b from 1 to 5
			 collect (cons (cons a b)
				       (mapcar #'read-from-string 
					       ;; Drop the first thing, which is just the problem statement
					       (cdr (string-split (read-line i nil nil))))))))

(defun parse-params (i)
  (loop for line = (read-line i nil nil)
	until (search "=== END OF DATA ===" line)
	collect (let ((p (position #\, line)))
		  (when p
		    (cons (subseq line 0 p)
			  (subseq line (+ p 1) (lline line)))))))

;;; Special purpose scanner to figure out which files to load.

(defvar *label->files* (make-hash-table :test #'equal))
(defvar *filename->label* (make-hash-table :test #'equal))
(defvar *filename->pathname* (make-hash-table :test #'equal))
(defvar *all-filenames* nil)

(defun most-recent-set-of-results-pathnames-by-label-mathcing ()
  ;; Collect all the labels and assocated filenames (numbers)
  (setq *all-filenames* nil)
  (clrhash *label->files*)
  (clrhash *filename->label*)
  (clrhash *filename->pathname*)
  (loop for pathname in (directory "test_csv/*.csv")
	as filename = (pathname-name pathname)
	as label = (with-open-file
		    (i pathname)
		    (format t ".")
		    (loop for l = (read-line i nil nil)
			  until (or (null l) (search "experiment_label" l))
			  finally (return (if l (subseq l 26 (lline l))))))
	when label
	do 
	(push filename (gethash label *label->files*))
	(setf (gethash filename *filename->label*) label)
	(push filename *all-filenames*)
	(setf (gethash filename *filename->pathname*) pathname)
	)
  ;; Now find out which label has the "highest" filename and get all
  ;; the filenames assocated with that label.
  (loop for filename in (sort (copy-list 
			       (gethash (gethash 
					 (car (sort (copy-list *all-filenames*) #'string>)) 
					 *filename->label*) *label->files*)) #'string<)
	collect (gethash filename *filename->pathname*)))

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

(defun compare (result-set target)
  (let* ((pairs (loop for a in (loop for (nil obs) in target
				     append obs)
		      ;; Slightly confusingly uses the same lables as
		      ;; the above, but there are never actually used.
		      for b in (loop for (problem) in target 
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

(defvar *d* nil) ;; This is just a drop so that we can look at an
		 ;; example of the data after the fact.

(defvar *file->summary* (make-hash-table :test #'equal))

;;; This is the order in which they'll show up in the analyzed
;;; results.
(defparameter *strat-keys* '(:ret :cfe :min :cf1x1 :cf1x2 :rand :dynaret :allret)) ;; :allret is the computed sum of :ret + :dynaret
(defvar *strat-key->correct+incorrect* (make-hash-table :test #'equal))

(defun analyze (&key (low *low*) (high *high*) &aux  (ts (get-universal-time)))
  (setq *results-version* nil *d* nil)
  (clrhash *file->data*)
  (clrhash *params->ccs*)
  (analyze-load-data low high)
  (analyze-summarize-logs ts)
  (analyze-summarize-coefs ts)
  )

(defun analyze-load-data (low high &aux first-fno last-fno label temp)
  (if (null low) (setq low 0))
  (if (null high) (setq high 99999999999999))
  (if (> low high) (setf temp high high low low temp)) ;; Idiot corrector
  ;; Load all the data, do some preliminary analysis, and store
  ;; partial results for report production
  (loop for file in (if (zerop low)
			(most-recent-set-of-results-pathnames-by-label-mathcing)
			(directory "test_csv/*.csv"))
	as fno = (parse-integer (pathname-name file))
	when (and (>= fno low) (<= fno high))
	do
	(let* ((r (ignore-errors (load-result-file file))))
	  (if r
	      (progn
		;; Save data for later
		(setf (gethash file *file->data*) r)
		(push r *d*)
		(let* ((p (second (assoc :params r))))
		  ;; Extract and check label
		  (let ((new-label (cdr (assoc "experiment_label" p :test #'string-equal))))
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
		  )) ;; If r
	    (format t "~a seems to be broken -- ignoring it!~%" file)
	    ))))

(defun analyze-summarize-logs (ts)
  ;; Report the retrieval fractions and % correct mean and serr for each dataset seprately
  ;; meanwhile storing the overall stats for summarization
  (clrhash *file->summary*)
  (loop for file being the hash-keys of *file->data*
	using (hash-value data)
	do 
	(with-open-file 
	 (o (print (format nil "sumstats/~a-~a-logsummary.xls" ts (substitute #\_ #\space (pathname-name file))))
	    :direction :output :if-exists :supersede) 
	 (format o "# ~a~%" *heuristicated-experiment-label*) 
	 (format o "i	n")
	 (loop for (key) in *comparator-datasets* do (format o "	~a" key))
	 (loop for s in *strat-keys*
	       do (format o "	~a_n	~a_log_%	~a_+	~a_+%" s s s s))
	 (format o "~%")

	 ;; The :log entry looks like this:
	 ;; (:LOG
	 ;;  (((:CF1X1 :DYNARET) 3 2 3) (:RET 1 3 2) (:RET 2 3 4) (:RET 2 4 6)
	 ;;   (:RET 4 5 6) (:RET 2 4 4) ((:CF1X1 :DYNARET) 3 5 4) (:RET 3 5 10)
	 ;;   (:RET 3 2 3) (:RET 5 4 4) (:RET 5 1 4) (:RET 1 3 2) (:RET 4 5 6)
	 ;;   (:RET 2 3 4) ((:CF1X1 :DYNARET) 5 3 3) (:RET 2 2 11) (:RET 3 4 5)
	 ;;   (:RET 4 4 6) (:RET 1 1 4) ((:MIN :DYNARET) 5 3 3) ((:CF1X1 :DYNARET) 5 5 5)
	 ;;   (:RET 3 5 8) (:RET 1 4 4) (:RET 4 3 7) (:RET 3 4 5)))

	 (loop for entry in (second (assoc :logs data))
	       as i = (second (assoc :n entry))
	       as log = (second (assoc :log entry))
	       as nlog = (length log)
	       as rnnpt = (second (assoc :rnnpt entry))
	       as ccs = (loop for (key data) in *comparator-datasets* 
			      collect `(,key ,(compare rnnpt data)))
	       do 
	       (push `((:i ,i) (:ccs ,ccs)) (gethash file *file->summary*))
	       (format o "~a	~a" i nlog)
	       (loop for (nil cc) in ccs do (format o "	~a" cc))
	       ;; Now analyze the strategy distributions:
	       (clrhash *strat-key->correct+incorrect*)
	       ;; Init pairs for (correct . incorrect) counts...
	       (loop for s in *strat-keys* do (setf (gethash s *strat-key->correct+incorrect*) (cons 0 0)))
	       ;; ...and count 'em up!
	       (loop for (s a b r) in log
		     do 
		     (if (listp s) (setf s (second s))) ;; recode (:CF1X1 :DYNARET) -> :DYNARET
		     (let ((c/ic-pair (gethash s *strat-key->correct+incorrect*))
			   (real-r (+ a b)))
		       (if (= r real-r)
			   (incf (car c/ic-pair))
			 (incf (cdr c/ic-pair)))))
	       ;; Special computation for :allret (+ :ret :dynaret)
	       (let ((r (gethash :ret *strat-key->correct+incorrect*))
		     (d (gethash :dynaret *strat-key->correct+incorrect*))
		     (a (gethash :allret *strat-key->correct+incorrect*)))
		 (setf (car a) (+ (car r) (car d)))		 
		 (setf (cdr a) (+ (cdr r) (cdr d))))
	       ;; Reporting
	       (loop for s in *strat-keys*
		     as pair = (gethash s *strat-key->correct+incorrect*)
		     as nc = (car pair)
		     as nw = (cdr pair)
		     as ns = (+ nc nw)
		     do (format o "	~a	~a	~a	~a" 
				ns 
				(if (zerop nlog) "x" (/ (float ns) nlog))
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

(defun analyze-summarize-coefs (ts &aux file->coefs)
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
	 (format o "~a" pn)
	 (loop for file being the hash-keys of *file->summary*
	       as params = (second (assoc :params (gethash file *file->data*)))
	       as pv = (cdr (assoc ps params :test #'string-equal))
	       ;; These are repeated once for each dataset bcs there'll be that many coefs
	       do 
	       (pushnew pv (gethash pn *params->all-values*) :test #'string-equal)
	       (loop for (nil) in *comparator-datasets*
		     do (format o "	~a" pv)))
	 (format o "~%"))

   ;; Report coefs -- WWW THIS ALL DEPENDS UPON HASH TABLES SCANNNING DETERMINISTICALLY !!!

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
		 as params = (second (assoc :params (gethash file *file->data*)))
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
		       (format o ",~a" (cdr (assoc ps p* :test #'string-equal))))
		 (loop for c in coef do (format o ",~a" c))
		 (format o "~%")))
     ))
  )

(untrace)
(trace parse-params most-recent-set-of-results-pathnames-by-label-mathcing) 
(analyze)
