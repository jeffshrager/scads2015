;;; !!! Notice !!! Warnings !!!

(defvar *dat* '((proposed . 0)
		(retrievals . 0)
		(sincelast . 0)
		(problems . 0)
		(right . 0)
		(wrong . 0)
		(saved . 0)
		))

(defun setdat (item value)
  (rplacd (assoc item *dat*) value))

(defun getdat (item)
  (cdr (Assoc item *dat*)))

(defun resdat () 
  (dolist (entry *dat*)
      (rplacd entry 0)))

(defun incdat (item &optional (n 1))
   (incf (cdr (assoc item *dat*)) n))

(defun repdat (&key (reset t))
  (format t "~%>>> ")
  (format t "~a done (~a ret., ~a% correct), ~a proposed, ~a mean ms saved~%"
	  (getdat 'problems)
	  (getdat 'retrievals)
	  (p2 (/ (getdat 'right) (getdat 'problems)))
	  (getdat 'proposed)
	  (p2 (/ (getdat 'saved) (getdat 'problems)))
	  )
  (when reset (resdat)))

(require "/users/shrager/lib/gcl")
(require "/users/shrager/lib/utils")
(require "/users/shrager/lib/stats")

;;; BUGS:
;;;
;;; need to make the shortcutter work the real way.
;;; some parts of reporting havn't been checked for order (see ???)
;;; 
;;; Notes:
;;;
;;; Generalize CLA information.

;;; ----- Run Notes -----
;;;
;;; (test :nruns 1000 :ntimeseach 5)
;;; (dosum "records.*" &key (combine-trials 10) (prefix "x")
;;;                         (stop-at nil) (start-at nil) 
;;;			    (fullreport nil)
;;;                         (correport nil)
;;;			    (postloadfn #'xlcla)
;;; (dosum "4.records.*" :combine-trials 25)
;;; (dosum "records.*" :start-at 200 :stop-at 500)
;;; (cortest)
;;; (collect-strats "records.*") 
;;;                  automatically adds "results/"

(defvar *MODEL-PARAMS*)
(setq *model-params*
  '(
 (*all-problems-are-the-same* nil)
 (*pdisc* 1)
 (*pmix* random)
; (*pmix* cam)
 (*cam-env* ((0 50) (100 50) (0 100) (0 9999)))
 (*perr* 4)

 (*new-strat-confidence-bump* 0.99)
 (*default-confidence* 0.2)
 (*novelty-confidence-incr* 0.99)

 (*initial-xpr* 0.1)
 (*xincr* 0.2)
 (*xinit* 0.02)

 (*new-default-confidence* 0.99)
 (*conf-incf-on-correct* 0.02)
 (*conf-incf-on-wrong* 0.01)

 (*default-general-strength* 1.0)
 (*new-strat-general-strength-bump* 1.0)
 (*general-strength-right-incr* 0.0)
 (*general-strength-wrong-incr* 0.0)

 (*memtbl-incr-right* 0.06)
 (*memtbl-incr-wrong* 0.03)

 (*memory-stack-limit* 200)

 (*use-internal-retrieval?* nil)
 (*retrieval-low-cc* 0.1)
 (*retrieval-high-cc* 0.9)
 ))

;;; Tons of global specs that aren't elsewhere defined.  (Note that I
;;; always use setq's instead of defvars where the defn are so that
;;; reloading actually changes the values of things!)

(defvar *ALLSTRATS*) ; used to remember suggested strategies for counting purposes.
(defvar *min-used*) ; indicates when min has been used (for challenge formation)
(defvar *MEMSTACK*)
(defvar *RECORD*)
(defvar *MEMTBL*)
(defvar *AD1*)
(defvar *AD2*)
(defvar *EB*)
(defvar *SOLUTION-COMPLETED*)
(defvar *CLA-HISTORY*)
(defvar *GOOD*)
(defvar *COUNT-FROM-ONE-TWICE*)
(defvar *CAM-COUNTER*)
(defvar *BC*)
(defvar *TC*)
(defvar *MGOAL*)
(defvar *MGOALS*)
(defvar *GLOBAL-MGOALS*)
(defvar *SAVED-MS-CYCLES*)
(defvar *NCYCLES*)
(defvar *CAM-ENV*)
(defvar *CAM-LEVEL*)
(defvar *FOA*)
(defvar *ADDEND*)
(defvar *SKELETAL-PLAN*)
(defvar *TESTS*)
(defvar *STRAT-TABLE*)
(defvar *CS*)
(defvar *GL*)

#| MAKE THIS SO!

Here's a description of how the discovery heuristics work.  Note: This
is an "in principle" description.  How the work *in fact* is a lot
messier than this description makes it sound.  One of my priorities
before the code can be released is to clean this up!

When a strategy is run, a short-term memory trace is maintained of all
the operators applied and each "external" action (such as saying a
number aloud).  Discovery works through two heuristics:

1. Remove externally redundant sequences of operations.  That is, when
there are operations that produce EXTERNAL REDUNDENCIES, such as
counting the same hand or value twice, the INTERNAL operations which
produced either the first or the second of these is proposed for
deletion.  This heuristic does most of the work of discovery, turning
Counting Fingers into SCSum+Recount and this into SCSUm.  This is the
messy heuristics; the way that redundency is found and attributed back
to its origins is very complicated.

2. Replace CHOOSE-ADDEND with CHOOSE-LARGER-ADDEND whenever the
statistics on a strategy suggest that it does better when the larger
addend is chosen first.  (This information is gleaned from the Short
Term trace as well, and stored in the ASCM information table with the
other info on the strategy.)

'Jeff

|#

;;; ----- Bugs, etc. -----
;;;
;;; Doesn't use true shortcutting.
;;; Doesn't properly use efficiency or error info to do choose-larger-addend.
;;;
;;; ----------------------

;;; Once an addend has been used in result generation, don't count it
;;; again.

;;; Entirely new addition model whose goals are imitation and medated
;;; training, rather than pure strategy discovery.  Of course, these
;;; functions are needed for s.d. as well since they enable the metac.
;;; system to do its thing.

;;; Generalized dist. of associations model, uses a giant array of
;;; acts each with a probability of moving to the next one.  Since
;;; this is NOT a sparse array, I actually represent the entire thing,
;;; but it also has to be dynamic, so we begin by producing a really
;;; big table and an associated ops list, and fill the ops list with
;;; nil (and the table with zeros) to begin with.  Then, as ops are
;;; added or discovered, they get added to the ops list, and so to the
;;; table.  Notice that this is a flat representation; there's no
;;; chaining. The only way that we're going to be able to distinguish
;;; one op pathway from another is to eventually build contextualized
;;; ops.  I'm not entirely sure at the moment how these will be
;;; represented, something like additional parts of the op name, or
;;; something.  The trick is to get, at the same time, generalized
;;; learning, reuse, and contextualization.  It's unclear at the
;;; moment that this can be easily accomplished through *any* means!
;;; However, I'm pretty firmly convinced that a static base op table
;;; is what's wanted, although what sits above it is.... well, stay
;;; tuned.

;;; Model parameters:

(defvar *general-strength-right-incr*)
(defvar *general-strength-wrong-incr*)
(defvar *default-general-strength*) ; when the initial strats are created
(defvar *new-strat-general-strength-bump*) ; added to the mean general
                                           ; strengths when a new strat is added.
(defvar *all-problems-are-the-same*) ; If t then all problems are lumped into
 ; the same charaterization ('addition-problem), otherwise (not t) then
 ; the detailed characterization of problem hold.
(defvar *pdisc*) ; probably that discovery will actually take place when it
                 ; is called as a meta-goal.  Usually set small so that 
                 ; all the relevant discovery doesn't happen in the first
                 ; ten trials! 

(defvar *perr*) ; probability of making counting errors (out of 100)

(defvar *pmix*) ; a symbolic param that specifies the problem mix.  See
                ; produce-a-problem for more info.

(defvar *default-confidence*) ; Confidence of initial.
(defvar *new-strat-confidence-bump*) ; Confidence of new strat OVER competitors.
(defvar *novelty-confidence-incr*) ; kick given to newly discovered strats.

(defvar *initial-xpr*) ; ???? SEE LOAD-STEP ???
(defvar *xincr*) ; How much to increment chain entries on use...
(defvar *xinit*) ; ...and where they begin.

(defvar *new-default-confidence*) ; for a new problem's characteristic
(defvar *conf-incf-on-correct*) ; add this when the strategy works.
(defvar *conf-incf-on-wrong*) ; and add this when it doesn't.

(defvar *memtbl-incr-wrong*) ; add this when you get a problem wrong
(defvar *memtbl-incr-right*) ; add this when you get one right

(defvar *memory-stack-limit*) ; ...and how far back it retains traces

;;; Retrieval cc ranges are used in select-strategy to determine when
;;; to actually choose retrieval (via setting the cc randomly).

(defvar *retrieval-low-cc*)
(defvar *retrieval-high-cc*)

;;; Tables and other control and operation globals; mostly reset at
;;; reload or new runs.

(defvar *stb*) ; The table of strategies.
(defvar *strat*) ; the currently selected strategy
(setq *tc* ()) ; the TOP chain, just an op list
(defvar *chain*) ; this is the actual subcognitive associative store
(defvar *xthresh*) ; set at run time tells us how high transitions are accepted
(setq *memtbl* ()) ; the siegler & shrager table of associations
(setq *mgoal* ()) ; The mgoal that is currently active.
(setq *mgoals* ()) ; The possible mgoals for a given run.
(setq *global-mgoals* ()) 
(setq *memstack* ()) ; The short term memory stack.

(defvar *cla*) ; tells when we've used choose-larger-addend
(setq *cla-history* nil) ; records the efficacy of choosing the larger addend

;;; Operational globals.

(defvar *tl* 0) ; trace level -- 0 means off
(defvar *runsum*) ; records multiple runs for per-strat stats
(defvar *overall-correct*) ; like runsum but not per-strat
(defvar *records*) ; This gets loaded with the records of the run.

;;; The oplist gets initialized with an alist containing the name of an op
;;; and its given position in the optbl.

(defvar *op-table-size* 100)
(defvar *oplist*)
(defvar *optbl*)
(defvar *next-available-op*)

(defun initialize-ops ()
  (format t "Op list and table being reinitialized!~%")
  (setq *optbl* (make-array (list *op-table-size* *op-table-size*)
			    :initial-element 0.0))
  (setq *oplist* 
    (do ((r () (cons (cons () k) r))
	 (k 0 (1+ k)))
	((= k *op-table-size*) (reverse r))))
  (setq *next-available-op* 0)
  )

;;; Functions to create and manpulate entries in the tables.

(defun new-op (name)
  (format t "Creating op ~a~%" name)
  (let* ((loc *next-available-op*)
	 (opp (nth loc *oplist*))
	 )
    (incf *next-available-op*)
    (rplaca opp name)))

(defun find-opn (op &optional create-new-if-missing)
  (let ((v (cdr (assoc op *oplist*))))
    (if v v
        (if create-new-if-missing
	    (progn (new-op op)
		   (cdr (assoc op *oplist*)))
	  ()
	  ))))

;;; Reporting functions for the optbl, which is too long to look at
;;; all the time.

(defun report-ops ()
  (dolist (o1 *oplist*)
    (if (car o1)
	(progn
	  (format t "~a -> ~%" (car o1))
	  (dolist (o2 *oplist*)
	    (let ((v (aref *optbl* (cdr o1) (cdr o2))))
	      (if (not (zerop v))
		  (format t "  ~a @ ~a~%" (car o2) v))
	      ))))))

;;; Here's the heart of the matter, updating op connectivity strenghs and 
;;; getting next ops from current ones.

(defun incf-op-to-op-strength (opa opb increment &optional create-new-if-missing)
  (let ((ap (find-opn opa create-new-if-missing))
	(bp (find-opn opb create-new-if-missing)))
    (if (and ap bp)
	(incf (aref *optbl* ap bp) increment)
      (break "An invalid op was sent to incf-op-to-op-strength."))))

;;; At the moment selecting the next op is just done with a simple max
;;; computation.

(defun get-best-next-op (curop &aux (maxval 0.0) maxop)
  (let ((curoploc (find-opn curop)))
    (dotimes (i *op-table-size*)
      (let ((xval (aref *optbl* curoploc i)))
        (if (> xval maxval)
            (progn (setq maxval xval)
		   (setq maxop (car (nth i *oplist*)))
		   )
	  )))
    maxop))

;;; Now we can run through a strategy and load up the op table.
;;; Oplist, as given here is just one of the strategies, presented at
;;; the end of this file.  We just run through each op and do it, and
;;; record the sequence in the op table.  (This is a special version
;;; of driver, as below, for the new model.)

(setq *ncycles* 0) ; total number of steps taken.
(setq *solution-completed* ()) ; this tells the driver to stop.

;;; This does the work of putting an image of the world into the
;;; memory stack.  It is given an action symbol to tag this stack
;;; entry and a list of state information. Also, old stack stuff is
;;; removed.  Note that the stack, which is just an alist of
;;; (action-symbols . state) is BACKWARDS: most recent first!  The
;;; theory is that whenever you take an action in the world you notice
;;; what you did and what the state of the things that you attended to
;;; were.

(defun mempush (action state)
  (push (cons action state) *memstack*)
  (do ((s *memstack* (cdr s))
       (i (1- *memory-stack-limit*) (1- i)))
      ((zerop i) (if s (rplacd s ())))))

(defun memclear ()
  (setq *memstack* ()))

(defun clear-records ()
  (setq *records* ())
  (clear-record))

;;; Each run record it a list of pairs made by record!  Note that the 
;;; *records* are in REVERSE ORDER OF THE RUN!  Some judicious reverses
;;; are called for!

(setq *record* ()) ; holds one run record

(defun clear-record ()
  (setq *record* ()))

(defun record! (id info)
  (push (list id info) *record*))

(defun save-record ()
  (push (reverse *record*) *records*)
  (clear-record))


#| 960514 - Added direct recall memory.  This is stored in an
   associative array, just like in Siegler & Shrager, and is updated
   at problem conclusion time, just like Siegler and Shrager.  And,
   just like... it is consulted ... um, okay, so this is different;
   it's consulted first, and then only when the cognitive system has
   some time with nothing to do.  Note that the array is actually one
   wider in each direction than the possible solutions; the zero index
   isn't used. |#

(defun init-memtbl (&aux i1)
  (setq *memtbl* (make-array '(6 6 11) :initial-element 0.01))

  ;; Set in place all the 1+ problems with small positive associations.
  (dotimes (i 5)
    (setq i1 (1+ i))
    (incf (aref *memtbl* i1 1 (1+ i1)) 0.05)
    (incf (aref *memtbl* 1 i1 (1+ i1)) 0.05))

  ;; And all the run-on problems (like 1+2=3)
  (dotimes (i 4)
    (setq i1 (1+ i))
    (incf (aref *memtbl* i1 (1+ i1) (+ 2 i1)) 0.05))

  )

;;; When the problem is completed, update the memory table
;;; appropriately depending upon whether we got it right or wrong.
;;; Ignore the results of challenge problems. 

(defun update-memtbl (a1 a2 result)
  (if (or (> result 9) (> a1 5) (> a2 5))
      (trp 1 "Addends (~a+~a) or result (~a) is/are larger than the memtbl limits -- Ignored!~%" a1 a2 result)
    (incf (aref *memtbl* a1 a2 result)
	  (if (= result (+ a1 a2))
	      *memtbl-incr-right*
	    *memtbl-incr-wrong*)))
  )

(defun show-memtbl ()
  (dotimes (i 5)
    (dotimes (j 5)
      (format t "~a + ~a = " (1+ i) (1+ j))
      (dotimes (k 10)
        (format t "~a (~a), " (1+ k) (p2 (aref *memtbl* (1+ i) (1+ j) (1+ k))))
	)
      (format t "~%")
      )
    (format t "~%")
    ))

#| Old guessing logic.

;;; Okay, now we need to be able to get values back; Feed a confidence
;;; critereon we return a guess that comes above the confidence
;;; critereon, or nothing if nothing comes above it.  First a guess is
;;; chosen at random from among the possible solutions in accord with
;;; their strenths.  But the guess is only stated if it is both chosen
;;; AND comes above the cc.  We need to check whether the problem
;;; addends are within the table limits because it might have been a
;;; challenge problem in which case there's never any guess.

(defun guess (a1 a2 cc)
  (if (or (> a1 5) (> a2 5)) nil
    (let ((guess (guess2 a1 a2)))
      (if (> (aref *memtbl* a1 a2 guess) cc) guess))
    )
  )

(defun guess2 (a1 a2 &aux (sum 0.0) result (cume 0.0))
  (dotimes (i 10)
    (incf sum (aref *memtbl* a1 a2 (1+ i))))
  (let ((thresh (/ (random 100) 100.0)))
    (dotimes (i 10 result)
      (if (> (incf cume (/ (aref *memtbl* a1 a2 (1+ i)) sum)) thresh)
	  (progn (setq result (1+ i)) ; why is this done in this weird way??
		 (return result)))
      )))

|#

;;; Pick at random from among the results that come above the cc, or
;;; return nil if nothing comes over the cc.

(defun guess (a1 a2)
  (if (or (> a1 5) (> a2 5)) nil
    (let ((options (results-above-cc a1 a2)))
      (if options 
	  (car (n-random 1 options))
	nil)
      ))
  )

(defun results-above-cc (a1 a2 &aux r)
 (let ((cc (+ *retrieval-low-cc* 
	      (random (- *retrieval-high-cc* *retrieval-low-cc*)))))
  (trp 1 "Results-above-cc choose cc=~a~%" cc)
  (dotimes (i 10 r)
    (if (>= (aref *memtbl* a1 a2 (1+ i)) cc)
	(push (1+ i) r)))
  ))

;;; Try-retrieval is the metagoal call for this.  At the moment is
;;; just gets a random cc and uses the global *ad1* and *ad2*. (This
;;; lets us easily call it inline as if it were a cognitive procedure.
;;; When called as a meta-goal this has to force the confidence
;;; updating code to give preference to retrieval instead of to the
;;; strategy that was initially called.

(defun try-retrieval (&aux cc)
  (let ((g (guess *ad1* *ad2*)))
    (trp 1 "********** Trying retrieval w cc=~a and got ~a.~%"
	    cc g)
    (if g 
        (progn 
	  (setq *eb* g)
	  (record! 'Used-retrieval! *eb*)
	  (incdat 'retrieval)
	  (setq *solution-completed* t) ; tell the driver we succeded!
	  ))
    ))

;;; Try-retrieval-op is the strategy operation version.  Here the cc
;;; is set low so that it always takes a guess regardless.  Early on,
;;; these will be poor, and retrieval will start to loose ground, but
;;; later, they'll be better since retrieval can be used as a metagoal
;;; as well.

(defun try-retrieval-op (&aux cc)
  (let ((g (guess *ad1* *ad2*)))
    (trp 1 "********** Trying retrieval w cc=~a and got ~a.~%"
	    cc g)
    (if g
        (progn 
	  (setq *eb* g)
	  (record! 'Used-retrieval! *eb*)
	  (setq *solution-completed* t) ; tell the driver we succeded!
	  ))
    ))

;;; An mgoal is bascially a production rule with a pattern and a right
;;; hand side which is just a list of function names in the meta
;;; system.  They're all named as mg.<name> for no good reason.

(defstruct (mgoal)
  name
  pattern
  procedures)

;;; Example mgoals; just for the general happiness of future programmers
;;; who, god forbid, might have to write these!
#|
(defun a-global-mgoal-fn1 () 
  (format t "Firing a-global-mgoal-fn1~%"))

(defun a-global-mgoal-fn2 () 
  (format t "Firing a-global-mgoal-fn2~%"))

(setq mg.a-global-mgoal 
  (make-mgoal :name 'a-global-mgoal
              :pattern #'(lambda () (y-or-n-p "Trying to fire a-global-mgoal? "))
              :procedures (list #'a-global-mgoal-fn1
				#'a-global-mgoal-fn2)))

(setq *global-mgoals* (list mg.a-global-mgoal))

(defun a-local-mgoal-fn1 () 
  (format t "Firing a-local-mgoal-fn1~%"))

(defun a-local-mgoal-fn2 () 
  (format t "Firing a-local-mgoal-fn2~%"))

(setq mg.a-local-mgoal 
  (make-mgoal :name 'a-local-mgoal
              :pattern #'(lambda () (y-or-n-p "Trying to fire a-local-mgoal? "))
              :procedures (list #'a-local-mgoal-fn1
				#'a-local-mgoal-fn2)))
|#

;;; Real mgoals.

;;; Global mgoals.

#|

;;; Mgoal to notice in the trace a sequence as: n, 1, 2, 3, ... n, that is,
;;; where the n is repeated.  Notice that in the memstack this sequence
;;; will be reversed, since things are pushed onto it.

(defun look-for-shortcuts ()
  (trp 2 "********* Looking for a shortcut sequence!~%")
  (let ((lcs (find-longcuts *memstack*)))
    (if lcs
	(propose-shortcut lcs (strat-chain *strat*))
      ))
  )

(defun clean-up-memstack-for-shortcutting ()
  (let* ((m1 (mapcan #'(lambda (i) 
			 (cond ((eq (car i) 'exec-op)
				(list (caadr i)))
			       ((eq (car i) 'say)
				(list i))
			       ))
		     (reverse *memstack*)))
	 )
    m1))

(defun find-longcuts (memstack)
  (cond ((null memstack) ())
	((and (eq 'say (caar memstack))
	      (is-a-longcut (car memstack) (car memstack) (cdr memstack)))
	 (cons (parse-out-longcut (car memstack) (cdr memstack) 
				  (list (car memstack)))
	       (find-longcuts (cdr memstack))))
	(t (find-longcuts (cdr memstack)))))

(defun is-a-longcut (target carry list &aux n)
  ;; Make (say #) -> #, in all of target, carry and list top (n).
  (let ((temp (car list)))
    (if (eq 'say (car temp)) (setq n (cadr temp))))
  (if (not (numberp carry)) (setq carry (cadr carry)))
  (if (not (numberp target)) (setq target (cadr target)))
  (cond ((null list) ())
	;; If we reach the target number, we're done.
        ((and n (= target n)) t)
	;; If it's the next down, loop.
	((and n (= (1- carry) n))
	 (is-a-longcut target (1- carry) (cdr list)))
	;; If it's a number, but not one of the above, fail!
	(n nil)
	;; Otherwise, anything else is ignored.
	(t (is-a-longcut target (1- carry) (cdr list)))
	))

(defun parse-out-longcut (target list collect &aux n)
  (let ((temp (car list)))
    (if (eq 'say (car temp)) (setq n (cadr temp))))
  (if (not (numberp target)) (setq target (cadr target)))
  (cond ((and n (= target n)) (push (car list) collect) collect)
	(t (parse-out-longcut target (cdr list) (push (car list) collect)))
	))

(defun propose-shortcut (lcs seq &aux newseq)
  (let* ((seq-to-rem (type-chain lcs))
	 (seq-chain (type-chain seq))
	 (newstrat (remove-subseq seq-to-rem seq-chain (random 2)))
	 )
    (trp 2 "~%~%Shortcutter removed~%   ~a ~%from~%   ~a ~%to get~%   ~a.~%~%"
	    seq-to-rem seq-chain newstrat)
    (if (good-strat? newstrat)
	(if (not (is-in-stb? newstrat))
	    (install-strat newstrat (gentemp "S"))
	  ))
    ))

|#

;;; Quick and dirty version of the discovery methods.

(defun look-for-shortcuts ()
 (let ((st (type-chain (strat-chain *strat*))))
  (dosc st (modstrat st))))

;;; Decide whether to install a suggested strategy, and update the 
;;; strategy counting records.

(defun dosc (oldstrat newstrat)
  (trp 2 "~%~%Shortcutting ~%~%~A~%~% to ~%~%~a~%~%" oldstrat newstrat)
  ;; Include this in the list of all suggested strategies, if isn't
  ;; not there already.  Note that this includes both good and bad
  ;; strats, as it's PRE FILTERING!  This is used in summarization to
  ;; count the number of strats that were proposed.
  (pushnew newstrat *allstrats* :test #'equal)
  ;; Now filter it and install it if it passes!
  (if (good-strat? newstrat)
      (if (not (is-in-stb? newstrat))
	  (install-strat newstrat (gentemp "S"))
	))
  )

;;; Optimization just changes the FIRST choose-addend to
;;; choose-large-addend.  And the q/d version of shortcutting 
;;; just knows what to cut!

(defun modstrat (p &aux (r (random 6)))
  (case r
     ;; The pair surrounding clear-eb are deleted to remove redundancy.

     (0 (setq p (remove-subseq '(say-addend clear-eb) p 1)))
     (1 (setq p (remove-subseq '(clear-eb raise) p 0)))

     ;; This chunk is deleted to remove additional redundancy.  It is
     ;; important to recognize that the actual shortcutter would NOT
     ;; do this unless there was already a sum appearing in the
     ;; memstack, so that has to be checked.  We do this by confirming
     ;; that the SAY at the tail of the memstack appears in the
     ;; body of the memstack as well.

     (2 (if (member (car *memstack*) (cdr *memstack*) :test #'equal)
	    (setq p (remove-subseq
		     '(choose-hand clear-eb count-fingers 
				   swap-hands count-fingers) p 0))
	  )
	)

     ;; This makes the choose-larger-addend version of strats.  It
     ;; ought to test for the correctness of the SECOND-LARGER form of
     ;; the procedure before being applied.

;;; !!! Always works at the moment !!! (See !!! in s-l-i-s-b)

     (3 (if (and *cla-history* (second-larger-is-significantly-better))
	    (let* ((np (copy-list p))
		   (where (member 'choose-addend np)))
	      (if where
		  (progn (rplaca where 'choose-larger-addend)
			 (setq p np)))
	      )
	  )
	)

     ;; This is an analog to the first pair, although it's in the
     ;; wrong position.  It's here because we can't support cheating
     ;; the shortcutter, assuming that it knows WHICH of these (from
     ;; these or the above set) to remove.

     (4 (setq p (remove-subseq '(say-addend clear-eb) p 0)))
     (5 (setq p (remove-subseq '(clear-eb raise) p 1)))
     )
  p)

(defun is-in-stb? (s)
  (if (member s *stb*
	  :test #'(lambda (ns stbentry)
		    (equal ns (type-chain (strat-chain stbentry)))))
      (progn (trp 2 "~%Already in the strat table!~%")
	     t)))

(defun install-strat (s name)
  (push (cons name (setq s (deloop-uniqueify s))) *good*)
  (repdat)
  (trp 0 "~%~%>>> Installing ~a (~a): ~%~%" name (name-that-strat s))
  (push (make-strat :name name
                    :chain s
                    :mgoals ()
		    :default-confidence *default-confidence*
		    :confidences (create-confidences-for-new-strat *strat*)
		    :general-strength (create-new-general-strength)
		    )
	*stb*)
  ;; Update the table of found strats for various functions.
  )

;;; The general strength of a new strat is the mean of the general
;;; strengths of all the other strats, plus a little bump.

(defun create-new-general-strength ()
  (+ *new-strat-general-strength-bump*
     (mean (mapcar #'strat-general-strength *stb*)))) 

;;; The confidence for a new strat uses the confidence bump to ensure
;;; that for this sort of problem, this strat is used more than the
;;; one it was created from.  There are other possible methods that
;;; might work better.

(defun create-confidences-for-new-strat (parent-strat)
  (let* ((chars (characterize-problem *ad1* *ad2*))
	 (old-conf (find-confidence parent-strat chars))
	 )
    (list
     (make-confidence :features chars
		      :value (+ (if old-conf 
				    (confidence-value old-conf)
				  *new-default-confidence*)
				*new-strat-confidence-bump*)
		      ))
    ))

(defun find-all-mem-entries (key list)
  (loop for item in list if (eq key (car item)) collect item))

(setq mg.look-for-shortcuts
  (make-mgoal :name 'look-for-shortcuts
              :pattern #'(lambda () (< (random 100) *pdisc*)) ; always possible
              :procedures (list #'look-for-shortcuts)))

(push mg.look-for-shortcuts *global-mgoals*)

;;; Global mgoal to try retrieval.

(setq mg.try-retrieval
  (make-mgoal :name 'try-retrieval
              :pattern #'(lambda () *use-internal-retrieval?*) 
              :procedures (list #'try-retrieval)))

;;; Set up the gobal mgoals, which have been defined above, we hope!

(push mg.try-retrieval *global-mgoals*)

;;; 

(defstruct (strat)
  name
  mgoals
  chain
  confidences
  default-confidence
  general-strength
  )

;;; Initialize the strategy database.

(defvar *retrieval*) ; for update intervention

(defun init-strats ()
  (setq *stb* ())
  (push (make-strat :name 'cf
                    :chain (deloop-uniqueify *count-from-one-twice*)
                    :mgoals ()
		    :default-confidence *default-confidence*
		    :general-strength *default-general-strength*
		    )
	*stb*)
  (push (setq *retrieval* ; for update intervention
	      (make-strat :name 'retrieval
			  :chain (deloop-uniqueify '(try-retrieval-op))
			  :mgoals ()
			  :default-confidence *default-confidence*
			  :general-strength *default-general-strength*
			  )
	      )
	*stb*)
  )

;;; Select a strategy for the present problem, given the addends.
;;; This makes its decison based upon the confidence that the system
;;; has for different sorts of problems of each strategy type,
;;; combined with the general strength values for each strategy.
;;; There is special analysis of the vaibility of retrieval here so
;;; that retrieval gets priority if the associative strength of any
;;; possible result comes above *retrieval-strat-cc*.  The convolution
;;; with general strength is a little bit ad hoc (not that everything
;;; in this model isn't ad hoc!)

(defun select-strategy (a1 a2 &aux strat-conf-table)
 ;; First see if retrieval will give us a valid guess.  If so, then
 ;; we're done; go ahead and select retrieval.  ???I'm confused about ASCM's
 ;; calculations when ret. is selected.  Apparently if ret doesn't give
 ;; you a strong guess when you actually run it, then you come back and 
 ;; reselect, but then how did ret. get selected to begin with???
 (if (guess a1 a2)
     *retrieval*
   (progn
     ;; Now allow selection of everything EXCEPT retrieval.
     ;; Build the table of (confidence-values . strats)
     (dolist (strat (remove *retrieval* *stb*))
	     (push (cons (compute-strategy-confidence a1 a2 strat) strat)
		   strat-conf-table)
	     )
     ;; Select the strat that has the highest conf. value.  If none is 
     ;; significantly greater than the others, we just choose at random.
     ;; Note that it's possible that there is NO confidence value for a 
     ;; given strat. In this case, it will be entered into the table with
     ;; nil, and is assigned the mean.
     (let (confidences)
       (dolist (c strat-conf-table)
	 (if (car c)
	     (push (car c) confidences)))
       ;; If there aren't any, then they all get set to 0.0 (doesn't matter what)
       (if confidences 
	   (setq confidences (mean confidences))
	 (setq confidences 0.0)) 
       (dolist (c strat-conf-table)
	 (if (null (car c))
	     (setf (car c) confidences)))
       )
     ;; Now find all the top ones and select one at random from among
     ;; them.
     (let* ((maxconf (caar (sort (copy-list strat-conf-table)
				 #'(lambda (a b) (> (car a) (car b))))))
	    (r nil)
	    (strats-with-maxconf
	     (dolist (s strat-conf-table r)
	       (if (= (car s) maxconf) (push s r))))
	    )
       (trp 1 "~a strategies are at maxconf = ~a~%" 
	    (length strats-with-maxconf) maxconf)
       (cdr (nth (random (length strats-with-maxconf))
		 strats-with-maxconf))
       )
     )) ; let if
 )

;;; If there's no confidence for this problem, use the default that
;;; would have been there if this was used.

(defun compute-strategy-confidence (a1 a2 strat)
  (let* ((probchars (characterize-problem a1 a2))
	 (confstructure (find-confidence strat probchars))
	 (confidence 
	  (if confstructure 
	      (confidence-value confstructure)
	    *new-default-confidence*))
	 )
    (* confidence (strat-general-strength strat))
    ))

#|
Code to use statistical confidence selection -- removed in favor of
just choosing at random from among the top values.

    ;; Okay, so each entry in the table ought to exist at this point
    ;; Next sort them best first.
    (setq strat-conf-table 
	  (sort strat-conf-table 
		#'(lambda (a b) (> (car a) (car b)))))
    ;; Now see if the first is significantly different than the rest.
    ;; If it isn't, then select at random, otherwise, choose the first.
    (if (and (< 1 (length strat-conf-table))
	     (sig-better? (caar strat-conf-table)
			  (mapcar #'car (cdr strat-conf-table)))
	     )
	(progn
	  (trp 1 "Selecting a strategy based upon confidence significance.~%")
	  (cdar strat-conf-table)
	  )
	(progn
	  (trp 1 "Selecting a strategy at random.~%")
	  (cdr (nth (random (length strat-conf-table))
		    strat-conf-table))
	  )
	) 

;;; Tells us if one value is significantly greater than a list of
;;; others.  Actually, this is a complete cheat.  Since this is very
;;; often called with multiple copies of the same value in the
;;; contrast list, we introduce a small amount of noise in order that
;;; there will be a small amount of variance.  This is carried out by
;;; randomly adding or substrating 0-3% of each value to itself.  It's
;;; just slightly possible that this will still yield equal values
;;; with no variance, so that it explicitly tested and we recur
;;; through this whole game if that's the case. This won't work if the
;;; values are all zero, so we also add 0.0001 to each before
;;; calculating the % fudge

(defun sig-better? (x l)
  (if (null (cdr l)) ; no variance for just one other possibility
      (not (= x (car l))) ; then the answer is just if they are equal
    (progn ; otherwise do the complicated analysis.
                                        ;; here's the fudge factor
      (maplist #'(lambda (vl) (let ((vd (* (/ (random 4) 100) 
					   (+ 0.0001 (car vl)))))
				(if (zerop (random 2))
				    (incf (car vl) vd)
				  (decf (car vl) vd)))
		   )
	       l)
      (if (apply #'= l)
	  (sig-better? x l) ; recuring will re-randomize the fudge factor.
	(eq '*/p<.05! (t1-test l x)))
      )
    ))

|#

;;; A confidence tells us what confidence criteria to apply to a
;;; strategy depending upon features of the problem.  The problem
;;; features are given by characterize-problem and go into the
;;; features slot as a list of atoms. 

(defstruct (confidence)
  features ; a list of atoms returned by characterize-problem
  value ; an integer
  )

;;; Characterize-problem returns a list of atoms that can be used
;;; to determine what kind of problem this is.

(defun characterize-problem (addend1 addend2 &aux r)
  (if (= addend1 addend2) 
      (push 'equal-addends r)
      (if (> addend1 addend2) (push 'first-larger r)
	(push 'second-larger r))
      )
  (if (or (> addend1 3) (> addend2 3))
      (push 'bignums r)
      (push 'nobignums r))
  (if *all-problems-are-the-same* '(addition-problem)
    r))

#| An abstract model of the mediation learner.  This merely runs two
   chains against one another, having one pick up portions of the task
   from the other.  The top chain as a sequence of random entries from
   the set of ops.  The bottom chain is more complicated, being a set
   of entries that include the operator and a transition probability
   to a next bottom chain item. |#

#| The *BC* (bottom chain) has a transition probability associated
   with each entry.  The *XTHRESH* must be reached in order to strike
   out on one's own initiative, and each time a trasntition is done,
   the *XINCR* gets added to this item. |#

;;; Structures for a bottom chain entries

(defstruct (bci)
  operator xlist)

(defstruct (xition)
  xprob xbci)

#| Main testing function.  Just runs through problem after problem. |#

#| sets up a top chain with an initial strategy, clears the bottom
   chain, and runs thru some number of trials. |#

(setq *saved-ms-cycles* 0) ; incf'ed whenever the SS does the work for us.

(defun inner-test (reset-everything &key (report t) (nruns 100) (pause nil)
				    record-save-stream)
  (setq *cla-history* ())
  (setq *min-used* ())
  (setq *cam-counter* 0)
 ; (if (> nruns 100) 
 ;     (progn
	;(format t "Reporting forced off!~%")
	;(setq report nil)))
  (if reset-everything
      (progn (setq *allstrats* () *good* ())
	     (clear-records)
	     (init-strats)
	     (init-memtbl)
	     (format t "~%>>> Strategies initialized: ~a~%." (mapcar #'strat-name *stb*))
	     (setq *bc* ()) ; reset for now (maybe later remove this)
	     ))

  ;; Main inner loop!
  (dotimes (run nruns)
    (produce-a-problem *pmix*)
    (if report 
	(format t "----- ~A ----- ~a+~a.~%" run *ad1* *ad2*)
      (when (zerop (mod run 50))
	  (format t "~a " run)
	))
    (when (> (getdat 'sincelast) 50)
	  (repdat :reset nil)
	  (setdat 'sincelast 0))
    (incdat 'sincelast)
    (incdat 'problems)
    (record! 'problem (list *ad1* *ad2*))

    ;; Select the strategy to use.

    (setq *strat* (select-strategy *ad1* *ad2*))
    (if report (format t "Selected to use ~a (~a).~%" 
		       (strat-name *strat*)
		       (name-that-strat (strat-chain *strat*))))
    (record! 'strat (strat-name *strat*))

    ;; If min was selected, indicate it for the challenge problem
    ;; generator.  Note that once this is set, it stays set for the
    ;; sim run!

    (if (equal "Min[Count_From_Either/CLA]" ; If it's a min record...
		   (name-that-strat *strat*))
	(setq *min-used* t))

    ;; Get the confidence or default for this strategy in these problem
    ;; conditions.  The transition threshold is then (1-confidence), so
    ;; that when you're very confident you use low xthresholds

    (setq *xthresh*
      (let ((c (find-confidence *strat* (characterize-problem *ad1* *ad2*))))
	(if c
	    (- 1 (confidence-value c))
	    (- 1 (strat-default-confidence *strat*)))))
    (trp 1 "Transition threshold is ~a~%" *xthresh*)
    (record! 'xthresh *xthresh*)

    (setq *tc* (strat-chain *strat*))

    ;; Setup the meta goal list for this run.  This is the append of the
    ;; global mgoals and the and local metagoals for this strat.
    (setq *mgoal* ()) ; clear from left over previous runs.
                           ;; pro. doesn't need to be copied
    (setq *mgoals* (append (copy-list (strat-mgoals *strat*))
			   (copy-list *global-mgoals*)))
    (setq *saved-ms-cycles* 0)
    (memclear)
    (setq *ncycles* 0) 
    (drive)
    (if *eb* 
        (progn (update-memtbl *ad1* *ad2* *eb*)
	       ;; If retrieval was used then credit the retrieval strat
	       ;; instead of the one originally chosen!
	       (if (assoc 'USED-RETRIEVAL! *record*)
		   (update-strat *retrieval* *ad1* *ad2* *eb* report)
		 (update-strat *strat* *ad1* *ad2* *eb* report))

	       ;; Update the CLA precision to represent when 'larger
	       ;; was the method.  Might be that this this ought to
	       ;; be a metagoal?
	       (push (list *ncycles* (= (+ *ad1* *ad2*) *eb*) *cla*)
		     *cla-history*)

	       ))
    (record! 'result *eb*)
    (record! 'ncycles *ncycles*)
    (trp 1 "Number of saved MS cycles = ~a.~%" *saved-ms-cycles*)
    (incdat 'saved *saved-ms-cycles*)
    (record! 'saved-ms-cycles *saved-ms-cycles*)
    (save-record) ; be sure to log this run's info!
    (if (and pause (not (y-or-n-p "Continue?"))) (return 'done))
    )
  (if record-save-stream
      (progn
	(save-records record-save-stream)
	(save-stb record-save-stream)
	)
    (with-open-file (f (form-record-same-filename)
		       :direction :output
		       :if-exists :supersede)
      (save-records f)
      (save-stb f)
      )
    )
  (summarize-records)
  (format t "~a strats were suggested (*allstrats*), ~a accepted (*stb*)~%"
	  (length *allstrats*) (length *stb*))
  'done
  )

;;; There are a number of problem-production methods.  

(defun produce-a-problem (mix)
  (case mix
    (random (setq *ad1* (1+ (random 5)))
	    (setq *ad2* (1+ (random 5)))
	    )
    ;; The challenge-after-min (CAM) envelope is based upon min usage
    ;; (NOTE: NOT MIN DISCOVERY!)
    (cam
     (if *min-used*
	 (cond ((zerop *cam-counter*)
		(let ((cam (pop *cam-env*)))
		  (setq *cam-level* (car cam))
		  (setq *cam-counter* (cadr cam))
		  (format t "~%New CAM level = ~a, for ~a problems~%" 
                            *cam-level* *cam-counter*)
		(produce-a-problem 'cam)))
	       ((> *cam-level* (random 100))
		(if (zerop (random 2)) ; can do it either way
		    (setq *ad1* 3 *ad2* 25)
		  (setq *ad1* 25 *ad2* 3)
		  )
		(decf *cam-counter*)
		)
	       (t (if (not (zerop *cam-counter*))
		      (decf *cam-counter*))
		  (produce-a-problem 'random))
	       )
       (produce-a-problem 'random)
       )
     )
    (t (break "Evil *pmix* value!"))
    ))

(defun form-record-save-filename ()
  (let ((fn (format nil "results/records.~a.~a" (date-as-int) (time-as-int))))
    (format t "*** Records being saved in ~a ***~%" fn)
    fn))

(defun save-records (s)
    (format s "~%~%;;; WARNING: THESE ARE IN REVERSE ORDER OF OCCURRANCE IN THE RUN~%")
    (format s "(setq *records* '(~%")
    (mapcar #'(lambda (r) (print r s)) *records*)
    (format s "))~%")
    )

(defun save-stb (s)
    (format s "~%~%(setq *stb* '(~%")
    (mapcar #'(lambda (r) (print r s)) *stb*)
    (format s "))~%")
    )

;;; Update the statistics for the strategy.  If this sort of problem
;;; hasn't been done before, then we have to make a new confidence
;;; struct for its characteristcs, else just change the confidence
;;; values for the strategy on this sort of problem.

(defun update-strat (strat a1 a2 answer &optional (report t) right?)
  (setq right? (= (+ a1 a2) answer))
  (if report (format t "** ~a + ~a = ~a ** ~%" a1 a2 answer))
  ;; Problem-specific updating.
  (let* ((chars (characterize-problem a1 a2))
	 (confidence (find-confidence strat chars)))
    (if confidence
	(update-confidence confidence right?)
        (let ((newconf (make-confidence :features chars
					:value *new-default-confidence*)))
	  (update-confidence newconf right?)
	  (push newconf (strat-confidences strat))
	  ))
    )
  ;; General updating.
  (incf (strat-general-strength strat)
	(if right?
	    *general-strength-right-incr*
	  *general-strength-wrong-incr*))
  )

(defun update-confidence (c right?)
  (if right?
      (progn (incf (confidence-value c) *conf-incf-on-correct*)
	     (incdat 'right)
	     (trp 2 "Yaaa....Got it right! (new confidence = ~a)~%"
		     (confidence-value c)))
      (progn (incf (confidence-value c) *conf-incf-on-wrong*)
	     (incdat 'wrong)
	     (trp 2 "Oops....Got it wrong! (new confidence = ~a)~%"
		     (confidence-value c)))
      ))

(defun find-confidence (strat features)
  (dolist (c (strat-confidences strat))
    (if (set-equal features (confidence-features c))
	(return c))))

;;; Display utils.

(defun display-chains ()
  (dctop)
  (dcbot))

(defun dctop ()
  (print *tc*)
  (format t "~%")
  )

(defun dcbot ()
  (dcbot2 *bc*)
  (format t "~%")
  )

(defun dcbot2 (el)
  (dolist (entry el)
     (format t "~s:~%" (bci-operator entry)) ; put out the op name
     (dolist (goto (bci-xlist entry))
       (format t "   - ~4,2f -> ~s~%" 
	       (xition-xprob goto) 
	       (bci-operator (xition-xbci goto)))
       )))

#| Driver for the task.  We always start off with the first bc entry
   and top entry.  If the transition probability is over xthresh then
   we take the bottom path, otherwise we take the top path and update
   the bottom path's xprob. |#

(defun drive (&optional (loop-kill-count 1000))
  (setq *solution-completed* ())
  (let* ((top *tc*)
	 )
    (prog (ok-xition current-op next-op)

      ;; Always start at the begnning of the TOP list.

      (setq current-op (pop top))

     loop

      ;; Check for recognition of completion.  (This might better be
      ;; done as an mgoal?)  Anyhow, it has to be better integrated into
      ;; the world. This is set by try-retrieval (if it wins) and by
      ;; end!, which is used at the end of each normal strategy

      (if *solution-completed* (return nil))

      ;; Watch out for internal loops!

      (if (zerop (decf loop-kill-count))
	  (progn (print "Loop Stopped!")
		 (record! 'Loop-stopped-by-force! nil)
		 (break)))

      (incf *ncycles*)

      ;; Normal end of a procedure.  (Usually it will call end! which will
      ;; set *solution-completed* but sometimes we just run off the end
      ;; of the stack.)

      (if (null current-op) 
	  (return nil))

      (exec-op current-op)

      (trp 2 "At ~a, " current-op)

      ;; Is there a bottom transition that's above threshold?

      (setq ok-xition (select-bottom-xsition current-op))

      (if ok-xition

	  ;; If so, use it!

	  (progn
	    (trp 2 "Selected -> ~a (~a)~%"
		    (bci-operator (xition-xbci ok-xition))
		    (xition-xprob ok-xition))

	    ;; Since there's a bottom xition selected, force it onto the
	    ;; operation stack.  This is actually wrong, but it'll do for now.

	    (setq next-op (bci-operator (xition-xbci ok-xition)))

	    ;; Okay, here we go with the metacog. system!
	    (do-a-meta-cycle)

	    )

	  ;; Otherwise don't bother and so the next thing on the TOP stack.

 	  (progn
	    (trp 2 "no transition selected.~%")
	    (if top
		(setq next-op (car top))
	        (setq next-op ()))
	    )
	  )

      (if top (pop top))
	      
      ;; Update xsition probabilities and go around.

      (if next-op
          (update-bottom-xprobs (list current-op next-op)))

      (setq current-op next-op)

;     (print (list current-op (list top)))

      (go loop)
      )
    ))

(defun select-bottom-xsition (current-op)
  (let* ((xition (find current-op *bc* :test
			  #'(lambda (a b) (eq (bci-operator b) a))))
	 (possible-paths (if xition (bci-xlist xition)))
	 )
    (if possible-paths
	(select-path-normalized possible-paths))
    ))

;;; This gets each paths' xprob and normalizes them to 1.0 and then
;;; randomly selects on in accord with their strength.  Only operators
;;; that come over *xthresh* are included in the list.

(defun select-path-normalized (paths &aux (sum 0.0) result)
  (let ((pl (mapcan #'(lambda (p) (if (> (xition-xprob p) *xthresh*)
				      (list (cons (xition-xprob p) p))))
		    paths)))
    (dolist (p pl)
      (rplaca p (incf sum (car p))))
    (let ((thresh (/ (random 100) 100.0)))
      (dolist (p pl result)
        (if (> (/ (car p) sum) thresh)
	    (progn (setq result (cdr p))
		   (return result)))
	))))

;;; --------------

(defun update-bottom-xprobs (tc-portion)
  (let ((from (car tc-portion))
	(to (cadr tc-portion)))
    (if (and from to)
	(ubx2 from to))))

(defun ubx2 (from to)
  (let ((from-bci (find from *bc* :test #'tbp1))
	(to-bci (find to *bc* :test #'tbp1)))
    (cond ((and from-bci to-bci)
	   (ubx3 from-bci to-bci))
	  (from-bci ; from- but no to-bci
	   (ubx3 from-bci (make-new-bci to)))
	  (to-bci ; to- but no from (unlikely but...)
	   (ubx3 (make-new-bci from) to-bci))
	  (t ; neither -- this is common when we start out!
	   (ubx3 (make-new-bci from) (make-new-bci to)))
	  )))

(defun ubx3 (from-bci to-bci)
  (let ((link (find to-bci (bci-xlist from-bci) :test #'findxlbci)))
    (if link ; if there's already a link...
	(incf (xition-xprob link) *XINCR*)
        (progn
	  (push (make-xition :xprob *XINIT* 
			     :xbci to-bci)
		(bci-xlist from-bci)))
	)))

(defun findxlbci (bci xition)
  (equal (bci-operator bci) (bci-operator (xition-xbci xition)))
  )

(defun make-new-bci (operator)
  (let ((new (make-bci :operator operator)))
    (push new *bc*)
    new))

(defun tbp1 (name bci)
  (equal name (bci-operator bci)))

;;; ----- Operators for actual addition strategies and test routines.

#| There are the following systems herein:

   1. The experimenter chooses problems, presents them, handles
      cycling the world, and gives feedback on results of addition.

   2. ASCM (more correctly, the LMT module) knows the specific high
      level structure of each strategy, and carries with each a memory
      strength value.  Each time the memory system is referenced (by
      the cognitive system), the referred-to elements get their
      strengths increased.  This principle gives us an account of the
      basic memory effects: (a) whenever a strategy is used (whether
      it works or not) its strenght is increased.  (b) when a strategy
      gets the right answer, it is increased again (by viture of the
      cognitivie system getting rewarded and so sort of patting the
      strategy on the head), and (c) when a new strategy is entered,
      it gets very high strength points (because there's a lot of
      memory interaction involved in entering it into the memory).

   3. The performance system knows how to do the specific things that
      come with goals (as stored in the memory to describe a
      strategy).

   4. The cognitive system gets the problem, computes its features,
      probes ASCM for a strategy, gets the goals for the selected
      strategy from the memory, sets the peripheral system off on the
      first of these and then monitors performance as the process
      takes place.  When the answer appears (in an echoic buffer at
      the end of the run) the cognitive system reports this to the
      experimenter, gets the right or wrong feedback, and then, if the
      answer was correct, pats ASCM on the head.  The cognitive system
      also permanently knows the general structure of a good addition
      strategy.  This is coded into the cognitive system and never
      changes.

     |#

#| General utilities for reporting, etc. |#

(defun trp (tl text &rest args)
  (if (>= *tl* tl)
    (apply #'format (append (list t text) args))
    ))

#| The peripheral system.  This is a very simple representation of the
   stuff needed for the addition domains: ten "fingers", a focus of
   attention, and an echoic memory into which numbers are stored.
   Operations on these constitute the basic operations of the
   domain. |#

#| The fingers memory structure; five on each of two hands,
   each of which my be up or down. |#

(defvar *left-hand*)
(defvar *right-hand*)

(defun clear-hands ()
  (setq *left-hand* (copy-list '(d d d d d)))
  (setq *right-hand* (copy-list '(d d d d d)))
  )

#| The focus of attention may point at a particular finger, or may be nil. |#

(setq *foa* nil)

#| The echoic buffer. |#

(setq *eb* nil)

#| Basic operations; most of which operate on what's in foa. |#

;;; Look at a hand, always at the first finger.

(defun focus-on-hand (hand)
   (setq *foa* hand)
   (mempush 'focus-on-hand (list hand))
   (report-hands))

(defun increment-focus ()
         ;; If done the right hand, return nil.
   (cond ((eq *foa* (last *right-hand*))
          ())
         ;; If we're done the left hand, move on to the rigt.
         ((eq *foa* (last *left-hand*))
          (setq *foa* *right-hand*))
         ;; Else shift to the next finger.
         (t (setq *foa* (cdr *foa*)))
         )
   (report-hands)
   )

;;; This is just a reporting function (and helpers).  The fingers are
;;; shown up (u) or down (d) for each hand, and the one begin attended to
;;; is capitalized.

(defun report-hands ()
  (trp 4 "~%")
  (rh *left-hand*)
  (trp 4 "|")
  (rh *right-hand*)
  (trp 4 "~%"))

(defun rh (h)
  (maplist #'rht h))

(defun rht (h)
  (if (eq *foa* h)
    (if (eq 'u (car h))
      (trp 4 "U")
      (trp 4 "D"))
    (if (eq 'u (car h))
      (trp 4 "u")
      (trp 4 "d"))
    ))

;;; Finger raising; always does the focussed finger.

(defun put-up ()
  (setf (car *foa*) 'u))

;;; Maipulation in the echoic buffer where number facts live.  We
;;; assume perfect knowledge of the number sequence.  That way incr
;;; and decr can be used.  This is where all possible errors come into
;;; the picture.  There is a probability (*perr*) of say-next
;;; reporting the WRONG number; this always takes place by simply
;;; failing to count.  Note that there are really a number of ways
;;; that one can get errors in addition, but the basic effect of
;;; correlating errors with the number of step in the procedure is
;;; accomplished by this method.

(defun say (n)
  (trp 2 " <~a> ~%" n)
  (setq *eb* n)
  (mempush 'say (list n)))
	     
(defun say-next ()
  (if (null *eb*)
      (say 1)
      (if (> *perr* (random 100))
        (say *eb*) ; err by double counting
        (say (1+ *eb*)) ; this is correct (increments)
	)
    ))

;;; Clear EB each time you're about to start a count off.  If you
;;; don't do this, the last number counted will be left in the echoic
;;; buffer and you'll count on from it, which is actually right, of
;;; course, for shortcut-sum.

(defun clear-eb ()
  (setq *eb* ()))

(defun end! ()
  (setq *solution-completed* t) ; this tells the driver to stop.
  )

;;; Raise is an important heart of this process.  The question is how
;;; to do the test-for-done.  That is, when putting up fingers, how
;;; does the child know when he's got the right number up?  In this
;;; version, he uses the echoic buffer trace, but that can't be right
;;; for shortcut sum because the echoic buffer contains the SUM, not
;;; just the single hand's count, so the right hand won't work.
;;; Somehow the child can SAY one thing while covertly counting
;;; another.  This suggests a dissociation of the echoic number
;;; sequence from counting, which can be done covertly.  Instead of
;;; relying upon the echoic trace for the count, We uses a new buffer
;;; (*cb*) to maintain the count of fingers up on a particular hand.
;;; This buffer is cleared by raise2 itself, and is used for the done
;;; test.

(defvar *cb*) ; counting buffer

(defun exec-op (op)
  (trp 2 "Doing: (~a)~%" op)
  (mempush 'exec-op (list op))
  (funcall (car op))) ; extract type only

;;; This version of raise assumes that hand control is done by the caller.

(defun raise ()
  (setq *cb* 0)
  (prog ()
 loop
    (say-next)
    (put-up)
    (increment-focus)
    (incf *cb*)
    (if (= *cb* *addend*) (return))
    (go loop)
    )
  )

(defun count-fingers ()
    (look-n-count)
    (look-n-count)
    (look-n-count)
    (look-n-count)
    (look-n-count)
    )

(defun look-n-count ()
  (if (eq 'u (car *foa*))
      (say-next))
  (increment-focus))

;;; The hands are external components as well, so 
;;; that all you need to do is select a hand and switch hands.

(defvar *hand*)

(defun choose-hand ()
  (trp 3 "~%Looking to the ")
  (if (zerop (random 2))
    (progn (setq *hand* *right-hand*) (trp 3 "right hand."))
    (progn (setq *hand* *left-hand*) (trp 3 "left hand.")))
  (focus-on-hand2)
  )

(defun focus-on-hand2 ()
   (setq *foa* *hand*)
   (report-hands))

(defun swap-hands ()
  (trp 3 "~%Looking to the ")
  (if (eq *hand* *left-hand*)
    (progn (setq *hand* *right-hand*) 
	   (mempush 'swap-hands '(:from left :to right))
	   (trp 3 "right hand."))
    (progn (setq *hand* *left-hand*) 
	   (mempush 'swap-hands '(:from right :to left))
	   (trp 3 "left hand."))
    )
  (focus-on-hand2)
  )

;;; Finally we need to replace the n1 and n2 with echoic buffers so
;;; that they aren't arguments to a lisp function. This also requires
;;; putting the addends into external stores which, like the hands,
;;; can be attended.  Instead of doing all that I just assume here
;;; that the problem is written on an external board, and that there
;;; is a sort of second set of eyes that can look at one or the other
;;; addend, and swap them, just like with the hands.  We ought to
;;; organize all the different buffers.  I wonder if kids ever get
;;; these mixed up, and if not, why not?

(defvar *ad1*)
(defvar *ad2*)

(defvar *addend*)

(defun choose-addend ()
  (if (zerop (random 2))
    (setq *addend* *ad1*)
    (setq *addend* *ad2*))
  ;; Indicate which addend we've chosen for min discovery.
  (setq *cla*
	(cond ((= *ad1* *ad2*) 'equal)
	      ((or (and (> *ad1* *ad2*) (= *addend* *ad1*))
		   (and (< *ad1* *ad2*) (= *addend* *ad2*)))
	       'larger)
	      (t 'smaller)
	      ))
  (trp 3 "~%Choose addend ~a.~%" *addend*)
  )

(defun swap-addends ()
  (trp 3 "~%Looking to the other addend: ")
  (if (eq *addend* *ad1*)
    (progn (setq *addend* *ad2*)
   	   (mempush 'swap-addends '(:from 1 :to 2))
	   )
    (progn (setq *addend* *ad1*)
   	   (mempush 'swap-addends '(:from 2 :to 1))
	   )
    )
  (trp 3 "~a.~%" *addend*)
  )

;;; Say-addend is just used to unify the calling form of the main fns
;;; so that they never need an argument

(defun say-addend ()
  (say *addend*)
  )

(defun choose-larger-addend ()
  (if (> *ad1* *ad2*)
    (setq *addend* *ad1*)
    (setq *addend* *ad2*))
  (setq *cla*
	(if (= *ad1* *ad2*) 'equal 'larger))
  (trp 3 "~%Choose addend ~a." *addend*)
  )

#| Okay, so next we need to replace the lisp function forms above with
P-chain versions.  This requires a lot of additional machinery.  The
image is that there is one and only one occurrance of a procedure, and
that what happens is that you encode trajectories of calls to these
procedures.  Since there are only a few relevant procedures here
(clear-eb, raise, and count-fingers).  Each of these is well
(pre)learned. |#

#| The chain representation is just a list of fn calls.  Each entry
   looks like this:

    (my-uid fn list-of-gotos)

   My-uid is a unique id assigned to each independent entry.  You'll
   see what this is used for below.

   Where the list of gotos is each:

    (strat-name xpr new-uid)

   Strat-name is the strategy under which this goto was entered.  xpr
   is a real valued transition probability, and new-uid is the uid of
   the place to go.

   The xprs get updated each time you run along this pathway.  (Is it
   normalized??)

|#

(defvar *lastuid*)

(defun init-chain ()
  (setq *chain* (copy-tree '((0 start nil)))) ; [copied so that it doesn't get mucked up later]
  (setq *lastuid* 1))

(defun load-strat (name sequence &aux last-step)
  (setq last-step (car *chain*))
  (dolist (step sequence)
    (setq last-step (load-step step (incf *lastuid*) last-step name))
    ))

(defun load-step (new-step step-uid last-step name &aux new-entry)
  ;; Add the new step to the chain list, unattached as yet.
  ;; This sort of ends up coming all in upside down, but it'll do so long as
  ;; the Car of the chain is always step 0!
  (push (setq new-entry (list step-uid new-step nil)) (cdr *chain*))
  ;; Update the old last-step to include the new step -- this actually attaches
  ;; it to the chain.
  (push (list name *initial-xpr* step-uid)
        (caddr last-step))
  ;; Return the new last-step ... ie., the new chain entry.
  new-entry
  )

;;; Running a strategy is, at this stage, very simple; just walk to
;;; chain from the start.

(defun run-strat (name n1 n2)
  (trp 1 "~%~%Testing ~a ~a ~a: ~%" name n1 n2)
  ;; Our friends the a-theoretic buffer loads.
  (setq *ad1* n1)
  (setq *ad2* n2)
  ;; Run the chain!
  (prog (uid step)
    (setq uid 0)
   loop
    (setq step (find-uid uid))
    (execute-step step)
    (setq uid (select-next-step step name))
    (if uid (go loop) (return 'done))
    ))

(defun find-uid (n)
  (find n *chain* :test #'(lambda (a b) (= a (car b)))))

(defun execute-step (step)
  (trp 3 "~%Doing ~a: ~a." (car step) (cadr step))
  (funcall (cadr step)))

(defun select-next-step (step name)
  (caddr (find name (caddr step) :test #'(lambda (a b) (eq a (car b))))))

;;; Metacognitive system; This is any old production system junk you
;;; feel like doing between and betwixt.  There are a number of
;;; general meta goals, in *global-mgoals* and some that are
;;; strategy-specific (in the strat 'mgoals slot).  All the mgoals
;;; that you would like to do during this run get pushed onto the
;;; *mgoals* and selected for when we have time.  The global *mgoal*
;;; contains the current list of active calls for the current
;;; mgoal. If that isn't empty when do-a-meta-cycle gets called, then
;;; we pop and do the top thing, else we scan for applicable meta
;;; goals and push one onto *mgoal*.

;;; Note that choosing what to do counts a a meta-cycle!

(defun do-a-meta-cycle ()
  (trp 2 "### Running a metacycle!~%")
  (incf *saved-ms-cycles*)
  (if *mgoal*
      (funcall (pop *mgoal*))
      (let ((possible-mgoals (find-possible-mgoals)))
	(if possible-mgoals
	    (setq *mgoal* (mgoal-procedures (nth (random (length possible-mgoals))
						 possible-mgoals)))))
	))

(defun find-possible-mgoals ()
  (mapcan #'(lambda (mg) (if (funcall (mgoal-pattern mg))
			     (list mg))
	      )
	  *mgoals*))

;;; Summarizes the results *record*; Remember that it's reversed if
;;; you want to do anything involving when something began to happen.
;;; This is mucho complicated because it's used both to report the
;;; results during runs, and to carry out post-hoc analysese, so
;;; there's lots of summarization taking place. 

(defstruct (sumrec)
  strat
  good? 
  code
  used
  ret
  nretcorrect
  ncorrect
  mcs ; meta-cycles-saved
  )

;;; Note that the entries in the probtable are lists of (n
;;; strat-name-string).  The reason that we use the strat-name-string
;;; instead of the strat id is that when doing this across multiple
;;; runs, we want to gather together results from the same strat,
;;; which has a different ID on each run, unfortunately.  This means
;;; that all assoc need to use equal (or string-equal).

(defvar *probtable*)

;;; Note the START-AT and STOP-AT keyargs are used to do dynamical
;;; anlaysis.  You can use them to limit the calculation to only a
;;; part of the records.  BE WARNED that if these are non-nil, it will
;;; only APPROXIMATELY (prob. +-1) operate at the right place,
;;; depending upon how fast summarize-n- trials-together scans and
;;; when the testings happens.  The last statistical entry will often
;;; be partial!  (START-AT may be unspecified even if STOP-AT is
;;; given; then it just starts at 0)

(defun summarize-records (&key (combine-trials 10) 
			       (report t)
			       (reset-summary-stats t)
			       (start-at 0)
			       (stop-at nil)
  		          &aux (rr (reverse *records*))
			       k  ; counter to tell when to start, stop, and update
			       stflag
			       sumrecs
			       allcorrect)
  (if report  (format t "~%~%----------~%Study Summary~%--------- [~a-~a]~%~%"
		      start-at
		      (or stop-at (length *records*))))

  ;; Initialize the problem array which will keep the error and strategy
  ;; usage records on a per-problem basis. (Too small for challanges!!)

  (if reset-summary-stats
      (progn
	(setq *probtable* (make-array '(6 6)))
	(init-min-stats)
	))

  ;; Init a record entry for each strategy.

  (setq sumrecs (mapcar #'(lambda (s) 
			    (if report
			     (format t "~a (~a):~% ~a~%~%" 
				     (strat-name s)
				     (name-that-strat (strat-chain s))
				     (mapcar #'car (strat-chain s))))
			     (make-sumrec :strat (strat-name s)
					  :code (mapcar #'car (strat-chain s))
					  :ncorrect (list 0) 
					  :nretcorrect (list 0)
					  :used (list 0) 
					  :ret (list 0) 
					  :mcs (list 0))
			     )
			 *stb*))
  (push 0 allcorrect)

  (if report  (format t "~%"))

  (setq k 1)
  (block recscan
    (dolist (rec rr)

      (if (> k start-at)
	  (progn
	    (if (and (not stflag) report)
		(format t "(!! Started analysis at k=~a !!)~%" k))
	    (setq stflag t)
	    ;; Even though it doesn't look like it from the 
	    ;; indentation, everything from here to the close of the 
	    ;; progn is included here!!

    ;; First store the problem's result and strat choice in the probtable.

    (let* ((problem (cadr (assoc 'problem rec)))
	   (a1 (first problem))
	   (a2 (second problem))
	   (strat (cadr (assoc 'strat rec)))
	   (result (cadr (assoc 'result rec)))
	   )
      ;; Don't record challenge problems!!
      (if (<= (+ a1 a2) 10)
	  (push (list result (name-that-strat strat))
		(aref *probtable* a1 a2)))
      )

    ;; This modifies the CAR of each sumrec entry to end up with a list
    ;; in each slot that has one element for each n trials.  We have to
    ;; push a 0 in front each n times through, and then the car (that 0)
    ;; will take the heat of the updating.

    ;; Set up a new entry in each strat's rec struct for this set of n trials.
    (if (zerop (mod (1+ k) combine-trials))
	(progn
	  (push 0 allcorrect)
	  (mapcar #'(lambda (sumrec)
		      (push 0 (sumrec-used sumrec))
		      (push 0 (sumrec-ret sumrec))
		      (push 0 (sumrec-mcs sumrec))
		      (push 0 (sumrec-ncorrect sumrec))
		      )
		  sumrecs)
      ))

    (let* ((s (cadr (assoc 'strat rec)))
	   ;; Find the summary record to update for this record entry.
	   (sumrec (find s sumrecs 
			 :test #'(lambda (n r) (eq n (sumrec-strat r)))
			 ))
	   ret?
	   )
      (incf (car (sumrec-used sumrec))) 

      (setq ret? (assoc 'USED-RETRIEVAL! rec))

      (if ret? (incf (car (sumrec-ret sumrec))))

      ;; Tally stats for a correct answer.
      (let* ((p (cadr (assoc 'problem rec)))
	     (r (apply #'+ p))
	     (a (cadr (assoc 'result rec))))
	(if a 
	    (progn
	      (if (= a r)
		  (progn
		    (incf (car (sumrec-ncorrect sumrec)))
		    (incf (car allcorrect))
		    (if ret? 
			(incf (car (sumrec-nretcorrect sumrec))))
		    ))
	      )
	  );if
	)

      (incf (car (sumrec-mcs sumrec)) (cadr (assoc 'saved-ms-cycles rec)))
      )

    )) ; progn ... if (or stflag ...> k...

    (incf k) ; update trial counter

    (if (and stop-at (= k stop-at))
	(progn
	  (if report (format t "(!! Record analysis interrupted at k=~a !!)~%" k))
	  (return-from recscan nil)))

    )) ; dolist block (recscan)

  (if report  (format t "~%-----~%"))
  (if report  (mapcar #'(lambda (s) (summarize-strat (sumrec-strat s) s)) 
		      sumrecs))
  (if report 
      (progn
	(format t "~%Overall %correct:~%")
	(mapcar #'(lambda (n) (format t "~a " (p2 (pconv n combine-trials)))) 
		allcorrect)
	(format t "~%")))

  ;; Insert the results into the meta-records.
  (mapcar #'(lambda (sumrec) (add-strat-summary-to-meta-records sumrec)) sumrecs)
  (push (reverse allcorrect) *overall-correct*)

  (if report
    (progn
      (format t "~%-----~%")
      (format t "Inverted Strategy Confidences; Sorted by preference.~%")
      (invert-confidences '(NOBIGNUMS EQUAL-ADDENDS))
      (invert-confidences '(NOBIGNUMS FIRST-LARGER))
      (invert-confidences '(NOBIGNUMS SECOND-LARGER))
      (invert-confidences '(BIGNUMS EQUAL-ADDENDS))
      (invert-confidences '(BIGNUMS FIRST-LARGER))
      (invert-confidences '(BIGNUMS SECOND-LARGER)) 
      (invert-confidences '(ADDITION-PROBLEM))
      ))

  (collect-min-stats report)

  (if report
    (progn
      (format t "~%-----~%")
      (format t "Problem success per strategy.~%")
      (analyze-problem-table)
      ))

  )

;;; Construct model internal correlation on a per-problem basis
;;; between %strat use (that is, non-retrieval strats), and
;;; %error. This is called after the probtable has been setup, so if
;;; you want dynamic analysis, you need to call is over and over while
;;; the problem table is being developed.  It only displays its
;;; results, so the user has to save them somehow.

(defvar *pts*)

(defun analyze-problem-table (&optional (complete-report nil)
			      &aux overtab)
  (setq overtab (make-array '(6 6)))
  (dotimes (a1o 5)
    (dotimes (a2o 5)
      (let* ((a1 (1+ a1o))
	     (a2 (1+ a2o))
	     (r* (aref *probtable* a1 a2))
	     (nall (length r*))
	     (allright 0)
	     (allwrong 0)
	     (sum (+ a1 a2))
	     stratlist 
	     ret-%-correct
	     ret-use-%
	     non-ret-correct-%-sum
	     )
	(if complete-report (format t "~%-- ~a + ~a = (~a occurances) " a1 a2 nall))
	(dolist (r r*)
          (let* ((result (first r))
		 (strat (second r))
		 (sentry (assoc strat stratlist :test #'string-equal))
		 )
	    (if (null sentry)
		(push (setq sentry (list strat 0 0)) stratlist))
	    (if (= sum result)
		(progn (incf (second sentry))
		       (incf allright))
	      (progn (incf (third sentry))
		     (incf allwrong))
	      )
	    ))
	(if complete-report (format t "right ~a%, wrong ~a%~%"
		(p2 (* 100 (/ allright (float nall))))
		(p2 (* 100 (/ allwrong (float nall))))))
	(setq non-ret-correct-%-sum ())
	(dolist (sentry stratlist)
	  (let* ((right (second sentry))
		 (wrong (third sentry))
		 (total (float (+ right wrong)))
		 a b c)
	    (setq a (* 100 (/ total nall)))
	    (if complete-report (format t " ~a (~a%) : " 
					(first sentry) (p2 a)))
	    (setq b (* 100 (/ right total)))
	    (setq c (* 100 (/ wrong total)))
	    (if complete-report (format t " right ~a%, wrong ~a%~%" (p2 b) (p2 c)))
	    (if (string-equal "Retrieval" (first sentry))
		(setq ret-use-% a ret-%-correct b)
	      (push b non-ret-correct-%-sum))
	    ))
	(if (null ret-use-%) (setq ret-use-% 0.0)) ; in case ret NEVER used!
	(setf (aref overtab a1 a2)
	      (list :overall-%-error (- 100 (* 100 (/ allright (float nall))))
		    :%-non-ret-use (- 100 ret-use-%)
		    :%-ret-correct ret-%-correct
		    :non-ret-mean-%-correct 
		       (if non-ret-correct-%-sum
			   (mean non-ret-correct-%-sum))
		    ))
	)
      ))
  (setq *pts* overtab)
  (if complete-report (format t "~%--- Intramodel correlations ---~%"))
  (format t "r % errors (per problem) with % overt strat use =~a~%"
	  (p2 (getf (correlate (nils-to-0.0 (scan-probtable :overall-%-error
							    overtab))
			   (nils-to-0.0 (scan-probtable :%-non-ret-use overtab)))
		:r)))
  (if complete-report (dump-probtable overtab))
  )

(defun dump-probtable (&optional (overtab *pts*))
  (display-probtable-value overtab :overall-%-error)
  (display-probtable-value overtab :%-non-ret-use))

;;; Displays a given value in a getf in the 5x5 array (which is
;;; actually 6x6 to simply origin analysis) of problems.

(defun display-probtable-value (probtable value)
  (format t "~%~%----------~%            ~a~%~%" value)
  (format t "        1      2      3      4      5~%")
  (dotimes (a1o 5)
    (format t " ~a : " (1+ a1o))
    (dotimes (a2o 5)
      (let ((a1 (1+ a1o))
	    (a2 (1+ a2o)))
	(format t " ~a " (p2 (getf (aref probtable a1 a2) value)))))
    (format t "~%")
    )
  (format t "----------~%")
  )

;;; Gives us back a vector of a chosen getf'able entry in every
;;; element of the problem table.  Note that there made be nil
;;; responses here if there was no entry in that cell.  (Use
;;; nils-to-0.0 to ensure that he result can be analyzed.)

(defun scan-probtable (entry table &aux r)
  (dotimes (i 5 r)
    (dotimes (j 5)
      (push (getf (aref table (1+ i) (1+ j)) entry) r)
      )))

;;; This just ensures that the stats routines don't get nils in their
;;; input.  Generally speaking, if there's been enough examples done,
;;; the nils will always be in the same places (e.g., 0+n and n+0), so
;;; this has no effect on the analyses.

(defun nils-to-0.0 (l)
  (maplist #'(lambda (e) (if (null (car e))
			     (rplaca e 0.0)))
	   l)
  l)

;;; Same as assoc but gets ALL the matching entries and returns them
;;; as a list.  (Also uses EQUAL)

(defun assoc* (w l)
  (loop for i in l
        if (equal (car i) w)
	collect i))

;;; Report the confidence of particular strategies on each problem
;;; type, in best-first order.

(defun invert-confidences (c &aux l)
  (format t "~a: " c)
  (dolist (s *stb*)
    (let ((conf (find-confidence s c)))
      (if conf 
	(push (cons (confidence-value conf)
		    (format nil "~a (~a) = ~a, " 
			    (strat-name s)
			    (name-that-strat (strat-chain s))
			    (p2 (confidence-value conf))
			    )
		    )
	      l)))
    )
  (dolist (i (sort l #'(lambda (a b) (> (car a) (car b)))))
	  (format t "  ~a~%" (cdr i)))
  (format t "~%")
  )

;;; After we've calculated the data to report for each strategy add
;;; the information to the multiple run table for statistical
;;; analysis.  This uses the same structure as the individual records
;;; but the slots which used to hold one number for each n trials now
;;; holds a list for each n trials.  In these lists there's one
;;; element for each experimental RUN.

(defun add-strat-summary-to-meta-records (this-summary)
  (let ((record (find this-summary *runsum*
		      :test #'(lambda (a b) (equal (sumrec-code a)
						   (sumrec-code b))))))
    (if (null record)
	(progn
	  (setq record this-summary)
	  (push record *runsum*)
	  (setf (sumrec-ncorrect record) 
		(mapcar #'list (sumrec-ncorrect record)))
	  (setf (sumrec-nretcorrect record) 
		(mapcar #'list (sumrec-nretcorrect record)))
	  (setf (sumrec-used record) (mapcar #'list (sumrec-used record)))
	  (setf (sumrec-ret record) (mapcar #'list (sumrec-ret record)))
	  (setf (sumrec-mcs record) (mapcar #'list (sumrec-mcs record)))
	  )
      ;; Note that this puts everything in BACKWARDS!
      (progn
	(setf (sumrec-ncorrect record)
	      (mapcar #'cons 
		      (sumrec-ncorrect this-summary)
		      (sumrec-ncorrect record)))
	(setf (sumrec-nretcorrect record)
	      (mapcar #'cons 
		      (sumrec-nretcorrect this-summary)
		      (sumrec-nretcorrect record)))
	(setf (sumrec-used record)
	      (mapcar #'cons
		      (sumrec-used this-summary)
		      (sumrec-used record)))
	(setf (sumrec-ret record)
	      (mapcar #'cons
		      (sumrec-ret this-summary)
		      (sumrec-ret record)))
	(setf (sumrec-mcs record)
	      (mapcar #'cons
		      (sumrec-mcs this-summary)
		      (sumrec-mcs record)))
	)
      ) ;if
    ))

(defun summarize-strat (s sumrec 
			&aux (nused 0) (nright 0) (nwrong 0)
			     (rused 0) (rright 0) (rwrong 0)
			     sn)
  (setq sn (name-that-strat 
	      (strat-chain 
	       (find-strat-by-name s)
	       )))
  (dolist (rec *records*)
    (if (eq (cadr (assoc 'strat rec)) s)
	(progn
	  (incf nused)
	  (let* ((p (cadr (assoc 'problem rec)))
		 (r (apply #'+ p))
		 (a (cadr (assoc 'result rec))))
	    (if a
		(if (= a r)
		    (incf nright)
		    (incf nwrong))
	      )
	    )
	  (if (assoc 'USED-RETRIEVAL! rec)
	      (progn
		(incf rused)
		(let* ((p (cadr (assoc 'problem rec)))
		       (r (apply #'+ p))
		       (a (cadr (assoc 'result rec))))
		  (if a
		      (if (= a r)
			  (incf rright)
			(incf rwrong))
		    )))
	    ) ; if
	  )) ; progn ... if
    ) ; dolist
 (if (zerop nused)
     (format t "~a was (~a) never used.~%" s sn)
   (format t "~a (~a) was selected ~a times (~a%); right ~a (~a%), wrong ~a (~a%)~%"
	   s sn
	   nused (p2 (* 100 (/ (setq nused (float nused)) (length *records*))))
	   nright (p2 (* 100 (/ nright nused)))
	   nwrong (p2 (* 100 (/ nwrong nused)))
	   ))
  (if (not (zerop rused))
      (format t "Retrieval was used ~a times (~a%); right ~a (~a%), wrong ~a (~a%)~%"
	      rused (p2 (* 100 (/ (setq rused (float rused)) nused)))
	      rright (p2 (* 100 (/ rright rused)))
	      rwrong (p2 (* 100 (/ rwrong rused))))
    (format t "Retrieval was never used.~%"))

  (show-abbrev-sumrec-data "Times Used" (reverse (sumrec-used sumrec)))
  (show-abbrev-sumrec-data "Retrieval " (reverse (sumrec-ret sumrec)))
  (show-abbrev-sumrec-data "Meta Saved" (reverse (sumrec-mcs sumrec)))
  (show-abbrev-sumrec-data "Correct   " (reverse (sumrec-ncorrect sumrec)))

  (format t "~%")
  )

(defun show-abbrev-sumrec-data (name data &aux compress)
  (if (> (length data) 20) (setq compress t))
  (format t "~A: " name)
  (dolist (d data)
    (if compress
	(format t "~A"  (if (> d 9) '+ d))
      (format t "~a " d)))
  (format t "~%")
  )

;;; Delooping makes each op into a (type . token) pair.  The type is
;;; used to do shortcutting and to call the operator, the token is
;;; used for subcognitive jump generation.

(defun deloop-uniqueify (chain)
  (mapcar  #'(lambda (op) (cons op (gentemp (format nil "~a" op)))) chain))

(defun type-chain (dchain)
  (mapcar #'car dchain))
(defun token-chain (dchain)
  (mapcar #'cdr dchain))

;;; The list of possible ops is used to select among operators when
;;; the child is guessing what to do next based upon imitation.

(setq *posops*
  '(clear-hands choose-hand choose-addend
    say-addend clear-eb raise swap-hands swap-addends
    count-fingers end!))

;;; Here's are the actual strategies.  

(setq *count-from-one-twice* '(

  ;; First addend on first hand.
		      
  clear-hands choose-hand choose-addend say-addend 
  clear-eb raise

  ;; Second addend on the other hand.

  swap-hands swap-addends
  say-addend clear-eb 
  raise

  ;; Final count out.

  choose-hand clear-eb
  count-fingers
  swap-hands
  count-fingers

  end!
  ))


;;; Okay, now it's easy to turn cf into shortcut sum by just dropping
;;; a couple of steps.

(setq *count-from-one-once* '(

  ;; First addend on first hand.

  clear-hands choose-hand choose-addend say-addend 
  clear-eb raise

  ;; Second addend on the other hand.

  swap-hands swap-addends
; say-addend clear-eb 
  raise

  ;; Final count out.

; choose-hand clear-eb 
; count-fingers 
; swap-hands
; count-fingers 

  end!
  ))


;;; This is just min without the choose-larger-addend; It's called
;;; Count From Either, and with CLA it's Min.  This translation is
;;; carried out by name-that-strat.

(setq *count-from-either* '(

  ;; First addend on first hand.

  clear-hands choose-hand choose-addend say-addend
; clear-eb raise

  ;; Second addend on the other hand.

  swap-hands swap-addends
; say-addend clear-eb 
  raise

  ;; Final count out.

; choose-hand clear-eb 
; count-fingers 
; swap-hands
; count-fingers 

  end!
  ))

;;; Additional strats that we want to give names to.  The namer will
;;; remove Choose-Addend nor Choose-Larger Addend in them so that the
;;; match catches both varieties.

(setq *ret* '(try-retrieval-op))

(setq *cfr* 
  '(CLEAR-HANDS CHOOSE-HAND SAY-ADDEND CLEAR-EB
		RAISE SWAP-HANDS SWAP-ADDENDS RAISE CHOOSE-HAND
		CLEAR-EB COUNT-FINGERS SWAP-HANDS COUNT-FINGERS END!))

;;; An experiment in discovering the min strategy through educated
;;; guesses and constraint.  First try: delete arbitrary lines from
;;; the code and see if it still satifies the conditions which are:

(setq *tests* '(
  represent-both-addends
  result-includes-both-addends
  dont-count-no-fingers
; removed tests:
;  resets-at-start-only
;  uses-both-addends
;  addends-are-processed-when-chosen
;  raises-have-a-choose-hand
;  if-your-gonna-count-count-twice 
  ))

;;; Don't count no fingers was added 970505 when the new shortcut sum
;;; was installed to avoid the problem of counting on a hand that had
;;; no fingers raised, which was allowed through by the new removal of
;;; a 'raise check from the *skeletal-plan* required in order to
;;; permit the correct scsum.  It works by ensuring that there's not
;;; MORE count-fingers than raises! (Can't be an = test; that rejects
;;; scsum!)  There is a virtual interpretation of this which is that a
;;; hand with nothing represented makes no sense, but this is hard to
;;; check from just the program structure, so this is our proxy.

(defun dont-count-no-fingers (p)
  (>= (count 'raise p)
      (count 'count-fingers p))
  )
    
;;; This is an attempt to make sense of the hard-to-describe-hack by 
;;; producing a test that excludes it without excluding good strats.
;;; The idea is, as in the name, that after the last clear-eb you have to
;;; have some way of getting to both hands and/or addends.

(defun result-includes-both-addends (p)
  ;; Min has no clear-eb's!
  (if (not (member 'clear-eb p))
      t
    (progn
      ;; Find everything after the last clear eb.
      (loop (if (not (member 'clear-eb p)) (return t)) (pop p))
      ;; Make sure there's the right stuff in there.  Specifically,
      ;; after the last clear-eb there's two raises.  
      (or 
       (member 'raise (cdr (member 'raise p)))
       (member 'count-fingers (cdr (member 'raise p)))
       (member 'count-fingers (cdr (member 'count-fingers p)))
       (member 'raise (cdr (member 'count-fingers p)))
       ) ; or
      ) ;progn
    ))

;;; This is composed from Min.

(setq *skeletal-plan* '(choose-hand say-addend ; raise !!!
				 swap-hands swap-addends
				 raise
				 ))

(defun represent-both-addends (p)
  (dolist (op *skeletal-plan* t)
    (if (not (setq p (member op p))) (return nil))))

;;; Test a strategy to see if it's good.

(defun good-strat? (p)
  (trp 0 ">>> Proposed ~a ...~%" (name-that-strat p))
  (incdat 'proposed)
  (dolist (test *tests* t)
    (if (not (funcall test p))
	(progn 
	  (trp 0 ">>> Test ~a failed!~%" test)
	  (return nil))
      )))

#| The rest of these were removed about 970319 because they don't appear 
   to be required.

;;; Ensure that if there's any counting of fingers going on, you
;;; get both hands!

(defun if-your-gonna-count-count-twice (p &aux temp)
  (if (setq temp (member 'count-fingers p))
      (member 'count-fingers (cdr temp))
    t))

;;; Makes sure that you don't try to raise without choosing a hand.

(defun raises-have-a-choose-hand (p)
  (let* ((p2 (member 'swap-hands p))
	 (p1 (first-n (- (length p) (length p2)) p))
	 )
    (and (if (member 'raise p1)
	     (or (member 'choose-hand p1)
		 (member 'swap-hands p1))
	   t)
	 (if (member 'raise p2)
	     (or (member 'choose-hand p2)
		 (member 'swap-hands p2))
	   t)
	 )))

;;; This is a little complex.  Each time a choose-addend (or c-l-a)
;;; happens, there has to be either a say-addend, or a raise, after
;;; it, but before the NEXT choose-addend (or c-l-a)

(defun addends-are-processed-when-chosen (p)
  ;; Break the program into two parts, at the swap-addends
  (let* ((p2 (member 'swap-addends p))
	 (p1 (first-n (- (length p) (length p2)) p))
	 )
    (and (or (member 'raise p1)
	     (member 'say-addend p1))
	  (or (member 'raise p2)
	     (member 'say-addend p2))
	  )))

;;; Checks that we reset the hands at and only at the beginning.

(defun resets-at-start-only (p)
  (and (eq (car p) 'clear-hands)
       (not (member 'clear-hands (cdr p)))))

;;; Must have a choose-addend (or choose-larger-addend) and a swap addend.

(defun uses-both-addends (p)
  (and (setq p (cdr (or (member 'choose-addend p)
			(member 'choose-larger-addend p))))
       (member 'swap-addends p)))

|#

;;; This is a sort of clunky implementation of shortcutting.  It tries
;;; to pull the indicated sequence, but since it may appear more than
;;; once, it can skip some occurrances.

(defun remove-subseq (seq-to-rem seq &optional (n-to-skip 0) &aux mid)
  (setq seq (copy-list seq))
  (setq mid seq)
  (dotimes (s (1+ n-to-skip))
    (setq mid (find-subseq seq-to-rem (cdr mid)))
    )
  (if mid
      (dotimes (k (length seq-to-rem))
        (rplaca mid (cadr mid))
	(rplacd mid (cddr mid))))
  seq)


(defun find-subseq (find in)
  (prog ()
    (maplist #'(lambda (l)
		 (if (same-at-front find l) 
		     (return l)))
	     in)))
		     
(defun same-at-front (a b)
  (dotimes (i (min (length a) (length b)) t)
    (if (not (equal (pop a) (pop b)))
	(return nil))))

;;; Names the standard strategies, and also choose-larger-addend
;;; version of them.  (The choice op has already been dropped from the
;;; strat table upon loadup.)

;;; The table of strategies has the var that contains the strat and
;;; the its "pen name" which is used in labels.  There is an important
;;; hack here in which "Count_From_Either/CLA" is translated to min.

(setq *strat-table*
  '((*ret* "Retrieval")
    (*count-from-one-twice* "Count_From_One_Twice[CF][Sum]")
    (*count-from-one-once* "Count_From_One_Once[Shortcut_Sum]")
    (*count-from-either* "Count_From_Either")
    (*cfr* "Count_From_One_Twice+Recount[CF+Recount]")
    ))

(defun name-that-strat (p)
  (if (symbolp p)
      (setq p (find-strat-by-name p)))
  (if (eq 'strat (type-of p))
      (setq p (strat-chain p)))
  (if (listp (car p))
      (setq p (type-chain p)))
  (let* ((p- (drop-choose-op p))
	 (n1 (dolist (s *strat-table* "new")
	       (if (equal p- (car s))
		   (return (second s)))))
	 )
    (if (member 'choose-larger-addend p)
	(if (string-equal n1 "Count_From_Either")
	    "Min[Count_From_Either/CLA]"
	  (format nil "~a/CLA" n1) 
	  )
      n1)
    )
  )

(defun drop-choose-op (p) 
  (remove 'choose-addend (remove 'choose-larger-addend (copy-list p))))

;;; Fix the table to have expanded versions of each strat w/o the
;;; choice ops.

(mapcar #'(lambda (entry) (rplaca entry (drop-choose-op (eval (car entry)))))
	*strat-table*)

(defun find-strat-by-name (name)
 (find name *stb* :test #'(lambda (n st) (eq n (strat-name st)))))
(defun whats (sname) (find-strat-by-name sname))

;;; Try each strategy once, just to see if it is sane.

(defun tryemall ()
  (mapcar #'(lambda (s) (demo (strat-name s))) *stb*))

;;; To demonstrate a strategy.

(defun demo (sname &optional (permit-meta-cycles? nil)
		             (a1 2) (a2 5) 
		   &aux (*tl* 10))
  (setq *bc* ())
  (setq *ad1* a1)
  (setq *ad2* a2)
  (format t "----- ~A ----- ~a+~a.~%" 'demo *ad1* *ad2*)
  (setq *strat* (whats sname))
  (format t "Selected to use ~a (~a).~%" 
	  (strat-name *strat*)
	  (name-that-strat (strat-chain *strat*)))
  (setq *xthresh*
	(if permit-meta-cycles?
	    (let ((c (find-confidence *strat* 
				      (characterize-problem *ad1* *ad2*))))
	      (if c
		  (- 1 (confidence-value c))
		(- 1 (strat-default-confidence *strat*))))
	  1e6))
  (trp 1 "Transition threshold is ~a~%" *xthresh*)
  (setq *tc* (strat-chain *strat*))
  (setq *mgoal* ()) ; clear from left over previous runs.
  (setq *mgoals* (append (copy-list (strat-mgoals *strat*))
			 (copy-list *global-mgoals*)))
  (setq *saved-ms-cycles* 0)
  (memclear)
  (setq *ncycles* 0) 
  (drive)
  (format t "Result = ~a~%" *eb*)
  (trp 1 "Number of saved MS cycles = ~a.~%" *saved-ms-cycles*)
  'done
  )

;;; This code carries out parameter scanning test.  For each model
;;; param we list its low-end, high-end, and number of steps (two =
;;; just do the low and high, three = one in between, etc.)  If
;;; there's just one value, it's a static param.  (See *model-params*
;;; near the top.)

(defun test (&key (nruns 1000) (ntimeseach 1) (report nil))
  (resdat)
  (if system::*dribble-stream* (dribble))
  (dribble (format nil "results/dribble.~a.~a" (date-as-int) (time-as-int)))
  (setq *runsum* nil *overall-correct* ())
  (let ((mpt (compute-model-params-table *model-params*)))
    (format t "There are ~a parameter combinations.~%" (length mpt))
    (dolist (mps mpt)
      (dotimes (time ntimeseach)
	(test-with-param-settings mps nruns report))))
  (dribble)
  )

;;; Get a list with one entry per combination of parameters, as per
;;; the model params list, above.  The assumed order is the same as
;;; that list.

(defun compute-model-params-table (mpl)
  (cond ((null mpl) ())
	(t (cmp-cross-embed (all-values-of (car mpl))
			    (compute-model-params-table (cdr mpl))))
	))

(defun cmp-cross-embed (v* l &aux r)
  (cond ((null l) (mapcar #'list v*))
	(t (dolist (v v* r)
		   (setq r (append r (mapcar #'(lambda (l) (cons v l)) l)))
		   )
	   )
	))
  

(defun all-values-of (mp &aux r)
  (cond ((or (= 2 (length mp))
	     (not (numberp (cadr mp))))
	 (copy-list (cdr mp)))
	(t (do ((v (second mp) (+ v (/ (- (third mp) (float (second mp)))
				       (fourth mp)))))
	       ((>= v (third mp)) r)
	       (push v r)))
	))

(defun test-with-param-settings (mps nruns report)
  (format t "======================================================~%")
  (with-open-file (f (form-record-save-filename)
		     :direction :output
		     :if-exists :supersede)
    (set-params mps f)
    (inner-test t :nruns nruns :report report :record-save-stream f)
    ))

(defun set-params (mps save-stream)
  (mapcar #'(lambda (name value) 
	      (setp name value save-stream)
	      )
	  (mapcar #'car *model-params*) mps))

(defun setp (name value save-stream)
  (set name value)
  (format t "Param ~a <- ~a ~%" name (p2 value))
  (format save-stream "(setq ~a '~a)~%" name value)
  )

;;; Picks up a records dump file and runs a particular strategy from it.

(defun tsfrf (record-file-id-string strat-s-number) ; try-strat-from-record-file
  (let ((fn (format nil "/users/shrager/development/add/results/records.~a" 
		   record-file-id-string)))
    (if (not (file-exists? fn))
	(break "File doesn't exist!"))
    (load fn)
    (demo strat-s-number)
    ))

;;; Recover a series of records (by filename match) and run a given
;;; fn. on each.  Need to be REALLY careful through all this analysis
;;; that we don't get the records backwards since they get swapped
;;; around a LOT. 

(defun dosum (pattern &key (combine-trials 10)
		           (prefix "x")
			   (start-at 0)
			   stop-at
			   fullreport
			   correport
			   (postloadfn #'xlcla)
			   )
  (if system::*dribble-stream* (dribble))
  (dribble (format nil "results/~a.sum.~a.~a" prefix (date-as-int) (time-as-int)))
  (format t "Analyses for ~a by ~a [~a-~a]~%"
	  pattern combine-trials
	  (or start-at 'start)
	  (or stop-at 'end))
  (setq *probtable* (make-array '(6 6)))
  (init-min-stats)
  (maprecords pattern combine-trials start-at stop-at fullreport postloadfn)
  (statify-mrec (format nil "~a.~a" prefix combine-trials) combine-trials)
  (report-mean-percent-correct combine-trials)
  (analyze-problem-table correport)
  (report-min-stats)
  (dribble)
  )

(defun report-mean-percent-correct (combine-trials)
  (format t "Overall % correct per block:~%")
  (loop for i from 0 to (1- (length (car *overall-correct*)))
	do (let ((d (loop for rec in *overall-correct*
			  collect (/ (nth i rec) combine-trials))))
	     (format t "~a (~a)~%" 
		     (p2 (mean d))
		     (p2 (standard-error d))
		     ))
	))

(defun maprecords (pattern combine-trials start-at stop-at fullreport postloadfn)
  (setq *runsum* nil)
  (setq *overall-correct* nil)
  (let ((files (directory (format nil "results/~a" pattern))))
    ;; Report the params from the topmost file.
    (load (car files))
    (dolist (p (collect-model-params))
      (format t "~a = ~a~%" (first p) (p2 (second p))))
    ;; Run thru all the rest.
    (dolist (f files)
      (load f)
      (if postloadfn 
	  (progn 
	    (format t "(Executing postloadfn.)~%")
	    (funcall postloadfn))
	(format t "(No postloadfn was called for.)~%")
	)
      (summarize-records :combine-trials combine-trials 
			 :report fullreport
			 :reset-summary-stats nil
			 :start-at start-at
			 :stop-at stop-at)
      );dolist
    );let
  )

;;; Note that I think everything's in REVERSE ORDER when we get here!

(defun statify-mrec (prefix combine-trials)
  (mapcar #'sm2 *runsum*)
  (produce-plot-file 'used prefix combine-trials)
  ;; Thes rest of these are out because I never use them
 ;(produce-plot-file 'ret prefix combine-trials)
 ;(produce-plot-file 'nretcorrect prefix combine-trials) ; this one might be buggy
 ;(produce-plot-file 'ncorrect prefix combine-trials)
 ;(produce-plot-file 'mcs prefix combine-trials)
  )

;;; Converts a value to a percentage of the sum.

(defmacro pconv (v sum)
  `(* 100.0 (/ (float ,v) ,sum)))

;;; Very messy fns to create plot data files and gnuplot drivers.
;;; Comverts each value to a percentage based on the number of trials
;;; combined to get the values.

(defvar *gnuplot-these-strats*)
(setq *gnuplot-these-strats*
   '("Min[Count_From_Either/CLA]"  
     "Retrieval"
     "Count_From_One_Twice[CF][Sum]"
     "Count_From_One_Once[Shortcut_Sum]"
     "Count_From_Either"
     ))

(defun produce-plot-file (name prefix combine-trials
			       &key (include-column-names t)
			       &aux names-for-gnuplot data-file-name)
  (with-open-file (f (setq data-file-name 
			   (format nil "results/~a~a.dat" prefix name))
		     :direction :output :if-exists :supersede)
   (dolist (s *runsum*)
     (let ((name (substitute #\- #\space 
		   (or (name-that-strat (sumrec-code s))
		       'new))))
       (push name names-for-gnuplot) ; these will be reversed!
       ;; Usually we don't want these because they screw up plotting packages
       ;; but I include them anyhow and make the user hand-delete them because
       ;; I want to ensure that we don't lose the info.
       (if include-column-names 
	   (format f "\"~a\" \"~a-stderr\" " name name))
       )
     )
   (format f "~%")
   ;; Put out the data itself.
   (dotimes (k (length (sumrec-used (car *runsum*))))
     (format f "~a " (* combine-trials (1+ k))) ; entry counter for plotting convenience
     (dolist (s *runsum*)
       (let ((v (nth k (reverse ;; put things back into fwd order ???
			(funcall (read-from-string 
				  (format nil "sumrec-~a" name)) s)
			)
		     ))
	     )
	                 ;; mean   stderr (for errorbars)
	 (format f "~a ~a "
		 (p2 (pconv (car v) combine-trials))
		 (p2 (pconv (cdr v) combine-trials)))
	 ))
     (format f "~%")
     )
   )
  ;; And produce the gnuplot control sequence...
  ;; (remove "results/" 'cause #$@^%^$@% gnuplot lines are too long otherwise!)
  (setq data-file-name (subseq data-file-name 8)) 
  (with-open-file (gpf (format nil "results/~a~a.gnuplot" prefix name)
		       :direction :output :if-exists :supersede)
    (format gpf "plot ")
    (do ((col 2 (+ col 2))
	 (names (reverse names-for-gnuplot)
		(cdr names)))
	((null names))
     ;; Only bother plotting the ones we really care about.
     (if (member (car names) *gnuplot-these-strats* :test #'equal)
	(progn
	 (if (> col 2) 
	     (format gpf ", "))
	 (format gpf "\"~a\" us 1:~a:~a  title \"~a\" w l, \"~a\" us 1:~a:~a notitle w er "
		 data-file-name col (1+ col)
		 (car names)
		 data-file-name col (1+ col)
		 )
	 )
       ) ; if
	 ) ; do
    (format gpf "~%pause -1 \"hit return to kill plot\"~%")
    ) ; w-o-f
  )

(defun sm2 (sumrecx)
  (maplist #'sm3 (sumrec-used sumrecx))
  (maplist #'sm3 (sumrec-ret sumrecx))
  (maplist #'sm3 (sumrec-nretcorrect sumrecx))
  (maplist #'sm3 (sumrec-ncorrect sumrecx))
  (maplist #'sm3 (sumrec-mcs sumrecx))
  )

(defun sm3 (v*l)
  (setf (car v*l) (cons (mean (car v*l))
			(standard-error (car v*l))))
  )

;;; Collect up all the strategies discovered.

(defun collect-strats (pattern)
  (setq *cs* ())
  (dolist (f (directory (format nil "results/~a" pattern)))
    (load f)
    ;; if we've already got this one in the list, skip it, else
    ;; add it in.
    (dolist (s *stb*)
      (if (member s *cs*
		  :test #'(lambda (ns stbentry)
			                                   ;;; ??? Bug ??? ns instead of s???
			    (equal (type-chain (strat-chain s))
				   (type-chain (strat-chain stbentry)))))
	  (format t "~a is already there.~%" (strat-name s))
	(progn
	  (format t "~a is new.~%" (strat-name s))
	  (push s *cs*))
	))
    )
  )

(defun reload-special-strats ()
  ;; We know a-priori that this has all the strats in it.
  (load "results/all-strat-record.19970210.29655")
  ;; gather the summary stats for this one trial
  (setq *runsum* nil)
  (summarize-records :combine-trials 10)
  ;; mark everything as to its quality
  ;; and give everything its true name
  (dolist (r *runsum*)
    (setf (sumrec-good? r) (good? r))
    (setf (sumrec-strat r) 
	  (cons (sumrec-strat r)
		(name-that-strat (sumrec-code r))))
    )
  )

(defun match-level (l)
  (sum (mapcar #'(lambda (p) (if (eq (car p) (cadr p)) 1 0)) l)))

(defun apply-test-pattern (tp code)
  (cond ((null tp) t)
	((member (car tp) code)
	 (apply-test-pattern (cdr tp) code))
	(t nil)))

(defun good? (sumrec &aux pc)
  (let ((n (sum (mapcar #'car (sumrec-ncorrect sumrec))))
	(d (sum (mapcar #'car (sumrec-used sumrec))))
	)
    (format t "~a (~a) Used: ~a/ Correct: ~a = " 
	    (sumrec-strat sumrec)
	    (name-that-strat (sumrec-code sumrec))
	    d n)
    (if (> d 0)
	(progn
	  (format t "(~a%) => " (setq pc (* 100 (/ n (float d)))))
	  (if (> pc 80)
	      (progn
		(format t "valid!~%")
		t)
	      (progn
		(format t "NOT valid!~%")
		nil)
	      )
	  )
      (progn
	(format t "not computable (NOT valid)!~%")
	nil
	)
      )
    ))

;;; This is a different approach in which each good is contrasted with
;;; the bads, and vice versa.  Have to to (reload-special-strats)
;;; first.

(defun contraster ()
  (setq *gl* ())
  (maplist #'cs2 *runsum*)
  (length *gl*)
  (list-only-bads)
  )

(defun cs2 (ss)
  (let ((s1 (car ss))
	(rest (cdr ss)))
    (mapcar #'(lambda (s) (cs3 s1 s)) rest)
    ))

(defun cs3 (s1 s2)
  (push (list (list (sumrec-strat s1)
		    (sumrec-strat s2))
	      (list (sumrec-good? s1)
		    (sumrec-good? s2))
	      (contrast (sumrec-code s1) 
			(sumrec-code s2)))
	*gl*))

(defun list-only-bads ()
  (dolist (i *gl*)
    (if (not (eq (car (second i)) (cadr (second i))))
	(print i))))

;;; This is the key fn.  There may be several versions of it.

(defun contrast (c1 c2 &aux missing extra)
  (dolist (op c1)
    (if (not (member op c2))
	(push op missing)
      (setq c2 (remove op c2 :count 1))
      )
    )
  (dolist (op c2)
    (if (not (member op c1))
	(push op extra)
      (setq c1 (remove op c1 :count 1))
      )
    )
  (list 'missing-from-second missing 'extra-in-second extra)
  )
	   
;;; Check all the strats in *multiple...* through the tests; for
;;; testing out new tests.

(defun testall ()
  (mapcar #'(lambda (sumrec) (good-strat? (sumrec-code sumrec)))
	  *runsum*))

;;; Retest and remove strats from *runsum* that aren't valid by the
;;; various tests.  This is used to filter bad strats loaded from
;;; records and put into *runsum* when new tests are added.
;;; use (contraster) to look at what's left.

(defun refilter ()
  (format t "~a left" 
    (length (setq *runsum*
		  (compress (testall) *runsum*))
	    )))

;;; Pretty printing the memory stack so that we can see what happened.

(defun ppms ()
  (format t "-----------~%")
  (dolist (m (reverse *memstack*))
    (cond ((eq 'say (car m)) (format t "   <~a>~%" (cadr m)))
	  (t (print m)))
    )
    (format t "-----------~%")
    )

;;; Convert the cla precision record into a list of values from which
;;; we can use a t-test (v. 0.0) to tell if we should use
;;; choose-larger-addend or not.  This isn't very clearly
;;; thought-through at the moment.
	    
(defun second-larger-is-significantly-better (&aux r (claworksum 0.0)
						     (nclaworksum 0.0)
						     (nentries 0))
  (dolist (entry *cla-history*)
    (incf nentries)
    (let ((right? (second entry))
	  (speed (first entry))
	  (type (third entry)))
      ;; Record the efficiency of the execution per type.
      (case type
       (larger (incf claworksum speed))
       (t (incf nclaworksum speed))
       )
      ;; Record whether we got it right or not.
      (push 
       (if right? ; did we get it right?
	   (case type ; correct
		 (larger 1)
		 (smaller -1)
		 (equal 0)
		 )
	 (case type ; incorrect
	       (larger -1)
	       (smaller 1)
	       (equal 0)
	       )
	 )
       r) ; push
      )
    )
  ;; Make sure there's enough data, and at least one non-zero (or
  ;; t1-test will crash!)
  (if (and (> nentries 10)
	   (dolist (e r nil) (if (not (zerop e)) (return t))))
      ;; Okay, it's not all zeros; analyze!
;      (print (list (/ claworksum nentries) (/ nclaworksum nentries) 
;		   (mean r) (t1-test r 0.0 nil)))
    ;; All zeros ain't different from zero!
    nil)
    ; !!! Temp hack always says to go for it!
  t ; !!! BUG
  )

;;; 

(defun cortest (&key (pattern "records.*") (length 1000) (window 100))
 (loop for i from 1 to (+ 10 (- length window)) by window
   do (dosum pattern :start-at i :stop-at (+ i window))
   ))

(defun challenge-record? (r)
  (let ((addends (cadr (assoc 'problem r))))
    (< 10 (+ (first addends) (second addends)))
    ))

;;; Calculate the percetage of min (v. other strats) utilization
;;; around min discovery -- before and after separately.  The reason
;;; that this is important is to align the challenge problem hits so
;;; that they don't overlap one-another, causing spurious appearance
;;; of a positive result.  There's some sense to embedding this into
;;; summarize-records, but it's easier to do it this way!  Note that
;;; we don't want to record the challenge problems themselves, and so
;;; these are excluded from both the sum and from the min proportions!

#|
(defun collect-min-stats (report
			  &aux (rr (reverse *records*))
			       (k 0)
			       (prechalsum 0)
			       (prechalmin 0)
			       (postchalsum 0)
			       (postchalmin 0)
			       chalfound?
			       )
  (dolist (rec rr)
    (incf k)
    (if (challenge-record? rec) ; On challenge problems....
	(if (not chalfound?) ; Not aleady marked?
	    (progn
	      (setq chalfound? t) ; change mode (Don't count it in sums!)
	      (if report (format t "Challenges start at ~A.~%" k))))
      ; Otherwise...
      (progn 
	;; Add this problem to the apropos sum....
	(if chalfound?
	    (incf postchalsum)
	  (incf prechalsum))
	;; And see if it should be added to the min sums as well.
	(if (equal "Min[Count_From_Either/CLA]" ; If it's a min record...
		   (name-that-strat (find-strat-by-name (cadr (assoc 'strat rec)))))
	    ;; And, if so, again put it in the apropriate bin.
	    (if chalfound? 
		(incf postchalmin)
	      (incf prechalmin)))
	)) ; if 
    )
  (progn
    (if report
	(format t "Min stats: ~a% prechallenge, ~a% postchallange.~%"
		(if (zerop prechalsum) "none" (p2 (pconv prechalmin prechalsum)))
		(if (zerop postchalsum) "none" (p2 (pconv postchalmin prechalsum)))
		)
      )
    (if (plusp prechalsum)
	(push (pconv prechalmin prechalsum) *premin%s*))
    (if (plusp postchalsum)
	(push (pconv postchalmin postchalsum) *postmin%s*))
    )
  )

(defun report-min-stats ()
  (format t "All pre-challenge min usage: ~%  ~a~%" (p2 *premin%s*))
  (if *premin%s*
      (format t "Mean pre-challenge %min use = ~a~%"
	      (p2 (mean *premin%s*)))
    (format t "No pre-challenge min discovery data!~%"))
  (format t "All post-challenge min usage: ~%  ~a~%" (p2 *postmin%s*))
  (if *postmin%s*
      (format t "Mean post-challenge %min use = ~a~%"
	      (p2 (mean *postmin%s*)))
    (format t "No post-challenge min discovery data!~%"))
  )

;;; This version of min stat collection ignores the
;;; challenge/no-challenge break and uses a random sampling w/o
;;; replacement strategy.  We don't sample

(setq *rsamplen* 200)

(defun collect-min-stats (report
			  &aux (r* (copy-list *records*))
			       (k 0)
			       (l (length *records*))
			       (mincount 0)
			       )
  (loop until (= k *rsamplen*)
        do (let ((r (nth (random l) r*)))
	     (if (and 
		  (not (eq 'already-used (car r)))
		  (not (challenge-record? r)))
		 (progn
		   (incf k)
		   (if (equal "Min[Count_From_Either/CLA]"
			      (name-that-strat 
			       (find-strat-by-name (cadr (assoc 'strat r)))))
		    (incf mincount))
		   (setf (car r) 'already-used))
	       ))
	)
  (format t "Over ~a samples, %min=~a~%" k (p2 (pconv mincount k)))
  (push (pconv mincount k) *premin%s*)
  )
|#  

;;; This is a fairly hairy version of challenge problem analysis.  The
;;; idea is to collect stats on min use during the three principal CAM
;;; phases.  (Assuming that the cam envelope looks like: ((0 100) (100
;;; 50) (0 100) (0 9999)) That is, there is a segment (100 long in
;;; this case) of no-challenge, a segement (50) of 100% min, and then
;;; back to no min for phase 3 (100) and finally, whatever after that
;;; (usually no min for the rest).  The stats for the first three
;;; phases are kept in the 1, 2, and 3 locations of minuse (number of
;;; times mn is used in this phase) and minsum (number of problems
;;; total in this phase). (Location 0 is the pre-min stats.)

(defvar *minuse*) ; the * versions collect over multiple record sets.
(defvar *minsum*) 
(defvar *min-found-at*)

;;; Call this just at the beginning to set up the ** versions.

(defun init-min-stats ()
  (setq *minuse* ())
  (setq *minsum* ())
  (setq *min-found-at* ())
  )

(defun collect-min-stats (report
    &aux (cam-env (copy-list (cadr (assoc '*cam-env* *model-params*))))
         (phase 0)
                 ;; There might be up to 5 phases.
	 (minuse (make-array 5 :initial-element 0))
	 (minsum (make-array 5 :initial-element 0))
	 camk 
	 )
  (push nil *min-found-at*)
  (loop for r in (reverse *records*)
	as k from 0 by 1
	do (let ((min? (equal "Min[Count_From_Either/CLA]"
			      (name-that-strat 
			       (find-strat-by-name (cadr (assoc 'strat r))))))
		 (chl? (challenge-record? r)))
;	     (format t "~A: ~a phase: ~a, min? ~a, chl? ~a, camk: ~a~%" 
;		     k (cadr (assoc 'problem r))
;		     phase min? chl? camk)

             (incf (aref minsum phase)) ; count all problems

	     ;; When min is used, count it, and incremement to phase 1 if not
	     ;; there already.
	     (if min?
		 (progn
		   ;; If this is the first place that min occurred, record it.
		   (if (null (car *min-found-at*))
		       (setf (car *min-found-at*) k))
		   (incf (aref minuse phase)) ; count min used
		   (if (not camk) ;; 1+ here counters the immediate decf
		       (setq camk (1+ (second (pop cam-env))) 
			     phase 1))
		   )
	       )
	     ;; Regardless of problem type, inc the sum and see if it's time to
	     ;; switch phase.
	     (if (and camk (zerop (decf camk)))
		 (setq camk (second (pop cam-env))
		       phase (1+ phase)))
	     )
	)
  (if report (format t "minuse=~a, minsum=~a~%" minuse minsum))
  (push minuse *minuse*)
  (push minsum *minsum*)
  )

(defun report-min-stats ()
  (let ((minlocs (remove nil *min-found-at* :count 1000)))
    (format t "Mean min discovery location: ~a, stderr: ~a, range ~a - ~a~%~%"
	    (p2 (mean minlocs))
	    (p2 (standard-error minlocs))
	    (min minlocs)
	    (max minlocs)))
  (format t "Overall Per-CAM-Phase Min Usage Stats:~%")
  (summary-min-stats "Pre-Min phase" 0)
  (summary-min-stats "Pre-Chal phase" 1)
  (summary-min-stats "Chal phase" 2)
  (summary-min-stats "Post-Chal phase" 3)
  )

;;; Skip any where min was NEVER discovered!

(defun summary-min-stats (name n &aux all)
  (format t "~a: " name)
  (mapcar #'(lambda (ause asum)
	          ;; Only use ones that have a non-zerop post-chal entry.
	      (if (plusp (aref asum 3))
		  (push (/ (aref ause n) (float (aref asum n))) all)))
	  *minuse*
	  *minsum*)
  (format t "~a, ~a~%~a~%" (p2 (mean all)) (p2 (standard-error all)) (p2 all))
  )

;;; Given two patterns that match at least one result entry, this
;;; tells you how their parameters differ.

(defun cp (p1 p2)
  (let ((m1 (get-model-params-from-example-file p1))
	(m2 (get-model-params-from-example-file p2)))
    (if (equal m1 m2)
	(format t "~a = ~a~%" p1 p2)
      (progn
	(format t "          ~a             ~a~%" p1 p2)
	(loop for p1 in m1 as p2 in m2
	  if (not (equal p1 p2))
	  do (format t "~a       ~a        ~a~%"
		     (first p1)
		     (second p1)
		     (second p2))
	  )
	)
      )))

(defun get-model-params-from-example-file (p)
  (load (car (directory (format nil "results/~a" p))))
  (collect-model-params))

(defun collect-model-params ()
  (loop for param in *model-params*
    collect (list (first param) (eval (first param)))
    ))

;;; This is a postloadfn (for use in dosum :postloadfn ...) which
;;; combines certain strategies together.  It does this by smashing
;;; the strategy record entries for strategies that are intended to be
;;; combined, specifically: sum with sum/cla and scsum with scsum/cla.

(defun xlcla (&aux (k1 0) (k2 0))
  (let ((sum (find-strat-by-sname "Count_From_One_Twice[CF][Sum]"))
	(sum/cla (find-strat-by-sname "Count_From_One_Twice[CF][Sum]/CLA"))
	(scsum (find-strat-by-sname "Count_From_One_Once[Shortcut_Sum]"))
	(scsum/cla (find-strat-by-sname "Count_From_One_Once[Shortcut_Sum]/CLA"))
	)
    ;; If there was not shortcut sum strategy in the world, we need to 
    ;; create one so that things can be converted into it.
    (if (and (null scsum) scsum/cla)
	(progn
	  (format t "Faked scsum!~%")
	  (push (make-strat :name (setq scsum 'scsum)
			    :chain (deloop-uniqueify *count-from-one-once*))
		*stb*)
	  )
      )
    (dolist (r *records*)
      (let* ((s (assoc 'strat r))
	     (strat (second s)))
	(cond ((eq strat sum/cla)
	       (setf (second s) sum)
	       (incf k1))
	      ((eq strat scsum/cla)
	       (if scsum (setf (second s) scsum))
	       (incf k2))
	      )
	)
      )
    )
  (format t "~a sum/cla->sum, ~a scsum/cla->scsum~%" k1 k2)
  )

(defun find-strat-by-sname (sname)
  (let ((strat (find sname *stb* 
		     :test #'(lambda (n s)
			       (equal n (name-that-strat s))))))
    (if strat (strat-name strat))))

