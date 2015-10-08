============================================================
1. GETTING THE CODE
============================================================

You generally get the code by cloning it from github. There are a
bunch of tolls that do this, but the easiest thing to do is to go to
github, create a (free) account, and then go to this page:

  https://github.com/jeffshrager/scads2015

About halfway down the righthand side of the page you'll see a silver
button labeled "Clone in Desktop". Click it and point it to wherever
you want to put the model code (usally a brand new folder). It should
dump all the model code there, just waiting for you to run it.

============================================================
2. SETTING UP TO RUN THE MODEL 
============================================================

The model is written in Python, and the analyzer is written in Common
Lisp. So you'll need Python, numPy (a numeric python library), and a
common lisp (to run the analyzer).

1.1 GETTING PYTHON

Instructions to install python are here:

  https://www.python.org/downloads/

I happen to be on version 2.7.10, but it should work on later Pythons.

Instruction to install numPy are here:

  http://docs.scipy.org/doc/numpy/user/install.html

You're likely to run into version compatibility problems, so make sure
that you install the numpy that is compatible with your python version.

If any of this doesn't work smoothly, see this flowchart for help in
trouble-shooting:

  https://xkcd.com/627/

Once you have a compatible python and numpy you should be able to just do:

  python settings.py

That will run that latest settings. You'll want to change the settings
to run your own experiments. More below on how to do this.

============================================================
3. SETTING UP TO RUN THE ANALYZER
============================================================

This is the hairiest part! 

3.1 GETTING A COMMON LISP

I use Clozure Common Lisp (ccl) which you can download here:

  http://ccl.clozure.com/

Lisps usually don't have version problems, but in case you want to
check, I'm using Version 1.5-r13651 (DarwinX8632).

Usually I do the model runs and the analysis in different separate
shell windows, but you can do them in the same one; it just requires a
little more restarting of things.

The way that you run the analyzer is to start up lisp by executing
whatever is the appropraite binary for your lisp, and then entering
this expression into the lisp listener (the thing that you get when
you start lisp -- often the prompt is ">" or "?"):

  (load (compile-file "analyzer.lisp"))

If everything is going right, this should automatically start to
analyze your lastest experiment, in which case you'll see something
like this:

  ? (load (compile-file "analyzer.lisp"))
  ;Compiler warnings for "analyzer.lisp" :
  ;   In ANALYZE-LOAD-DATA: Unused lexical variable LAST-FNO
  ;   In ANALYZE-SUMMARIZE-COEFS: Unused lexical variable FILE............................................................................................................................................................................................................................................................
  Loading /Users/jeffshrager/Desktop/etc/scads2015/test_csv/20151008103900.csv
  WARNING: In parse-20150807-data, rnnpt seems to be followed by log entries incorrectly. These will be lost!
  Loading /Users/jeffshrager/Desktop/etc/scads2015/test_csv/20151008103921.csv
  WARNING: In parse-20150807-data, rnnpt seems to be followed by log entries incorrectly. These will be lost!
  "sumstats/3653319440-20151008103900-logsummary.xls" 
  "sumstats/3653319440-20151008103921-logsummary.xls" 
  "sumstats/3653319440-mastersummary.xls" 
  "sumstats/3653319440-FinalPivotforR.csv" 
  #P"/Users/jeffshrager/Desktop/etc/scads2015/analyzer.dx32fsl"
  ? 
  
First off, ignore the compiler warnings -- they're only warnings,
afterall!

The long strong of dots is just to tell you that it's running. It
prints one dot every time it examines a results file. Don't worry if
you think that you only created a small number of results files, but
it's printing hundreds of dots; it looks at EVERY results file
regardless of whether it's part of the latest run. (I could improve on
this...someday.)

Next it should load your specific results. (In the case above there are 2 results files).

If you see this specific warning: 

    WARNING: In (my results file), rnnpt seems to be followed by log entries incorrectly. These will be lost!

again, don't worry about that. It actually always happens (and,
anyway, again, it's just a warning).

If all went well you'll see a bunch of results file names, as:

  "sumstats/3653319440-20151008103900-logsummary.xls" 
  "sumstats/3653319440-20151008103921-logsummary.xls" 
  "sumstats/3653319440-mastersummary.xls" 
  "sumstats/3653319440-FinalPivotforR.csv" 
  #P"/Users/jeffshrager/Desktop/etc/scads2015/analyzer.dx32fsl"

These are the results of the runs combined and output in a (somewhat)
convenient format for excel or R analysis. We'll get to looking at
what's in those later. Again, if all went well you'll end up back at
the lisp prompt, in this case a "?". 

At this point I usually just leave the lisp thing there because you
can just re-run the analyzer over and over on new results from the
same lisp by just going back to this step:

  (load (compile-file "analyzer.lisp"))

This is why I do the runs and the analysis in different shell windows;
because I can just go back and forth, trying different parameters, and
then analyzing them.

============================================================
4. SETTING EXPERIMENTAL PARAMETERS AND RUNNING EXPERIMENTS
============================================================

The model parameters are in the file 'settings.py'. There are lots of
them, and to understand them requires understanding the model, so I'm
only going to talk about the couple that change most often, just to
get you going.

The most important parameter is:

   settings.experiment_label

which gets set in the second line of PART 3 (commonly changing
             parameters), as: 

scan_spec = {"settings.experiment_label": 
             ["\"20151008b: Exploring distributed rep of inputs longer\""],
             ...etc...

WARNING: YOU MUST CHANGE THIS ON EVERY NEW EXPERIMENT OR ELSE THE
ANALYZER WILL THINK THAT YOU'RE STILL RUNNING THE LAST EXPERIMENT!

You can see that I usually use a date followed by some reminder of
what the experiment is about.

WARNING: ONLY CHANGE THE DATE AND TEXT PARTS OF THIS ENTRY. IF YOU
MONKEY WITH THE QUOTES AND BACKSLASHES, QUOTES, BRACKETS, OR ANY OTHER
PUNCTUATION, YOU ARE QUOTE LIKELY TO MESS UP EVERYTHING!

Okay, so now you can setup the parameters that you'd like to
explore. These are the rest of the entries after the experiment label.

Suppose, for example, you want to explore different learning rates in
the main learning part of the model. Here's the relevant parameter:

               "settings.learning_rate": [0.1], # Explored 201509010826

The part after the # is just a comment. Please leave these in
place. Right now it's only trying one learning rate, but if you wanted
to try a range, you change that entry to something like:

               "settings.learning_rate": [0.1,0.25,0.5,0.75,1.0], # Explored 201509010826

When you run this (AND REMEMEBR TO CHANGE "settings.experiment_label"
AS WELL!!) it will do a series of runs with five different learning
rates. It's actually a bit subtle to understand how many runs it's
actually going to do. For example, suppose that we change the learning
rate, as above, and also do this:

                "settings.PERR": [0.0,0.1,0.2], # 0.1 confirmed 201509010826

What you'll actually get is 5x3=15 runs with each combination of
parameters, as:

    lr=0.1, perr=0.0
    lr=0.1, perr=0.1
    lr=0.1, perr=0.2
    lr=0.25, perr=0.0
    lr=0.25, perr=0.1
    lr=0.25, perr=0.2
    ...etc...

And, actually, you're probably going to get 15x3=45 separate runs. Why
the last 3x? Beacuse there's a parameter in the PART 1 (mostly don't
change) called:

    ndups = 3

This says: For every combination of parameters (15 in the above
example), do this number (3 here) copies in order to get explore the
variance. Although you can change this number, you usually don't want
to. Unless you're doing something that has a lot more variance than
this model usually has, 3 copies is usually enough.

If you haven't made any mistakes, when you run the model it'll look sometihng like this:

  python settings.py
  Parameter spec:
  {'settings.DECR_on_WRONG': [-1.0], 'settings.non_result_y_filler': [0.0], 'settings.initial_counting_network_burn_in_epochs': [1000], 'settings.DR_threshold': [1.0], 'settings.initial_counting_network_learning_rate': [0.25], 'settings.experiment_label': ['"20151008b: Exploring distributed rep of inputs longer"'], 'settings.RETRIEVAL_HIGH_CC': [1.0], 'settings.INCR_the_right_answer_on_WRONG': [0.0], 'settings.STRATEGY_LOW_CC': [0.6], 'settings.STRATEGY_HIGH_CC': [1.0], 'settings.addend_matrix_offby1_delta': [0.0, 1.0], 'settings.RETRIEVAL_LOW_CC': [0.6], 'settings.PERR': [0.0], 'settings.learning_rate': [0.1], 'settings.in_process_training_epochs': [10], 'settings.INCR_on_RIGHT': [1.0], 'settings.n_problems': [10000]}
  Strategies in play:
  [<function count_from_either_strategy at 0x1066678c0>, <function count_from_one_once_strategy at 0x106667848>, <function count_from_one_twice_strategy at 0x1066677d0>, <function min_strategy at 0x106667938>]
  -----
  >>>>> Rep #1 <<<<<
  settings.DECR_on_WRONG=-1.0
  settings.non_result_y_filler=0.0
  settings.initial_counting_network_burn_in_epochs=1000
  settings.DR_threshold=1.0
  settings.initial_counting_network_learning_rate=0.25
  settings.experiment_label="20151008b: Exploring distributed rep of inputs longer"
  settings.RETRIEVAL_HIGH_CC=1.0
  settings.INCR_the_right_answer_on_WRONG=0.0
  settings.STRATEGY_LOW_CC=0.6
  settings.STRATEGY_HIGH_CC=1.0
  settings.addend_matrix_offby1_delta=0.0
  settings.RETRIEVAL_LOW_CC=0.6
  settings.PERR=0.0
  settings.learning_rate=0.1
  settings.in_process_training_epochs=10
  settings.INCR_on_RIGHT=1.0
  settings.n_problems=10000
  ---Running!---
  settings.addend_matrix_offby1_delta=1.0
  settings.RETRIEVAL_LOW_CC=0.6
  settings.PERR=0.0
  settings.learning_rate=0.1
  settings.in_process_training_epochs=10
  settings.INCR_on_RIGHT=1.0
  settings.n_problems=10000
  ---Running!---
  40.8306379318
  
The number at the end is how many seconds it took to run (to a stupid
number of decimal places!) Generally you can ignore the rest of that
output; it's just telling you what each run is doing.

On to analysis!

============================================================
5. RUNNING THE ANALYZER AND UNDERSTANDING ITS OUTPUT
============================================================

So, let's assume that you did everything right to this point, and you
have your results. The model leaves the results in a bunch of
uninterpretable log files in the folder "test_csv", and each has a
name something like: "20150901083953.csv".  You can't read these
files.  (These actually aren't csv files at all, so don't try to load
them into excel! The name is historical.) Or, more precisely, you can
read them, but they aren't very useful because the look something like
this:

  ===== Results NN Prediction table ======
  1 + 1 = ,-0.94736,-0.51531,-0.75688,0.78067,0.35831,-0.25817,0.90539,0.87823,-0.96181,0.90321,-0.91011,-0.10427,0.37619
  1 + 2 = ,-0.03809,0.02957,-0.00055,0.9678,-0.01931,0.1135,0.08834,0.04505,-0.0453,-0.1637,-0.15963,-0.00512,0.19583
  1 + 3 = ,-0.94297,-0.94116,-0.89555,0.97445,0.91844,0.19546,-0.87282,0.37504,0.11806,0.743,-0.74497,0.45006,-0.73808
  1 + 4 = ,-0.88432,-0.85154,-0.46725,0.95839,0.69538,0.75996,-0.05457,0.87203,-0.96324,0.61515,-0.78728,-0.0753,0.64213
  1 + 5 = ,-0.99732,0.11577,0.31596,0.63662,0.88873,0.98953,0.49335,0.69592,-0.40726,0.75955,-0.84397,0.77331,-0.84182
  2 + 1 = ,0.63596,0.91249,0.17819,-0.92299,0.13883,-0.60389,0.99081,0.59355,-0.99248,0.81786,-0.93197,0.23795,0.14804
  2 + 2 = ,0.97492,0.95946,0.80124,-0.80879,-0.51657,-0.30644,0.979,-0.43619,-0.52933,-0.13922,0.28631,-0.26487,-0.44761
  2 + 3 = ,-0.11117,-0.07066,0.0144,0.07968,0.98442,-0.02999,0.21248,0.11121,0.06283,-0.243,-0.19636,-0.08814,0.21674
  2 + 4 = ,0.1145,0.56928,0.84556,-0.64206,0.90219,0.92597,0.91012,0.53498,-0.97374,0.25509,-0.28654,-0.52197,0.60172
  2 + 5 = ,-0.22987,0.82395,0.81223,-0.97562,0.92724,0.8864,0.98808,0.62049,-0.9227,0.19753,-0.92241,0.62781,-0.96522
  3 + 1 = ,0.42205,0.51979,-0.89889,-0.75238,-0.87515,0.39955,0.50156,0.00426,-0.99804,0.91135,-0.83532,0.64842,-0.39885
  3 + 2 = ,0.96285,0.64546,-0.16584,-0.5671,-0.86125,0.27682,0.34562,-0.70598,-0.87967,-0.34029,0.34587,0.46793,-0.48249
  3 + 3 = ,-0.16345,-0.65539,-0.83469,-0.04848,0.55414,0.89201,-0.50664,-0.37347,-0.85671,-0.3275,0.01539,0.55468,-0.36052
  3 + 4 = ,0.01609,-0.18422,-0.09473,-0.04524,0.05326,0.98853,0.1046,0.19556,-0.99396,-0.12983,-0.19071,-0.13354,0.18859
  3 + 5 = ,-0.90002,0.65182,-0.04279,-0.90406,-0.18855,0.9955,0.47724,-0.23923,-0.9566,0.08815,-0.72238,0.95445,-0.98044
  4 + 1 = ,-0.79576,-0.16818,-0.93154,-0.01069,-0.16036,0.62385,0.97932,0.09866,-0.91856,0.72615,0.11408,-0.35783,0.86635
  4 + 2 = ,-0.4345,0.54717,-0.03088,0.71844,-0.90543,0.80146,0.91678,-0.90194,0.08194,-0.65133,0.97916,-0.45084,0.84913
  4 + 3 = ,-0.94062,-0.8952,-0.9719,0.94022,0.19307,0.94568,0.1324,-0.5228,-0.23937,-0.00552,0.06717,0.18191,0.66727
  4 + 4 = ,-0.21914,0.2466,-0.70217,0.53662,0.02181,0.99304,0.89825,-0.31982,-0.98593,0.66595,-0.02204,-0.40195,0.57722
  4 + 5 = ,-0.99856,-0.01996,-0.10475,0.18742,0.11638,0.99963,0.97524,-0.04678,0.02938,-0.0914,-0.03616,0.06149,0.1247
  5 + 1 = ,-0.30536,0.85003,0.67446,-0.08248,-0.89182,0.19834,0.75446,0.79879,-0.99427,0.93022,-0.96969,0.53587,-0.43215
  5 + 2 = ,0.90085,0.97698,0.98164,0.02435,-0.98893,0.30158,0.15771,-0.42223,-0.63524,0.56261,-0.02544,0.03584,-0.42201
  5 + 3 = ,-0.55113,0.03448,0.84952,0.94058,0.31125,0.82463,-0.38179,0.77611,-0.21422,0.68469,-0.92501,-0.39707,-0.24894
  5 + 4 = ,-0.15115,0.8423,0.98442,0.87987,-0.43119,0.99194,0.58235,0.84274,-0.9851,0.72414,-0.73169,-0.53074,0.55956
  5 + 5 = ,-0.92647,0.89376,0.92561,-0.27178,-0.1798,0.98833,0.40116,0.58261,-0.93369,0.82498,-0.92141,0.85119,-0.93388
  ========================================
  

The analyzer changes these into summary files that really are csv
files and can actually be read into excel or R and used to understand
what's going on. These results are left in the folder called
"sumstats", and here are the files typically left there from an
analysis:

       3653319440-20151008103900-logsummary.xls
       3653319440-20151008103921-logsummary.xls
       3653319440-FinalPivotforR.csv
       3653319440-mastersummary.xls

The first part of this is just a sequence number that identifies this
analysis. It's essentially random, although it increases over
time. (For the Lisparati among us, it's just the result of a call to
(get-universal-time). See, you don't care, do you!)

The file that you're most interested in are the mastersummary and
FinalPivotforR files.
