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
