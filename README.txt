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
2. RUNNING THE MODEL 
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
3. ANALYZING THE RESULTS
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
4. Changing the experimental parameters.
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
             ...

YOU MUST CHANGE THIS ON EVERY NEW EXPERIMENT OR ELSE THE ANALYZER WILL
THINK THAT YOU'RE STILL RUNNING THE LAST EXPERIMENT!

