import timeit
import datetime
import os
import numpy
from random import randint, shuffle, random
from types import *

global settings, logstream, rnet, lexicon

def RoundedStr(l):
    if type(l) is ListType:
        return str(['{0:.5f}'.format(v) for v in l])
    elif type(l) is FloatType:
        return str('{0:.5f}'.format(l))
    else:
        sys.exit("Bad type sent to RoundedStr: +" + type(l))

def lispify(s):
    return (((str(s).replace(","," ")).replace("[","(")).replace("]",")")).replace("\'","\"").replace(":","$").replace("{","(").replace("}",")")

class TrainingSet():
    def __init__(self, n, nn):
#DDD        print ">>> TrainingSet__init__"
        self.number = n
        # FFF This will eventually get replaced by something more
        # complex that provides an input representation. For the
        # moment these are the same.
        self.input = lexicon.numberWordWithNoise(self.number)
#DDD        print "self.input : " + str(self.input)
        self.correct_output = nn.outputs[n]
#DDD        print "self.correct_output : " + str(self.correct_output)
#DDD        print ">>> TrainingSet__init__"

##################### LINGUISTIC INPUT #####################
### By Myra; testing input creation for Lingustic model.

n_inputs = 5
noise_scale = 0.05

# :-) Made this a class which will make it much simler to move the
# whole thing into your new model.

class Lexicon(object): 

      input_dictionary = {}
      output_dictionary = {}

      # Init will fill the dictionary with random numbers. We don't
      # ever want to change these. Instead, copy them in the
      # noisifying process.

      def __init__(self):
#DDD          print ">>> Lexicon_init_"
          # MMM Add a param that says the number of bits in each input, so 
          # for param=1 you get (1=10000, 2=00100, ...) for 3 (1=10101, 2=11001,...)
          # Also, if this is something special -999 then use 10000 11000 11100 ...
          for i in range(1,n_inputs+1):
            self.input_dictionary[i] = [randint(0, 1) for x in range(n_inputs)]
          print "self.input_dictionary : " + str(self.input_dictionary)
          # MMM Add a param that says the number of bits in each output, so 
          # for param=1 you get (1=10000, 2=00100, ...) for 3 (1=10101, 2=11001,...)
          # Also, if this is something special -999 then use 10000 11000 11100 ...
          self.output_dictionary = {k:v for k, v in [[x,[1 for b in range(1,x+1)]+[0 for b in range(x,5)]] for x in range(1,6)]}
          print "self.output_dictionary : " + str(self.output_dictionary)
#DDD          print "<<< Lexicon_init_"

      # I'll get called over and over in a map over the list of values.
      def noisify(self,v):
          noise = numpy.random.normal(loc=0.0, scale=noise_scale)
            #scale is SD
            #absolute value of noise? since no negative values
          if v == 0:
              return (v + abs(noise))
          else:
              return (v - abs(noise))

      # This is the main function that a user will call. It just
      # creates a COPY of the representation, with noise.
      def numberWordWithNoise(self,a): 
          r = self.input_dictionary[a]
          return [self.noisify(r[x]) for x in range(n_inputs)] 

      # Figures out which correct output is closest to the one given.
      def scoresub1(self,i,o):
          sum = 0
          for p in range(len(i)):
              sum += o[p]-i[p]
          return sum

      def score(self,nn_output):
          minn = -999
          mins = 999
          r = [[number,self.scoresub1(target_output,nn_output)] for number, target_output in dict.iteritems(self.output_dictionary)]
          for n,s in r:
              if s<mins:
                  mins=s
                  minn=n
          return minn

##################### SETTINGS #####################

class Settings:

    # PART 1: These usually DON'T change:
    ndups = 1  # Number of replicates of each combo of params -- usually 3 unless testing.
    pbs = 50  # problem bin size, every pbs problems we dump the predictions
    
    def param(self, key):
        return self.params[key]

    params = {} # These are set for a given run by the recursive param search algorithm

#change the experiment label below!
    param_specs = {"experiment_label": ["\"test 20160625b\""],

                 # Problem presentation and execution
                 "n_exposures": [3000],

                 # Learning target params
                 "output_one_bits": [1,3,-999], # If -999 then uses 10000,11000, etc
                 "input_one_bits": [1,3,-999], # If -999 then uses 10000,11000, etc

                 "results_hidden_units": [4,6,8], 
                 "non_result_y_filler": [0.0], 
                 "results_learning_rate": [0.01,0.05,0.1,0.15,0.2], 
                 "in_process_training_epochs": [1] 
                 }

##################### NN #####################

# The fns addends_matrix and sum_matrix create the input and output
# arrays that get appened up into training matrices by the caller (in
# driver).
#
# Transform two addends into a distributed representation input array,
# e.g., for a representation of 3 + 4, the input array created by
# addends_matrix) is:
#
# Index:         0 , 1 ,   2 , 3 , 4   , 5 , 6 | 7 , 8 , 9 , 10  , 11 , 12 , 13]
# Input array: [ 0 , 0 , 0.5 , 1 , 0.5 , 0 , 0 | 0 , 0 , 0 ,  0.5,  1 ,  0.5, 0]
#                              ^=3                                  ^=4
#                                (0 is NOT a possible input!)
#
# The surrounding 0.5s are supposed to represent children's confusion
# about the number actually stated. (Or about how to xform the stated
# number into the exact internal representation). Note that there are
# 7 elements per input bcs the 1 and 5 both need to allow for a left
# and right surround respectively. The exact value of the surrounding
# inputs is controlled by addends_matrix_offby1_delta.
#
# And the output array (created by sum_matrix) is:
#
# Index:         0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12
# Output array:[ 0 , 0 , 0 , 0 , 0 , 0 , 0 , 1 , 0 , 0 ,  0 ,  0 ,  0 ]
#                                            ^=7 
#                ^ 0 IS a possible answer!
#
# WWW WARNING !!! Don't confuse either of these with the fingers on
# the hands!

class NeuralNetwork:
    def __init__(self, name, layers, type, outputs):
        self.outputs = outputs
        self.name=name
        self.activation = lambda x: numpy.tanh(x)
        self.activation_prime = lambda x: 1.0 - x**2

        # The layers is a vector giving the number of nodes in each
        # layer. The first (0th) is assumed to be the input and the
        # last the output layer.

        # ?????????????????????????????????????????????????????
        # ??? WWW It looks like we're getting one extra unit in
        # the hidden layer. Is there an obiwan error here?? WWW
        # ?????????????????????????????????????????????????????

        self.layers=layers

        # Set weights
        self.weights = []
        self.target = []

        # range of weight values (-1,1)
        # input and hidden layers - random((2+1, 2+1)) : 3 x 3

        for i in range(1, len(layers) - 1):
            r = 2 * numpy.random.random((layers[i - 1] + 1, layers[i] + 1)) - 1
            self.weights.append(r)

        r = 2 * numpy.random.random((layers[i] + 1, layers[i + 1])) - 1

        self.weights.append(r)

        self.X = []

        # Initial input, counting numbers
        for i in range(1, 6):
                self.X.append(lexicon.numberWordWithNoise(i))
        self.X = numpy.array(self.X)
        self.predictions = []

        X_count = []
        y_count = []
        self.update_predictions()

    # The output array from the NN is created by .append(ing) a bunch of
    # probe results, something like this: 
    #
    #    for i in range(1,6):
    #        for j in range (1,6):
    #          ...generate the i+j output array and append to the growing vector...
    #
    # What results is a long array where the index of the problem we're looking for 
    # is position: 5 * (i - 1) * (j - 1). For example, for 3+4 you end up with 
    # 5 * 2 * 3 = position 30 in the output units array. 
        
    # Main forward feed and backpropagation
    def fit(self, learning_rate, epochs, X=None, y=None):

#DDD        print ">>> Fit(learning_rate="+str()+", epochs="+str(epochs)+")"

        if X is None: X = self.X
        if y is None: y = self.target

#DDD        print "self.target : " + str(self.target)

        ones = numpy.atleast_2d(numpy.ones(X.shape[0]))
        X = numpy.concatenate((ones.T, X), axis=1)
        for k in range(epochs):
            # Choose a random training set
            i = numpy.random.randint(X.shape[0])
            a = [X[i]]

#DDD            print "Input:" 
#DDD            print "X : " + str( X)
#DDD            print "i : " + str( i)
#DDD            print "X[i] : " + str( X[i])
#DDD            print "y : " + str( y)
#DDD            print "a : " + str( a)

#Q00 what is X.shape[0] what does it look like...
#mnote basically make the input the same format as addends_matrix
#and then make the output like a 14-char array start with [] and then .append the weights of the other things

            for l in range(len(self.weights)):
                dot_value = numpy.dot(a[l], self.weights[l])
                activation = self.activation(dot_value)
                a.append(activation)

            # Output layer
            error = y[i] - a[-1]

#DDD            print "y : " + str(y)
#DDD            print "a : " + str(a)
#DDD            print "i : " + str(i)
#DDD            print "y[i] : " + str(y[i])
#DDD            print "a[-1] : " + str(a[-1])
#DDD            print "error : " + str(error)

            deltas = [error * self.activation_prime(a[-1])]

            # We need to begin at the second to last layer 
            # (a layer before the output layer)
            for l in range(len(a) - 2, 0, -1):
                deltas.append(deltas[-1].dot(self.weights[l].T) * self.activation_prime(a[l]))

            # [level3(output)->level2(hidden)]  => [level2(hidden)->level3(output)]
            deltas.reverse()

            # backpropagation
            # 1. Multiply its output delta and input activation 
            #    to get the gradient of the weight.
            # 2. Subtract a ratio (percentage) of the gradient from the weight.

            for i in range(len(self.weights)):
                layer = numpy.atleast_2d(a[i])
                delta = numpy.atleast_2d(deltas[i])
                self.weights[i] += learning_rate * layer.T.dot(delta)

#DDD            print "<<< Fit"

    # Outputs a results "probability" (more like "amplitude") matrix
    # given an input (problem) matrix; used when we want to know "what
    # is in the kid's mind"

    def predict(self, x):
#DDD        print ">>> predict("+str(x)+")"
        amplitude = numpy.insert(numpy.array(x), 0, numpy.ones(1).T)
#DDD        print "amplitude : " + str(amplitude)
        for l in range(0, len(self.weights)):
            amplitude = self.activation(numpy.dot(amplitude, self.weights[l]))
#DDD        print "final amplitude : " + str(amplitude)
#DDD        print "<<< predict"
        return amplitude

    def wan2lnp(self,word,n): # word_and_number_to_localist_number_pattern
#DDD        print ">>> wan2lnp(n="+str(n)+", word="+str(word)+")"
#DDD        print "n-1 : " + str(n-1)
        result = self.predictions[n-1]
#DDD        print "result : " + str( result)
        return result

    # Returns a function that picks a random result from a list of
    # results above the confidence criterion this is used for the
    # retrieval. when we want to try to retrieve a sum, for example 3
    # + 4 = 7, we pass in a1 = 3, a2 = 4. It looks to see the values
    # that are above the cc, and chooses a random number from those
    # values. if there are none, it returns None. 

    # Used for analysis output, this just gets the prediction values
    # for a particular sum. FFF Maybe this could be used inside guess?
    # FFF Anyway, see notes for guess to explain the begin and end
    # things.

    #explain this? Q00 what is guess vector and what is theh decr_right/wrong stuff

    def guess_vector(self, n, beg, end):
#DDD        print ">>> guess_vector(n="+str(n)+", beg="+str(beg)+", end="+str(end)+")"
        vec = []
#DDD        print "self.predictions : " + str( self.predictions)
        self.predict(lexicon.numberWordWithNoise(n))
#DDD        print "self.predictions : " + str( self.predictions)
        for i in range(beg, end):
#DDD            print "i : " + str(i)
#DDD            print "n-1 : " + str(n-1)
            vec.append(round(self.predictions[n][i], 5))
#DDD        print "vec : " + str(vec)
#DDD        print "<<< guess_vector"
        return (vec)

    def update_predictions(self):
#DDD        print ">>> update_predictions"
        self.predictions = []
        for n in range(1, 6):
            self.predictions.append(self.predict(lexicon.numberWordWithNoise(n)))
#DDD        print "<<< update_predictions"

    # What target does for now is create a square matrix filled with
    # 0.5, and for the 1d matrix at y_index(a1, a2) it will have
    # everything but the correct answer be -= DECR_RIGHT/WRONG and the
    # correct answer will have INCR_RIGHT/WRONG added to it

    def reset_target(self):
#DDD        print ">>> reset_target"
        self.target = []
        self.target.append([settings.param("non_result_y_filler")] * (self.layers[-1]))
        self.target = numpy.array(self.target)
#DDD        print "self.target : " + str(self.target)
#DDD        print "<<< reset_target"

    # This gets very ugly because in order to be generalizable
    # across different sorts of NN outputs.

    def update_target(self, input, retrieved_output, correct_output):
#DDD        print ">>> update_target(n="+str(input)+", retrieved_output="+str(retrieved_output)+", correct_output="+str(correct_output)+")"
        self.X = []
        self.X.append(input)
        self.X = numpy.array(self.X)
#DDD        print "self.X: " + str(self.X)
        self.target[0]=correct_output
#DDD        print "self.target: " + str(self.target)

#DDD        print "<<< update_target"

##################### DRIVER #####################

# Set up the neural network fitted to kids' having learned how to
# count before we got here, so there is a tendency for problems what
# look like 3+4 to result in saying 5. To do this we burn in a set of
# I/O relationships that have this tendency.

def results_network():
    nn = NeuralNetwork("Results", [5, settings.param("results_hidden_units"), 5],"RETRIEVAL",lexicon.output_dictionary)
    # Inits the NN training machine by doing a first prediction.
    return nn

# We first try a retrieval on the sum, and if that fails we have to
# use a strategy, which we try to retrieve and if that fails we choose
# a random strategy. Then we update the rnet accordingly, and fit and
# update_y this is the main driver within driver that does the testing

def train_word():
#DDD    print ">>> train_word"
    global rnet
    rnet.reset_target()
    trainingset = TrainingSet(randint(1, 5), rnet)
    number=trainingset.number
    input=trainingset.input
    correct_output=trainingset.correct_output
#DDD    print "number : " + str(number)
#DDD    print "input : " + str(input)
#DDD    print "correct_output : " + str(correct_output)
    retrieved_output = rnet.wan2lnp(input,number)
#DDD    print "retrieved_output : " + str(retrieved_output)
    minn = lexicon.score(retrieved_output)
    if minn == number:
        rw = ":+right+"
    else:
        rw = ":-wrong-"
    logstream.write("(:encoding " + " " + rw + " (" +  str(number) + " => " +str(minn)+") ((" + lispify(input) + ") => " + lispify(retrieved_output) + ")) ")
    rnet.update_target(input, retrieved_output, correct_output) 
    rnet.fit(settings.param("results_learning_rate"), settings.param("in_process_training_epochs"))
    rnet.update_predictions()
#DDD    print "<<< train_word"

# UUU The open and close structure here is a mess bcs of the
# occassional dumping of tables, which I'm trying to embed at the end
# of the relevant preceeding problem block for analytical conveneince.

def present_words():
    logstream.write('(:training_block\n')
    logstream.write('   (:training\n')
    for i in range(settings.param("n_exposures")):
        logstream.write('(')
        train_word()
        logstream.write(')\n')
        if i % settings.pbs == 0 or i == settings.param("n_exposures"):
            logstream.write('      ) ;; close :training\n')
            logstream.write('    ) ;; close :training-block\n')
            logstream.write('   (:training-block\n')
            logstream.write('     (:training\n')
    logstream.write('   ) ;; close :training\n')
    logstream.write('    ) ;; close :training-block\n') # Warning! We may get an extra one of these!           

# Execute with all the possible values of each parameter. This is a
# weird recursive function. The top half that calls itself repeatedly
# until it has chosen and set the next value for each paramter. Then
# it drops into the second half, which actually runs with whatever the
# latest set of parameters is. The state of the recursion holds the
# state of the parameter scan. (For slight efficiency this uses a
# quasi-global called param_specs_keys and gets set in the caller.)

def config_and_test(index=0):
    global param_specs_keys, logstream, rnet
    if index < len(param_specs_keys):  # Any more param_specs_keys to scan?
        # Get the current param_values, for instance: epochs = [100,200,300]
        # 100 200 and 300 are param+values
        for param_value in settings.param_specs[param_specs_keys[index]]:
            settings.params[param_specs_keys[index]] = param_value
            print ("Setting param: " + param_specs_keys[index] + " = " + str(settings.params[param_specs_keys[index]]))
            config_and_test(index + 1)  # Next param (recursive!)
    else:
        # Finally we have a set of choices, do it:
        fn=gen_file_name()
        print("^^^^^^^^^^^^ Above settings will log in "+ fn + " ^^^^^^^^^^^^")
        with open(fn, 'wb') as logstream:
            # initialize the logstream and neural network for each config we want to test
            logstream.write('(:log\n')
            logstream.write(' (:head\n')
            logstream.write(" (:file " + fn + ")\n")
            logstream.write(' (:output-format-version 20151103)\n')
            logstream.write(' (:problem-bin-size ' + str(settings.pbs) + ")\n")
            logstream.write(" (:dictionaries\n")
            logstream.write("   (:input "+ lispify(lexicon.input_dictionary) + ")\n")
            logstream.write("   (:output "+ lispify(lexicon.output_dictionary) + "))\n")
            rnet = results_network() # Init neural net
            logstream.write(' )\n')
            logstream.write('(:run\n')
            present_words()
            logstream.write(' ) ;; Close :run\n')
            # Output params
            logstream.write(' (:params\n')
            for key in settings.param_specs:
                logstream.write("  (:"+str(key)+" "+str(settings.param(key))+")\n")
            logstream.write(' )\n')
            logstream.write(')\n')

#making a file - deal with this later  Q00
def gen_file_name():
    file_name = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
    full_file__name = os.path.join(os.path.join(os.path.dirname(__file__), 'runlogs'), file_name + '.lisp')
    return full_file__name

#print out the settings and timer, and then run it
def top_level_run():
    global param_specs_keys, settings, hidden_units, lexicon
    start = timeit.default_timer()
    # Used in the recursive config_and_test fn.
    settings = Settings()  
    lexicon = Lexicon()
    param_specs_keys=settings.param_specs.keys()
    print "Parameter spec :" + str(settings.param_specs)
    for i in range(settings.ndups):
        print ">>>>> Rep #" + str(i + 1) + " <<<<<"
        config_and_test()
    stop = timeit.default_timer()
    print stop - start

# Run:
if __name__ == '__main__':
    top_level_run()
