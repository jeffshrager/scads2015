import timeit
import datetime
import os
import numpy
from random import randint, shuffle, random
from types import *

global logstream, rnet, lexicon, settings

def RoundedStr(l):
    if type(l) is ListType:
        return str(['{0:.5f}'.format(v) for v in l])
    elif type(l) is FloatType:
        return str('{0:.5f}'.format(l))
    else:
        sys.exit("Bad type sent to RoundedStr: +" + type(l))

def lispify(s):
    return (((str(s).replace(","," ")).replace("[","(")).replace("]",")")).replace("\'","\"").replace(":","=").replace("{","(").replace("}",")")

##################### SETTINGS #####################

experiment_label = "\"test\""

suppress_auto_timestamping = False

##################### GLOBAL SETTINGS #####################

ndups = 1  # Number of replicates of each combo of params -- usually 3 unless testing.
pbs = 500  # problem bin size, every pbs problems we dump the predictions

initial_weight_narrowing_divisor = 10.0 # Usually 1.0, turn up >1 to narrow initial weights closer to 0.0. 10 is somewhat arbitrary #.
 
input_one_bits = 5


anti_1_bit = -1 

dump_all_words_encodings = False # Is this is True, you get everything, otherwise, only the presentation of 1-10

n_exposures = 5000 # Problem presentation and execution

current_params = {} # These are set for a given run by the recursive param search algorithm

##################### SCANNED SETTINGS #####################

scanned_params = {
               "zeros": [0, 1],
               "results_hidden_units": [20], # 20 per experiments of 20160112b -- maybe 18?
               "results_learning_rate": [0.05], 
               "in_process_training_epochs": [1] # Number of training epochs on EACH test problem (explored 201509010826)
               }

##################### LINGUISTIC INPUT #####################
### By Myra; testing input creation for Lingustic model.

n_inputs = 10
noise_scale = 0.0001

# :-) Made this a class which will make it much simler to move the
# whole thing into your new model.

class Lexicon(object): 
    word01 = {}
    sem01 = {}

    # Init will fill the dictionary with random numbers. We don't
    # ever want to change these. Instead, copy them in the
    # noisifying process.

    def __init__(self):
        global scanned_params_keys, settings
        # MMM Add a param that says the number of bits in each input, so 
        # for param=1 you get (1=10000, 2=00100, ...) for 3 (1=10101, 2=11001,...)
        # Also, if this is something special -111 then use 10000 11000 11100 ...

        #input
        self.word01 = {}
        #new
        fmt = "{0:0"+str(10)+"b}"
        self.word02 = []

        for i in range(1025):
            s = fmt.format(i)
            if s.count('1') == 5:
                self.word02.extend([s])
        for k in range(len(self.word02)):
          self.word02[k]=[anti_1_bit if int(c) == 0 else int(c) for c in self.word02[k]]
        shuffle(self.word02)

        self.allwords = {}

        for k in range(1,11):
            self.allwords[k]=[anti_1_bit if int(c) == 0 else int(c) for c in self.word02[k-1]]
        #11 and up is WORDS
        for k in range(len(self.word02) - 10):
            self.allwords[k + 11] = self.word02[k+10]
        #print self.allwords

        for k in range(1,11):
            self.word01[k]=[anti_1_bit if int(c) == 0 else int(c) for c in self.word02[k-1]]
        #word02 includes word01

        #OUTPUT dictionary
        output_one_bits = 3 

        self.sem01={}

        #5:3 part
        fmt2 = "{0:0"+str(5)+"b}"
        v = [x for x in range(2**5)]
        r = []
        while len(r) < 10:
            n = randint(0,len(v)-1)
            s = fmt2.format(v[n])
            if s.count('1') == output_one_bits:
                r.extend([s])
                v = v[:n] + v[n+1:]
        #print r

        #5:2 part
        w = [x for x in range(2**5)]
        o = []
        while len(o) < 10:
            n = randint(0,len(w)-1)
            s = fmt2.format(w[n])
            if s.count('1') == 2:
                o.extend([s])
                w = w[:n] + w[n+1:]
        #print o

        if current_params["zeros"] == 1:
            for k in range(1,11):
                self.sem01[k]=[anti_1_bit if int(c) == 0 else int(c) for c in r[k-1]] + [anti_1_bit]*5
        else:
            for k in range(1,11):
                self.sem01[k]=[anti_1_bit if int(c) == 0 else int(c) for c in r[k-1]] + [anti_1_bit if int(c) == 0 else int(c) for c in o[k-1]]

        #random vals : rest of the output set
        self.sem02 = []
        for i in range(1025):
            s = fmt.format(i)
            firsthalf = s[:5]
            secondhalf = s[5:]
            if current_params["zeros"] == 1:
                if firsthalf.count('1') == 0 and secondhalf.count('1') == 2:
                    self.sem02.extend([s])
            else:
                if firsthalf.count('1') != 3 and secondhalf.count('1') == 2:
                    self.sem02.extend([s])
        for k in range(len(self.sem02)):
            self.sem02[k]=[anti_1_bit if int(c) == 0 else int(c) for c in self.sem02[k]]  

        #dictionary of all of them:
        self.allsem = {}
        if current_params["zeros"] == 1:
            for k in range(1,11):
                self.allsem[k]=[anti_1_bit if int(c) == 0 else int(c) for c in r[k-1]] + [anti_1_bit]*5
        else:
            for k in range(1,11):
                self.allsem[k]=[anti_1_bit if int(c) == 0 else int(c) for c in r[k-1]] + [anti_1_bit if int(c) == 0 else int(c) for c in o[k-1]]
        #11 and up is WORDS
        for k in range(len(self.sem02)):
            self.allsem[k + 11] = self.sem02[k]
        #print self.allsem
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
        r = self.allwords[a]
        return [self.noisify(r[x]) for x in range(n_inputs)] 
        
    # Figures out which correct output is closest to the one given.

class TrainingSet():
    def __init__(self, nn):
    	self.rint = randint(1, len(lexicon.allwords))
        self.input = lexicon.allwords[self.rint]

        if self.rint <= 10:
        	self.correct_output = nn.outputs[self.rint]
        else:
        	self.correct_output = nn.outputs[10+int(round(((float(len(lexicon.allsem)) - 10)/len(lexicon.allwords))*self.rint))]
        #print "d"

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
            r = (2 * numpy.random.random((layers[i - 1] + 1, layers[i] + 1)) - 1)/initial_weight_narrowing_divisor
            self.weights.append(r)

        r = (2 * numpy.random.random((layers[i] + 1, layers[i + 1])) - 1)/initial_weight_narrowing_divisor

        self.weights.append(r)

        self.X = []

        # Initial input, counting numbers
        for i in range(1, 11):
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


        if X is None: X = self.X
        if y is None: y = self.target


        ones = numpy.atleast_2d(numpy.ones(X.shape[0]))
        X = numpy.concatenate((ones.T, X), axis=1)
        for k in range(epochs):
            # Choose a random training set
            i = numpy.random.randint(X.shape[0])
            a = [X[i]]


#Q00 what is X.shape[0] what does it look like...
#mnote basically make the input the same format as addends_matrix
#and then make the output like a 14-char array start with [] and then .append the weights of the other things

            for l in range(len(self.weights)):
                dot_value = numpy.dot(a[l], self.weights[l])
                activation = self.activation(dot_value)
                a.append(activation)

            # Output layer
            error = y[i] - a[-1]


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


    # Outputs a results "probability" (more like "amplitude") matrix
    # given an input (problem) matrix; used when we want to know "what
    # is in the kid's mind"

    def predict(self, x):
        amplitude = numpy.insert(numpy.array(x), 0, numpy.ones(1).T)
        for l in range(0, len(self.weights)):
            amplitude = self.activation(numpy.dot(amplitude, self.weights[l]))
        return amplitude

    def prediction(self,n): 
        result = self.predictions[n-1]
        #print "a"

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

    def update_predictions(self):
        self.predictions = []
        #print "b"
        for n in range(1, 253):
            self.predictions.append(self.predict(lexicon.allwords[n]))

    # What target does for now is create a square matrix filled with
    # 0.5, and for the 1d matrix at y_index(a1, a2) it will have
    # everything but the correct answer be -= DECR_RIGHT/WRONG and the
    # correct answer will have INCR_RIGHT/WRONG added to it

    def reset_target(self):
        self.target = []
        self.target.append([0.0] * (self.layers[-1]))
        self.target = numpy.array(self.target)

    # This gets very ugly because in order to be generalizable
    # across different sorts of NN outputs.

    def update_target(self, input, retrieved_output, correct_output):
        self.X = []
        self.X.append(input)
        self.X = numpy.array(self.X)
        #print "correct output is HERE " + str(correct_output)
        #print self.target
        self.target = self.target.tolist()
        self.target[0]=correct_output

##################### DRIVER #####################

# Set up the neural network fitted to kids' having learned how to
# count before we got here, so there is a tendency for problems what
# look like 3+4 to result in saying 5. To do this we burn in a set of
# I/O relationships that have this tendency.

def results_network():
    nn = NeuralNetwork("Results", [10, current_params["results_hidden_units"], 10],"RETRIEVAL",lexicon.allsem)
    # Inits the NN training machine by doing a first prediction.
    return nn

# We first try a retrieval on the sum, and if that fails we have to
# use a strategy, which we try to retrieve and if that fails we choose
# a random strategy. Then we update the rnet accordingly, and fit and1
# update_y this is the main driver within driver that does the testing

def train_word():
    global rnet
    rnet.reset_target()
    trainingset = TrainingSet(rnet)
    input=trainingset.input
    correct_output=trainingset.correct_output
    retrieved_output = rnet.prediction(trainingset.rint)
    if (dump_all_words_encodings or (trainingset.rint <= 10)):
        logstream.write("(:encoding " + "(:input " + lispify(input) + ") (:correct_output " + lispify(correct_output) + ") (:rint " + lispify(trainingset.rint) + ")\n      (:retreived_output " + lispify(retrieved_output) + "))\n")
    rnet.update_target(input, retrieved_output, correct_output) 
    rnet.fit(current_params["results_learning_rate"], current_params["in_process_training_epochs"])
    rnet.update_predictions()

# UUU The open and close structure here is a mess bcs of the
# occassional dumping of tables, which I'm trying to embed at the end
# of the relevant preceeding problem block for analytical conveneince.

def present_words():
    logstream.write('(:training_block\n')
    logstream.write('   (:training\n')
    for i in range(n_exposures):

        train_word()

        if i % pbs == 0 or i == n_exposures:
            logstream.write('      ) ;; close :training\n')
            logstream.write('    ) ;; close :training-block\n')
            logstream.write('   (:training-block\n')
            logstream.write('     (:training\n')
        #print "c"

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
    global scanned_params_keys, logstream, rnet, lexicon
    if index < len(scanned_params_keys):  # Any more scanned_params_keys to scan?
        # Get the current param_values, for instance: epochs = [100,200,300]
        # 100 200 and 300 are param+values
        for param_value in scanned_params[scanned_params_keys[index]]:
            current_params[scanned_params_keys[index]] = param_value
            print ("Setting param: " + scanned_params_keys[index] + " = " + str(current_params[scanned_params_keys[index]]))
            config_and_test(index + 1)  # Next param (recursive!)
    else:
        # Finally we have a set of choices, do it:
        lexicon = Lexicon()
        fn=gen_file_name()
        print("^^^^^^^^^^^^ Above settings will log in "+ fn + " ^^^^^^^^^^^^")

        with open(fn, 'wb') as logstream:
            # initialize the logstream and neural network for each config we want to test
            logstream.write('(:log\n')
            logstream.write(' (:head\n')
            logstream.write(" (:file " + fn + ")\n")

            logstream.write(' (:output-format-version 20151103)\n')
            logstream.write(' (:problem-bin-size ' + str(pbs) + ")\n")
            logstream.write(" (:dictionaries\n")
            logstream.write("   (:input_allwords "+ lispify(lexicon.allwords) + ")\n")
            logstream.write("   (:output_allsem "+ lispify(lexicon.allsem) + "))\n")
            rnet = results_network() # Init neural net

            logstream.write(' )\n')
            logstream.write('(:run\n')

            present_words()

            logstream.write(' ) ;; Close :run\n')
            # Output params
            logstream.write(' (:params\n')
            dump_non_scanned_params()
            for key in scanned_params:
                logstream.write("  (:"+str(key)+" "+str(current_params[key])+")\n")
            logstream.write(' )\n')
            logstream.write(')\n')


def dump_non_scanned_params():
    logstream.write("  (:ndups "+str(ndups)+")\n")
    logstream.write("  (:pbs "+str(pbs)+")\n")
    logstream.write("  (:n_problems "+str(n_exposures)+")\n")
    logstream.write("  (:suppress_auto_timestamping "+str(suppress_auto_timestamping)+")\n")
    logstream.write("  (:initial_weight_narrowing_divisor "+str(initial_weight_narrowing_divisor)+")\n")
    logstream.write("  (:experiment_label "+str(experiment_label)+")\n")

#making a file - deal with this later  Q00
def gen_file_name():
    file_name = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
    full_file__name = os.path.join(os.path.join(os.path.dirname(__file__), 'runlogs'), file_name + '.lisp')
    return full_file__name

#print out the settings and timer, and then run it
def top_level_run():
    global experiment_label, scanned_params_keys, hidden_units, lexicon
    start = timeit.default_timer()
    # Used in the recursive config_and_test fn.
    scanned_params_keys=scanned_params.keys()
    if suppress_auto_timestamping is False:
        experiment_label = experiment_label[:-1] + " (n = " + str(n_exposures) + ", @" +  str(datetime.datetime.now().strftime("%Y%m%d%H%M%S")) + ")\""
    print "*** Running: " + experiment_label + "***"
    print "Scanned Parameters:"
    print str(scanned_params)
    print "-----"
    for i in range(ndups):
        print ">>>>> Rep #" + str(i + 1) + " <<<<<"
        config_and_test()
    stop = timeit.default_timer()
    print stop - start
# Run:
if __name__ == '__main__':
    top_level_run()
