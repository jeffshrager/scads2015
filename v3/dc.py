#to do : figure out inputs, Q00's, change the addends_matrix accordingly
import timeit
import datetime
import os
import numpy
from random import randint, shuffle, random
from types import *

global settings, logstream, rnet, snet

def RoundedStr(l):
    if type(l) is ListType:
        return str(['{0:.5f}'.format(v) for v in l])
    elif type(l) is FloatType:
        return str('{0:.5f}'.format(l))
    else:
        sys.exit("Bad type sent to RoundedStr: +" + type(l))

def lispify(s):
    return (((str(s).replace(","," ")).replace("[","(")).replace("]",")")).replace("\'","\"")

class Addend(object):
    def __init__(self, ad1, ad2):
        self.ad1 = ad1
        self.ad2 = ad2
        self.addend = 0
        self.cla = ''

#this is the inputs
def PPA():
    global ADDENDS
    ADDENDS = Addend(randint(1, 5), randint(1, 5))

##################### input for linguistic model #####################

### :-) These params would end up in the setting class in scads, so
### they're just global here.

n_inputs = 5
noise_scale = 0.05

# :-) Made this a class which will make it much simler to move the
# whole thing into your new model.

class lexical_inputs(object): 

    # :-) The dictionary is a local to the lexical_inputs object.
      input_dictionary = {}

      # :-) Init will fill the dictionary with random numbers. We
      # don't ever want to change these. Instead, copy them in the
      # noisifying process.
      def __init__(self):
          for i in range(1,n_inputs+1):
            self.input_dictionary[i] = [randint(0, 1) for x in range(n_inputs)] # :-) Used a fancy comprehension here

      # :-) This just noisifies a single value at a time. I'll get called
      # over and over in a map over the list of values.
      def noisify(self,v):
          noise = numpy.random.normal(loc=0.0, scale=noise_scale)
            #scale is SD
            #absolute value of noise? since no negative values
          if v == 0:
              return (v + abs(noise))
          else:
              return (v - abs(noise))

      # :-) This is the main function that a user will call. It just
      # creates a COPY of the representation, with noise.
      def addendWithNoise(self,a): 
          r = self.input_dictionary[a]
          return [self.noisify(r[x]) for x in range(n_inputs)] # :-) Comprehension again!
      
              
### Just for display purposes, don't need all those decimal places.
def Rstr(l):
    return str(['{0:.5f}'.format(v) for v in l])

### Testing

indict = lexical_inputs() # :-) Init the dictionary

print("Dictionary:")

for k, v in indict.input_dictionary.items():
        print(str(k) + " : " + Rstr(v))

print("Expample addition inputs:")

for i in range(10):
    a1 = randint(1,5)
    a2 = randint(1,5)
    print(str(a1) + ":" + Rstr(indict.addendWithNoise(a1)) + "+" + str(a2) + ":" + Rstr(indict.addendWithNoise(a2)))



##################### SETTINGS #####################

class Settings:

    # PART 1: These usually DON'T change:
    ndups = 3  # Number of replicates of each combo of params -- usually 3 unless testing.
    pbs = 50  # problem bin size, every pbs problems we dump the predictions
    dump_hidden_activations = False
    
    def param(self, key):
        return self.params[key]

    params = {} # These are set for a given run by the recursive param search algorithm

#change the experiment label below!
    param_specs = {"experiment_label": ["\"testing 061516\""],


                 # Setting up the initial counting network
                 "initial_counting_network_burn_in_epochs": [0,2000], # 1000 based on 201509010902
                 "initial_counting_network_learning_rate": [0.25], # 0.25 based on 201509010902

                 # Problem presentation and execution
                 "n_problems": [200,1000],
                 "addends_matrix_offby1_delta": [1.0], # =1 will make the "next-to" inputs 0, =0 makes them 1, and so on

                 # Choosing to use retrieval v. a strategy
                 "RETRIEVAL_LOW_CC": [0.8], # Should be 0.6 usually; at 1.0 no retrieval will occur
                 "RETRIEVAL_HIGH_CC": [1.0], # Should be 1.0 usually

                 # Learning target params
                 "results_hidden_units": [20], # 20 per experiments of 20160112b -- maybe 18?
                 "non_result_y_filler": [0.0], # Set into all outputs EXCEPT result, which is adjusted by INCR_RIGHT and DECR_WRONG


                 # WARNING! THE DIRECTIONALITY OF THESE INCR and DECRS IS VERY IMPORTANT! GENERALLY, THEY SHOULD
                 # ALL BE POSITIVE NUMBERS AS THE DECR_on_WRONG (for example) IS ACTUALLY *SUBTRACTED* FROM WRONG TARGETS!
                 "INCR_on_RIGHT": [1.0], # Added to non_result_y_filler at the response value when you get it right.
                 "DECR_on_WRONG": [1.0], # Substrated from non_result_y_filler at the response value when you get it right.
                 "INCR_the_right_answer_on_WRONG": [1.0], # Added to non_result_y_filler at the CORRECT value when you get it WRONG.
                 "results_learning_rate": [0.05], # default: 0.1 Explored 201509010826
                 "in_process_training_epochs": [1] # Number of training epochs on EACH test problem (explored 201509010826)
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

#this is going to be the output
def addends_matrix(a1, a2):
    cv = 1.0 # central value
    delta = settings.param("addends_matrix_offby1_delta")
    lis = [0] * 14
    # First addend
    lis[a1 - 1] = cv - delta
    lis[a1] = cv
    lis[a1 + 1] = cv - delta
    # Second addend
    lis[a2 + 6] = cv - delta
    lis[a2 + 7] = cv
    lis[a2 + 8] = cv - delta
    return lis


class NeuralNetwork:
    def __init__(self, name, layers, type, outputs):
        self.outputs = outputs
        #print "HERE IS SELF.OUTPUT" + str(self.outputs)
        self.name=name
        self.errr=[]
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

        # Generate a cc in the range for this network type
        self.low_cc = settings.param(type + "_LOW_CC")
        self.high_cc = settings.param(type + "_HIGH_CC")
        self.cc = self.low_cc + (self.high_cc - self.low_cc) * random()

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
            for j in range(1, 6):
                self.X.append(addends_matrix(i, j))
                #print outputs
#building input probe^
        self.X = numpy.array(self.X)
        #go to v2 later print self.X
        self.predictions = []

    # The output array from the NN is created by .append(ing) a bunch of
    # probe results, something like this: 
    #
    #    for i in range(1,6):
    #        for j in range (1,6):
    #          ...generate the i+j output array and append to the growing vector...
    #
    # What results is a long array where the index of the problem we're looking for 
    # is position: 5 * (i - 1) + (j - 1). For example, for 3+4 you end up with 
    # 5 * 2 * 3 = position 30 in the output units array. 
        
    @staticmethod
    def y_index(a1, a2):
        return 5 * (a1 - 1) + (a2 - 1)

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
            self.errr.append(error)
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
        # a = numpy.concatenate((numpy.ones(1).T, numpy.array(x)), axis=1)
        #WWW for an updated numpy version, replace the above line with the line below
        a = numpy.insert(numpy.array(x), 0, numpy.ones(1).T)
        for l in range(0, len(self.weights)):
            a = self.activation(numpy.dot(a, self.weights[l]))
        return a

    def predict_with_dumpage(self, x):
        logstream.write("(:predict_with_dumpage\n (:inputs " +  lispify(x) + ")\n")
        a = numpy.concatenate((numpy.ones(1).T, numpy.array(x)), axis=1)
        for l in range(0, len(self.weights)):
            logstream.write(" (:level " + lispify(l))
            a = self.activation(numpy.dot(a, self.weights[l]))
            logstream.write(" (:products "+ lispify([round(x,5) for x in  a]) + "))\n")
        logstream.write(")\n")
        return a
    
    def try_memory_retrieval(self, a1, a2):
        index = self.y_index(a1, a2)
        #print index
        if (a1 > 5) or (a2 > 5):
            return None
        # Collect the values that come above cc.
        results_above_cc = [x for x in range(self.layers[-1]) if self.predictions[index][x] > self.cc]

        l = len(results_above_cc)
        if l > 0:
            # At the moment this chooses randomly from all those
            # (either strats or results) above the respective cc,
            # although this could be changed to choose in a weighted
            # manner. FFF ???
            return self.outputs[int(results_above_cc[randint(0, l - 1)])]
        return None

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
    def guess_vector(self, a1, a2, beg, end):
        #print a1,a2,beg,end
        vec = []
        #print self.predictions
        self.predict(addends_matrix(a1, a2))
        #print self.predictions
        for i in range(beg, end):
            #print i, self.y_index(a1,a2)
            vec.append(round(self.predictions[self.y_index(a1, a2)][i], 5))
        return (vec)

    def update_predictions(self):
        #print "> update_predictions"
        self.predictions = []
        for i in range(1, 6):
            for j in range(1, 6):
                #print "update_predictions: ", i, j, self.predictions
                self.predictions.append(self.predict(addends_matrix(i, j)))

    # What target does for now is create a square matrix filled with
    # 0.5, and for the 1d matrix at y_index(a1, a2) it will have
    # everything but the correct answer be -= DECR_RIGHT/WRONG and the
    # correct answer will have INCR_RIGHT/WRONG added to it

    def reset_target(self):
        #print "> reset_target"
        self.target = []
        self.target.append([settings.param("non_result_y_filler")] * (self.layers[-1]))
        self.target = numpy.array(self.target)

    # This gets very ugly because in order to be generalizable
    # across different sorts of NN outputs.
    def update_target(self, a1, a2, targeted_output, correct, correct_output_on_incorrect = None):
        #print "> update_target"
        self.X = []
        self.X.append(addends_matrix(a1, a2))
        self.X = numpy.array(self.X)
        #ok let's investigate this
        #print targeted_output
        targeted_output_position = self.outputs.index(targeted_output)
        #print "HERE IS TARGETED OUTPUT POSITION" + str(targeted_output_position)
        if correct:
            self.target[0][targeted_output_position] += settings.param("INCR_on_RIGHT")
        else:
            self.target[0][targeted_output_position] -= settings.param("DECR_on_WRONG")
            if correct_output_on_incorrect is not None: 
                self.target[0][self.outputs.index(correct_output_on_incorrect)] += settings.param("INCR_the_right_answer_on_WRONG")
        #print self.target

    def dump_hidden_activations(self):
        logstream.write('(:'+self.name+"-hidden-activation-table\n")
        for a1 in range(1, 6):
            for a2 in range(1, 6):
                self.predict_with_dumpage(addends_matrix(a1, a2))
        logstream.write(')\n')

    def dump_predictions(self):
        logstream.write('(:'+self.name+"-prediction-table\n")
        for i in range(1, 6):
            for j in range(1, 6):
                gv = self.guess_vector(i, j, 0, self.layers[-1])
                logstream.write(" (%s + %s = " % (i, j) + str(self.outputs[numpy.argmax(gv)]) + " " + lispify(gv) + ")\n")
        logstream.write(')\n')

    def dump_weights(self):
        logstream.write(" (:dump-weights\n")
        for l in range(0, len(self.weights)):
            logstream.write(" (:level " + lispify(l))
            logstream.write(" (:weights "+ lispify([[round(x,5) for x in y] for y in self.weights[l]]) + "))\n")
        logstream.write("   )\n")

    def dump(self):
        self.dump_weights()
        if settings.dump_hidden_activations:
            self.dump_hidden_activations()
        self.dump_predictions()

##################### DRIVER #####################

# Set up the neural network fitted to kids' having learned how to
# count before we got here, so there is a tendency for problems what
# look like 3+4 to result in saying 5. To do this we burn in a set of
# I/O relationships that have this tendency.

def init_neturalnets():
    global rnet
    rnet = results_network()

def results_network():
    # There are 14 input units bcs we include an extra on each side of each addends for representation diffusion.
    nn = NeuralNetwork("Results", [14, settings.param("results_hidden_units"), 13],"RETRIEVAL",[0,1,2,3,4,5,6,7,8,9,10,11,12,"other"])
    # Burn in counting examples. For the moment we simplify this to
    # training: ?+B=B+1.
    X_count = []
    y_count = []
    for a in range(1, 5):
        for b in range(1,5):
            X_count.append(addends_matrix(a, b))
            y_count.append(1)
    X_count = numpy.array(X_count)
    y_count = numpy.array(y_count)
    #print X_count
    #print y_count
    # Now burn it in:
    nn.fit(settings.param("initial_counting_network_learning_rate"), settings.param("initial_counting_network_burn_in_epochs"), X_count, y_count)
    nn.update_predictions()
    return nn

# We first try a retrieval on the sum, and if that fails we have to
# use a strategy, which we try to retrieve and if that fails we choose
# a random strategy. Then we update the rnet accordingly, and fit and
# update_y this is the main driver within driver that does the testing

def exec_strategy():
    global rnet
    global SOLUTION
    rnet.reset_target()
    #create input
    PPA()  # Create a random problem: sets the global ADDENDS to an Addend object
    ad1=ADDENDS.ad1
    ad2=ADDENDS.ad2

    correct = ad2 + ad1
    # *** Herein Lies a fundamental choice of whether retrieval is an explicit strategy or not !!!
    strat_name = None
    retrieval = rnet.try_memory_retrieval(ad1,ad2)
    #changed the below from -666 to 0, since we don't have anything else in the else block right now and it would break :(
    SOLUTION = 0
    # Used to be 0, but why is this needed?! 
    # (DDD If this shows up, there's something really wrong!) 
    # (this is just used to initialize solution, or else it's not in the right code block
    # we have to reset the target for every problem, 
    # or else it uses the target from the last problem
    #print retrieval
    if retrieval is not None:
        SOLUTION = retrieval
        #print "USING RETRIEVAL, SOLUTION IS!!!!!!!!!!!" + str(SOLUTION)
        logstream.write("(:used retrieval " +  str(ad1) + " + " + str(ad2) + " = " + str(SOLUTION) + ") ")
    else:
        #there are no results above the confidence criterion
        # ??? what should go here for the new version
        pass
# update the nns:
    
    rnet.update_target(ad1, ad2, SOLUTION, correct, ad1 + ad2)
    rnet.fit(settings.param("results_learning_rate"), settings.param("in_process_training_epochs"))
    rnet.update_predictions()

# UUU The open and close structure here is a mess bcs of the
# occassional dumping of tables, which I'm trying to embed at the end
# of the relevant preceeding problem block for analytical conveneince.

def present_problems():
    logstream.write('(:problem-block\n')
    rnet.dump()
    logstream.write('   (:problems\n')
    for i in range(settings.param("n_problems")):
        logstream.write('(')
        exec_strategy()
        logstream.write(')\n')
        if i % settings.pbs == 0 or i == settings.param("n_problems"):
            logstream.write('   ) ;; close :problems\n')
            rnet.dump()
            logstream.write('    ) ;; close :problem-block\n')
            logstream.write('  (:problem-block\n')
            logstream.write('   (:problems\n')
    logstream.write('   ) ;; close :problems\n')
    logstream.write('    ) ;; close :problem-block\n') # Warning! We may get an extra one of these!           

# Execute with all the possible values of each parameter. This is a
# weird recursive function. The top half that calls itself repeatedly
# until it has chosen and set the next value for each paramter. Then
# it drops into the second half, which actually runs with whatever the
# latest set of parameters is. The state of the recursion holds the
# state of the parameter scan. (For slight efficiency this uses a
# quasi-global called param_specs_keys and gets set in the caller.)

def config_and_test(index=0):
    global param_specs_keys, logstream
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
            init_neturalnets()
            logstream.write(' )\n')
            logstream.write('(:run\n')
            present_problems()
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
    global param_specs_keys, settings, hidden_units
    start = timeit.default_timer()
    # Used in the recursive config_and_test fn.
    settings = Settings()  
    param_specs_keys=settings.param_specs.keys()
    print "Parameter spec:"
    print (str(settings.param_specs))
    print "-----"
    for i in range(settings.ndups):
        print ">>>>> Rep #" + str(i + 1) + " <<<<<"
        config_and_test()
    stop = timeit.default_timer()
    print stop - start

# Run:
if __name__ == '__main__':
    top_level_run()
