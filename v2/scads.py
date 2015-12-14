# Notes:
# Maybe should change on every, say, pbs round (approx.: age) to
# simulate improvment in ability to count correctly with age.

import timeit
import datetime
import os
import numpy
from random import randint, shuffle, random

global settings, logstream, rnet, snet

##################### ADD #####################

global EB, ADDENDS, HAND, CB, EB, SOLUTION_COMPLETED, SOLUTION, TL

def lispify(s):
    return (((str(s).replace(","," ")).replace("[","(")).replace("]",")")).replace("\'","\"")

# ----- Operators for actual addition strategies and test routines.

# The peripheral system.This is a very simple representation of the
# stuff needed for the addition domains: ten "fingers", a focus of
# attention, and an echoic memory into which numbers are stored.
# Operations on these constitute the basic operations of the
# domain.

class Hand(object):
    def __init__(self):

        # The fingers memory structure; five on each of two hands,
        # each of which my be up or down.

        self.s = {'left': ['d'] * 5, 'right': ['d'] * 5}
        # The focus of attention.

        self.foa = {'hand': 'left', 'finger': 0}
        self.hand = 'left'

    def clear(self):
        self.s['left'] = ['d'] * 5
        self.s['right'] = ['d'] * 5

    # Basic operations; most of which operate on what's in foa.

    def increment_focus(self):
        if not ((self.foa['hand'] == 'right') and (self.foa['finger'] == 4)):

            # If we're done the left hand, move on to the rigt.

            if (self.foa['hand'] == 'left') and (self.foa['finger'] == 4):
                self.foa['hand'] = 'right'
                self.foa['finger'] = 0

            # Else shift to the next finger.

            else:
                self.foa['finger'] += 1

    # Finger raising; always does the focussed finger.

    def put_up(self):
        self.s[self.foa['hand']][self.foa['finger']] = 'u'

    # The hands are external components as well, so
    # that all you need to do is select a hand and switch hands.

    def choose(self):
        if randint(0, 1) == 0:
            self.hand = 'left'
        else:
            self.hand = 'right'
        self.foa['hand'] = self.hand
        self.foa['finger'] = 0

    def swap(self):
        if self.hand == 'left':
            self.hand = 'right'
        else:
            self.hand = 'left'
        self.foa['hand'] = self.hand
        self.foa['finger'] = 0

# Dynamic Retrieval (DR) simulates the case where when the child looks
# at his hands after raising his fingers it primes a retrival that
# interrupts the problem flow with an answer. There are several
# complexities with this, some theoretical and some pratical. The
# practical problem is that we have to transform the fingers-on-a-hand
# representation into the input layer representation, and then do the
# rnet probe. That's what most of this does. A more interesting problem
# is that theoretical problem of what precisely is being primed by
# having one's fingers raised. For example, suppose the problem is
# 3+4, the child raises 3 fingers on one hand, but if we were to do a
# primed probe at that moment (which we DO when DR is on!), why would
# that prime 7? It should prime 3, right? Do we want 3 or 7? That is,
# does this prime CORRECT or INCORRECT solutions? And if it's supposed
# to prime 7, then you'd need some other representational machinery to
# make that happen...maybe, or maybe the rnet will have a slight 3->7
# (and 4->7) links already burned in, as well as the "correct" 3+4->7
# links. 

def try_dynamic_retrieval():
    global SOLUTION, SOLUTION_COMPLETED
    # Transform fingers-on-a-hand representation into input layer rep
    add_matrix = [0] * 14
    index = 0
    while index < 5 and HAND.s['left'][index] == 'u':
        index += 1
    add_matrix[index + 1] = 1
    while index < 5 and HAND.s['right'][index] == 'u':
        index += 1
    add_matrix[index + 5 + 1] = 1
    # Make a prediction
    prediction = rnet.predict(add_matrix)
    results_above_DRR = []
    for i in range(0, 13):
        if prediction[i] > DR_threshold:
            results_above_DRR.append(i)
    if len(results_above_DRR) > 0:
        SOLUTION = results_above_DRR[randint(0, len(results_above_DRR) - 1)]
        SOLUTION_COMPLETED = True  # Tell the strategy executor to break
        logstream.write("(:dynamic-retrival " + str(ADDENDS.ad1) + " + " + str(ADDENDS.ad2) + " = " + str(SOLUTION) + ") ") 
        return None
    return None

# Manipulation in the echoic buffer where number facts live.  We
# assume perfect knowledge of the number sequence.  That way incr
# and decr can be used.  This is where all possible errors come into
# the picture.  There is a probability (PERR) of say-next
# reporting the WRONG number; this always takes place by simply
# failing to count.  Note that there are really a number of ways
# that one can get errors in addition, but the basic effect of
# correlating errors with the number of step in the procedure is
# accomplished by this method.

def say(n):
    global EB
    EB = n

def say_next():
    global EB
    if EB == 0:
        say(1)
    elif settings.param("PERR") > random():
        say(EB)  #forgot to count but flipped finger
    else:
        say(EB + 1)

# Clear EB each time you're about to start a count off.  If you
# don't do this, the last number counted will be left in the echoic
# buffer and you'll count on from it, which is actually right, of
# course, for shortcut-sum.

def clear_eb():
    global EB
    EB = 0

# This tells the driver.py to stop.

def end():
    global SOLUTION, SOLUTION_COMPLETED
    SOLUTION_COMPLETED = True
    SOLUTION = EB

# Raise is an important part of this process.  The question is how to
# do the test-for-done.  That is, when putting up fingers, how does
# the child know when he's got the right number up?  In this version,
# he uses the echoic buffer trace, but that can't be right for
# shortcut sum because the echoic buffer contains the SUM, not just
# the single hand's count, so the right hand won't work.  Somehow the
# child can SAY one thing while covertly counting another.  This
# suggests a dissociation of the echoic number sequence from counting,
# which can be done covertly.  Instead of relying upon the echoic
# trace for the count, We uses a new buffer (*cb*) to maintain the
# count of fingers up on a particular hand.  This buffer is cleared by
# raise2 itself, and is used for the done test.

def count_from_one_twice_strategy():
    return [
        # First addend on first hand.
        HAND.clear, HAND.choose, ADDENDS.choose, ADDENDS.say, clear_eb, raise_hand,
        # Second addend on the other hand.
        HAND.swap, ADDENDS.swap, ADDENDS.say, clear_eb, raise_hand,
        # Final count out.
        HAND.choose, clear_eb, count_fingers, HAND.swap, count_fingers,
        end]

def count_from_one_once_strategy():
    return [
        # First addend on first hand.
        HAND.clear, HAND.choose, ADDENDS.choose, ADDENDS.say, clear_eb, raise_hand,
        # Second addend on the other hand.
        HAND.swap, ADDENDS.swap, raise_hand,
        end]

def count_from_either_strategy():
    return [
        # Count from the first addend.
        ADDENDS.choose, ADDENDS.say,
        # Second addend on a hand.
        HAND.clear, HAND.choose, ADDENDS.swap, raise_hand,
        end]

def min_strategy():
    return [
        # Count from the larger addend.
        ADDENDS.choose_larger, ADDENDS.say,
        # Second addend on a hand.
        HAND.clear, HAND.choose, ADDENDS.swap, raise_hand,
        end]

def random_strategy():
    list_of_operations = [
        HAND.clear, HAND.clear, HAND.choose, HAND.choose, ADDENDS.choose, ADDENDS.choose,
        ADDENDS.say, ADDENDS.say, clear_eb, clear_eb, raise_hand, raise_hand, HAND.swap, HAND.swap,
        ADDENDS.swap, ADDENDS.swap, count_fingers, count_fingers, end]
    shuffle(list_of_operations) # WWW This puts the end someplace randomly in the middle of the strategy WWW???
    return list_of_operations

# This version of raise assumes that hand control is done by the
# caller.

def raise_hand():
    CB = 0
    while True:
        say_next()
        HAND.put_up()
        HAND.increment_focus()
        CB += 1
        if CB >= ADDENDS.addend:
            break
    if settings.dynamic_retrieval_on:
        # DR (if on) will set global SOLUTION and SOLUTION_COMPLETED
        # if it works, otherwise, things will just move along.
        try_dynamic_retrieval()

def count_fingers():
    for i in range(5):
        look_n_count()


def look_n_count():
    if HAND.s[HAND.foa['hand']][HAND.foa['finger']] == 'u':
        say_next()
    HAND.increment_focus()


# Finally we need to replace the n1 and n2 with echoic buffers so
# that they aren't arguments to a lisp function. This also requires
# putting the addends into external stores which, like the hands,
# can be attended.  Instead of doing all that I just assume here
# that the problem is written on an external board, and that there
# is a sort of second set of eyes that can look at one or the other
# addend, and swap them, just like with the hands.  We ought to
# organize all the different buffers.  I wonder if kids ever get
# these mixed up, and if not, why not?

class Addend(object):
    def __init__(self, ad1, ad2):
        self.ad1 = ad1
        self.ad2 = ad2
        self.addend = 0
        self.cla = ''

    def choose(self):
        if randint(0, 1) == 0:
            self.addend = self.ad1
        else:
            self.addend = self.ad2

        # Indicate which addend we've chosen for min discovery.

        if self.ad1 == self.ad2:
            self.cla = 'equal'
        elif ((self.addend == self.ad1) and (self.ad1 > self.ad2)) or (
                    (self.addend == self.ad2) and (self.ad1 < self.ad2)):
            self.cla = 'larger'
        else:
            self.cla = 'smaller'

    def swap(self):
        if self.addend == self.ad1:
            self.addend = self.ad2
        else:
            self.addend = self.ad1

    def say(self):
        say(self.addend)

    def choose_larger(self):
        if self.ad1 > self.ad2:
            self.addend = self.ad1
        else:
            self.addend = self.ad2

        if self.ad1 == self.ad2:
            self.cla = 'equal'
        else:
            self.cla = 'larger'

# Initialize hand, echoic buffer and counting buffer,and carry out the
# strategy.  Update memory and RNET at the end.

def exec_explicit_strategy(strategy_choice):
    global SOLUTION, SOLUTION_COMPLETED
    SOLUTION_COMPLETED = False
    global HAND
    global EB
    EB = 0
    CB = 0
    HAND = Hand()
    # Funcall the named to get the list of operations from the strategy.
    list_of_operations = strategy_choice()
    # This part is really confusing. Python is using it's own functions and arguments
    # but it's functions and arguments are all exported out and back in. ???!!!
    # Carry out the operations.
    for i in list_of_operations:
        if SOLUTION_COMPLETED:
            break
        i()
    return SOLUTION

# Problem Presentation Algorithm (PPA).  Just random for now.

def PPA():
    global ADDENDS
    ADDENDS = Addend(randint(1, 5), randint(1, 5))

##################### SETTINGS #####################

class Settings:

    # PART 1: These usually DON'T change:
    ndups = 30  # Number of replicates of each combo of params -- usually 3 unless testing.
    pbs = 500  # problem bin size, every pbs problems we dump the predictions
    dynamic_retrieval_on = False
    
    # PART 2: These also usually DON'T change, although they might if you
    # want to explore bringing in and out various strategies:
    # We usually leave out random_strategy
    strategies = {"count_from_one_twice": count_from_one_twice_strategy,
                  "count_from_one_once": count_from_one_once_strategy,
                  "count_from_either": count_from_either_strategy,
                  #"random_strategy": random_strategy,
                  "min": min_strategy
                  }
    
    # PART 3: These usually DO change:
    # IMPORTANT: REMEMBER TO CHANGE experiment_label, which is
    # used by the analyzer to label the results file. (Because we set
    # these by exec(), this has to have an extra set of "\"quotes\""
    # around it.) [These are all False initially just so that if something
    # screws up, and one or more of these don't get set, I'll get an obvious 
    # error.]
    
    def param(self, key):
        return self.params[key]

    params = {} # These are set for a given run by the recursive param search algorithm

    param_specs = {"experiment_label": 
                 ["\"20151201g: 3000 problems (binning @ 500) with various hidden unit widths\""],

#     ************************************************************************************************************************
#     ******************************** REMEMBER TO CHANGE THE EXPERIMENT_LABEL (ABOVE) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     ************************************************************************************************************************

                 # Setting up the initial counting network
                 "initial_counting_network_burn_in_epochs": [1], # 1000 based on 201509010902
                 "initial_counting_network_learning_rate": [0.01], # 0.25 based on 201509010902

                 # Problem presentation and execution
                 "n_problems": [3000],
                 "DR_threshold": [1.0], # WWW!!! Only used if dynamic_retrieval_on = True
                 "PERR": [0.1], # 0.1 confirmed 201509010826
                 "addends_matrix_offby1_delta": [1.0], # =1 will make the "next-to" inputs 0, =0 makes them 1, and so on

#     ************************************************************************************************************************
#     ******************************** REMEMBER TO CHANGE THE EXPERIMENT_LABEL (ABOVE) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     ************************************************************************************************************************

                 # Choosing to use retrieval v. a strategy
                 "RETRIEVAL_LOW_CC": [0.8], # Should be 0.6 usually; at 1.0 no retrieval will occur
                 "RETRIEVAL_HIGH_CC": [1.0], # Should be 1.0 usually
                 "STRATEGY_LOW_CC": [0.6], # If 1.0, strategies will be chosen randomly
                 "STRATEGY_HIGH_CC": [1.0],

                 # Learning target params
                 "strategy_hidden_units": [10],
                 "results_hidden_units": [9,11,13,15,17,19], # 20 seems to be enough
                 "non_result_y_filler": [0.0], # Set into all outputs EXCEPT result, which is adjusted by INCR_RIGHT and DECR_WRONG

#     ************************************************************************************************************************
#     ******************************** REMEMBER TO CHANGE THE EXPERIMENT_LABEL (ABOVE) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#     ************************************************************************************************************************

                 # WARNING! THE DIRECTIONALITY OF THESE INCR and DECRS IS VERY IMPORTANT! GENERALLY, THEY SHOULD
                 # ALL BE POSITIVE NUMBERS AS THE DECR_on_WRONG (for example) IS ACTUALLY *SUBTRACTED* FROM WRONG TARGETS!
                 "INCR_on_RIGHT": [1.0], # Added to non_result_y_filler at the response value when you get it right.
                 "DECR_on_WRONG": [1.0], # Substrated from non_result_y_filler at the response value when you get it right.
                 "INCR_the_right_answer_on_WRONG": [1.0], # Added to non_result_y_filler at the CORRECT value when you get it WRONG.
                 "strategy_learning_rate": [0.1],
                 "results_learning_rate": [0.1], # default: 0.1 Explored 201509010826
                 "in_process_training_epochs": [10] # Number of training epochs on EACH test problem (explored 201509010826)
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

# This is used for counting exposure
def sum_matrix(s):
    lis = [0] * 13
    lis[s] = 1
    return lis

class NeuralNetwork:
    def __init__(self, name, layers, type, outputs):
        self.outputs = outputs
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
        self.X = numpy.array(self.X)
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
        a = numpy.concatenate((numpy.ones(1).T, numpy.array(x)), axis=1)
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

    # Returns a function that picks a random result from a list of
    # results above the confidence criterion this is used for the
    # retrieval. when we want to try to retrieve a sum, for example 3
    # + 4 = 7, we pass in a1 = 3, a2 = 4. It looks to see the values
    # that are above the cc, and chooses a random number from those
    # values. if there are none, it returns None. 

    def try_memory_retrieval(self, a1, a2):
        index = self.y_index(a1, a2)
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

    # Used for analysis output, this just gets the prediction values
    # for a particular sum. FFF Maybe this could be used inside guess?
    # FFF Anyway, see notes for guess to explain the begin and end
    # things.

    def guess_vector(self, a1, a2, beg, end):
        vec = []
        self.predict(addends_matrix(a1, a2))
        for i in range(beg, end):
            vec.append(round(self.predictions[self.y_index(a1, a2)][i], 5))
        return (vec)

    def update_predictions(self):
        self.predictions = []
        for i in range(1, 6):
            for j in range(1, 6):
                self.predictions.append(self.predict(addends_matrix(i, j)))

    # What target does for now is create a square matrix filled with
    # 0.5, and for the 1d matrix at y_index(a1, a2) it will have
    # everything but the correct answer be -= DECR_RIGHT/WRONG and the
    # correct answer will have INCR_RIGHT/WRONG added to it

    def reset_target(self):
        self.target = []
        self.target.append([settings.param("non_result_y_filler")] * (self.layers[-1]))
        self.target = numpy.array(self.target)

    # This gets very ugly because in order to be generalizable
    # across different sorts of NN outputs.
    def update_target(self, a1, a2, targeted_output, correct, correct_output_on_incorrect = None):

        self.X = []
        self.X.append(addends_matrix(a1, a2))
        self.X = numpy.array(self.X)

        targeted_output_position = self.outputs.index(targeted_output)

        if correct:
            self.target[0][targeted_output_position] += settings.param("INCR_on_RIGHT")
        else:
            self.target[0][targeted_output_position] -= settings.param("DECR_on_WRONG")
            if correct_output_on_incorrect is not None: 
                self.target[0][self.outputs.index(correct_output_on_incorrect)] += settings.param("INCR_the_right_answer_on_WRONG")

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
        self.dump_hidden_activations()
        self.dump_predictions()

##################### DRIVER #####################

# Set up the neural network fitted to kids' having learned how to
# count before we got here, so there is a tendency for problems what
# look like 3+4 to result in saying 5. To do this we burn in a set of
# I/O relationships that have this tendency.

def init_neturalnets():
    global rnet, snet
    rnet = results_network()
    snet = strategy_network()

def results_network():
    # There are 14 input units bcs we include an extra on each side of each addends for representation diffusion.
    nn = NeuralNetwork("Results", [14, settings.param("results_hidden_units"), 13],"RETRIEVAL",[0,1,2,3,4,5,6,7,8,9,10,11,12,"other"])
    # Create the counting examples matrix k, the inputs are the
    # addends matrix for (1+2) , (2+3), etc and the outputs are
    # (1+2)=3 (2+3)=4.
    X_count = []
    y_count = []
    for i in range(1, 5):
        X_count.append(addends_matrix(i, i + 1))
        y_count.append(sum_matrix(i + 2))
    X_count = numpy.array(X_count)
    y_count = numpy.array(y_count)
    # Now burn it in:
    nn.fit(settings.param("initial_counting_network_learning_rate"), settings.param("initial_counting_network_burn_in_epochs"), X_count, y_count)
    nn.update_predictions()
    return nn

def strategy_network():
    nn = NeuralNetwork("Strategy", [14, settings.param("strategy_hidden_units"), len(settings.strategies)],"STRATEGY",settings.strategies.keys())
    nn.update_predictions()
    return nn

# We first try a retrieval on the sum, and if that fails we have to
# use a strategy, which we try to retrieve and if that fails we choose
# a random strategy. Then we update the rnet accordingly, and fit and
# update_y this is the main driver within driver that does the testing

def exec_strategy():
    global rnet, snet
    global SOLUTION
    rnet.reset_target()
    snet.reset_target()
    PPA()  # Create a random problem: sets the global ADDENDS to an Addend object
    ad1=ADDENDS.ad1
    ad2=ADDENDS.ad2
    # *** Herein Lies a fundamental choice of whether retrieval is an explicit strategy or not !!!
    strat_name = None
    retrieval = rnet.try_memory_retrieval(ad1,ad2)
    SOLUTION = -666
    # Used to be 0, but why is this needed?! 
    # (DDD If this shows up, there's something really wrong!) 
    # (this is just used to initialize solution, or else it's not in the right code block
    # we have to reset the target for every problem, 
    # or else it uses the target from the last problem
    if retrieval is not None:
        SOLUTION = retrieval
        logstream.write("(:used retrieval " +  str(ad1) + " + " + str(ad2) + " = " + str(SOLUTION) + ") ")
    else:
        # retrieval failed, so we get try to get a strategy from above the confidence criterion and use hands to add
        strat_name = snet.try_memory_retrieval(ad1,ad2)
        if strat_name is None:
            # Pick a random one!
            strat_name = settings.strategies.keys()[randint(0, len(settings.strategies) - 1)]
        logstream.write("(:trying " +  strat_name +  " " + str(ad1) + " + " + str(ad2) + ") ")
        SOLUTION = exec_explicit_strategy(settings.strategies[strat_name])
        # !!! WWW WARNING (for analysis): This gets displayed even if
        # Dynamic Retrieval was used. You have to Analyze this
        # distinction out of the log at the end by seeing that a DR
        # message appeared!
        logstream.write("(:used " +  strat_name + " " + str(ad1) + " + " + str(ad2) + " = " + str(SOLUTION) + ") ")
        # update the target based on if the strategy worked or not
        snet.update_target(ad1, ad2, strat_name, SOLUTION == ad1 + ad2)
    correct = SOLUTION == ad1+ad2 # slightly redundant but we needed it for the else-nested call above. Oh well.
    # update the nns:
    rnet.update_target(ad1, ad2, SOLUTION, correct, ad1 + ad2)
    rnet.fit(settings.param("results_learning_rate"), settings.param("in_process_training_epochs"))
    rnet.update_predictions()
    if strat_name is not None:
        snet.update_target(ad1, ad2, strat_name, correct)
        snet.fit(settings.param("strategy_learning_rate"), settings.param("in_process_training_epochs"))
        snet.update_predictions()

# UUU The open and close structure here is a mess bcs of the
# occassional dumping of tables, which I'm trying to embed at the end
# of the relevant preceeding problem block for analytical conveneince.

def present_problems():
    logstream.write('(:problem-block\n')
    logstream.write('   (:problems\n')
    for i in range(settings.param("n_problems")):
        logstream.write('(')
        exec_strategy()
        logstream.write(')\n')
        if i % settings.pbs == 0 or i == settings.param("n_problems"):
            logstream.write('   ) ;; close :problems\n')
            rnet.dump()
            snet.dump()
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
            logstream.write(' (:strategies' + lispify(settings.strategies.keys()) + ")\n")
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

def gen_file_name():
    file_name = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
    full_file__name = os.path.join(os.path.join(os.path.dirname(__file__), 'runlogs'), file_name + '.lisp')
    return full_file__name

def top_level_run():
    global param_specs_keys, settings, hidden_units
    start = timeit.default_timer()
    # Used in the recursive config_and_test fn.
    settings = Settings()  
    param_specs_keys=settings.param_specs.keys()
    print "Parameter spec:"
    print (str(settings.param_specs))
    print "Strategies in play:"
    print settings.strategies
    print "-----"
    for i in range(settings.ndups):
        print ">>>>> Rep #" + str(i + 1) + " <<<<<"
        config_and_test()
    stop = timeit.default_timer()
    print stop - start

# Run:
if __name__ == '__main__':
    top_level_run()
