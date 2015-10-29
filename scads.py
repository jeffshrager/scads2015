import timeit
import csv
import datetime
import os
import numpy
from random import randint, shuffle, random

global settings, writer, nn

##################### ADD #####################

global EB, ADDEND, HAND, CB, EB, SOLUTION_COMPLETED, SOLUTION, TL

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
            # mempush(swap-hands, from left to right)
        else:
            self.hand = 'left'
            # mempush(swap-hands, from right to left)
        self.foa['hand'] = self.hand
        self.foa['finger'] = 0

# Dynamic Retrieval (DR) simulates the case where when the child looks
# at his hands after raising his fingers it primes a retrival that
# interrupts the problem flow with an answer. There are several
# complexities with this, some theoretical and some pratical. The
# practical problem is that we have to transform the fingers-on-a-hand
# representation into the input layer representation, and then do the
# nn probe. That's what most of this does. A more interesting problem
# is that theoretical problem of what precisely is being primed by
# having one's fingers raised. For example, suppose the problem is
# 3+4, the child raises 3 fingers on one hand, but if we were to do a
# primed probe at that moment (which we DO when DR is on!), why would
# that prime 7? It should prime 3, right? Do we want 3 or 7? That is,
# does this prime CORRECT or INCORRECT solutions? And if it's supposed
# to prime 7, then you'd need some other representational machinery to
# make that happen...maybe, or maybe the nn will have a slight 3->7
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
    prediction = driver.nn.predict(add_matrix)
    results_above_DRR = []
    for i in range(0, 13):
        if prediction[i] > DR_threshold:
            results_above_DRR.append(i)
    if len(results_above_DRR) > 0:
        SOLUTION = results_above_DRR[randint(0, len(results_above_DRR) - 1)]
        SOLUTION_COMPLETED = True  # Tell the strategy executor to break
        writer.writerow(["!", "dynamic_retrival", ADDEND.ad1, ADDEND.ad2, SOLUTION])  # This might report twice
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
    # mempush(say, list n)


def say_next():
    global EB
    if EB == 0:
        say(1)
    elif settings.PERR > random():
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

        HAND.clear,
        HAND.choose,
        ADDEND.choose,
        ADDEND.say,
        clear_eb,
        raise_hand,

        # Second addend on the other hand.

        HAND.swap,
        ADDEND.swap,
        ADDEND.say,
        clear_eb,
        raise_hand,

        # Final count out.

        HAND.choose,
        clear_eb,
        count_fingers,
        HAND.swap,
        count_fingers,
        end]


def count_from_one_once_strategy():
    return [

        # First addend on first hand.

        HAND.clear,
        HAND.choose,
        ADDEND.choose,
        ADDEND.say,
        clear_eb,
        raise_hand,

        # Second addend on the other hand.

        HAND.swap,
        ADDEND.swap,
        raise_hand,
        end]


def count_from_either_strategy():
    return [

        # Count from the first addend.

        ADDEND.choose,
        ADDEND.say,

        # Second addend on a hand.

        HAND.clear,
        HAND.choose,
        ADDEND.swap,
        raise_hand,
        end]


def min_strategy():
    return [

        # Count from the larger addend.

        ADDEND.choose_larger,
        ADDEND.say,

        # Second addend on a hand.

        HAND.clear,
        HAND.choose,
        ADDEND.swap,
        raise_hand,
        end]


def random_strategy():
    list_of_operations = [
        HAND.clear, HAND.clear,
        HAND.choose, HAND.choose,
        ADDEND.choose, ADDEND.choose,
        ADDEND.say, ADDEND.say,
        clear_eb, clear_eb,
        raise_hand, raise_hand,
        HAND.swap, HAND.swap,
        ADDEND.swap, ADDEND.swap,
        count_fingers, count_fingers,
        end]

    shuffle(list_of_operations)
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
        if CB >= ADDEND.addend:
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
# strategy.  Update memory and NN at the end.

def exec_explicit_strategy(strategy_choice):
    global SOLUTION, SOLUTION_COMPLETED
    SOLUTION_COMPLETED = False
    global HAND
    global EB
    writer.writerow(["trying", strategy_choice, ADDEND.ad1, ADDEND.ad2])  # This might report twice
    EB = 0
    CB = 0
    HAND = Hand()
    # Get the list of operations from the strategy.
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
    global ADDEND
    ADDEND = Addend(randint(1, 5), randint(1, 5))

##################### SETTINGS #####################

class Settings:

    # PART 1: These usually DON'T change:
    hidden_units = 30
    ndups = 3  # Number of replicates of each combo of params -- usually 3 unless testing.
    pbs = 25  # problem bin size, every pbs problems we dump the predictions
    dynamic_retrieval_on = False
    
    # PART 2: These also usually DON'T change, although they might if you
    # want to explore bringing in and out various strategies:
    # We usually leave out random_strategy
    strategies = [count_from_either_strategy, count_from_one_once_strategy,
                  count_from_one_twice_strategy,  min_strategy]
    
    # PART 3: These usually DO change:
    # IMPORTANT: REMEMBER TO CHANGE experiment_label, which is
    # used by the analyzer to label the results file. (Because we set
    # these by exec(), this has to have an extra set of "\"quotes\""
    # around it.)
    
    experiment_label = 0
    n_problems = 0
    DR_threshold = 0
    PERR = 0
    addend_matrix_offby1_delta = 0
    RETRIEVAL_LOW_CC = 0
    RETRIEVAL_HIGH_CC = 0
    STRATEGY_LOW_CC = 0
    STRATEGY_HIGH_CC = 0
    non_result_y_filler = 0
    INCR_on_RIGHT = 0
    DECR_on_WRONG = 0
    INCR_the_right_answer_on_WRONG = 0
    learning_rate = 0
    in_process_training_epochs = 0

    scan_spec = {"experiment_label": 
                 ["\"20151027: 33333333333333\""],
                 # Setting up the initial counting network
                 "initial_counting_network_burn_in_epochs": [1000], # 1000 based on 201509010902
                 "initial_counting_network_learning_rate": [0.25], # 0.25 based on 201509010902
                 # Problem presentation and execution
                 "n_problems": [1000],
                 "DR_threshold": [1.0], # WWW!!! Only used if dynamic_retrieval_on = True
                 "PERR": [0.0], # 0.1 confirmed 201509010826
                 "addend_matrix_offby1_delta": [0.0,1.0], # =1 will make the "next-to" inputs 0, =0 makes them 1, and so on
                 # Choosing to use retrieval v. a strategy
                 "RETRIEVAL_LOW_CC": [0.6], # Should be 0.6 usually; at 1.0 no retrieval will occur
                 "RETRIEVAL_HIGH_CC": [1.0], # Should be 1.0 usually
                 "STRATEGY_LOW_CC": [0.6], # If 1.0, strategies will be chosen randomly
                 "STRATEGY_HIGH_CC": [1.0],
                 # Learning target params
                 "non_result_y_filler": [0.0], # Set into all outputs EXCEPT result, which is adjusted by INCR_RIGHT and DECR_WRONG
                 "INCR_on_RIGHT": [1.0], # Added to non_result_y_filler at the response value when you get it right.
                 "DECR_on_WRONG": [-1.0], # Substrated from non_result_y_filler at the response value when you get it right.
                 "INCR_the_right_answer_on_WRONG": [0.0], # Added to non_result_y_filler at the CORRECT value when you get it WRONG.
                 "learning_rate": [0.1], # Explored 201509010826
                 "in_process_training_epochs": [10] # Number of training epochs on EACH test problem (explored 201509010826)
                 }

# The fns addends_matrix and sum_matrix create the input and output
# arrays that get appened up into training matrices by the caller (in
# driver).
#
# Transform two addends into a distributed representation input array,
# e.g., for a representation of 3 + 4, the input array created by
# addends_matrix) is:
#
# Index:         0 , 1 , 2 ,   3 ,   4 , 5 , 6 , 7 , 8 , 9 , 10 ,   11 , 12 ,   13 , 14 
# Input array: [ 0 , 0 , 0.5 , 1 , 0.5 , 0 , 0 , 0 , 0 , 0 ,  0 ,  0.5 ,  1 ,  0.5 ,  0 ]
#
# The surrounding 0.5s are supposed to represent children's confusion
# about the number actually stated. (Or about how to xform the stated
# number into the exact internal representation).
#
# And the output array (created by sum_matrix) is:
#
# Index:         0 , 1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11 , 12 , 13 , 14 
# Output array:[ 0 , 0 , 0 , 1 , 0 , 0 , 0 , 0 , 0 , 0 ,  0 ,  0 ,  1 ,  0 ,  0 ]
#
# WWW WARNING !!! Don't confuse these with the fingers on the hands!

def addends_matrix(a1, a2):
    lis = [0] * 14
    # First addend
    lis[a1 - 1] = 1 - settings.addend_matrix_offby1_delta
    lis[a1] = 1
    lis[a1 + 1] = 1 - settings.addend_matrix_offby1_delta
    # Second addend
    lis[a2 + 6] = 1 - settings.addend_matrix_offby1_delta
    lis[a2 + 7] = 1
    lis[a2 + 8] = 1 - settings.addend_matrix_offby1_delta
    return lis

def sum_matrix(s):
    lis = [0] * (13 + len(settings.strategies))
    lis[s] = 1
    return lis

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

def y_index(a1, a2):
    return 5 * (a1 - 1) + (a2 - 1)

class NeuralNetwork:
    def __init__(self, layers):
        self.errr=[]
        self.activation = lambda x: numpy.tanh(x)
        self.activation_prime = lambda x: 1.0 - x**2

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
        # initial input, counting numbers. 
        for i in range(1, 6):
            for j in range(1, 6):
                self.X.append(addends_matrix(i, j))
        self.X = numpy.array(self.X)
        self.predictions = []

    # Main forward feeding/backpropagation part

    def fit(self, X, y, learning_rate, epochs):
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

    # Outputs a matrix given an input matrix, this is used heavily
    # when we want to "know" what is in the kid's mind

    def predict(self, x):
        a = numpy.concatenate((numpy.ones(1).T, numpy.array(x)), axis=1)
        for l in range(0, len(self.weights)):
            a = self.activation(numpy.dot(a, self.weights[l]))
        return a

    # Returns a function that picks a random result from a list of
    # results above the confidence criterion this is used for the
    # retrieval. when we want to try to retrieve a sum, for example 3
    # + 4 = 7, we pass in a1 = 3, a2 = 4, beg = 0, and end = 13 guess
    # loops through [beg,end) to see the values that are above the cc,
    # and chooses a random number from those values. if there are
    # none, it returns none.  it does the same thing for when we want
    # to retrieve a strategy, except beg = 13, and end = 13 +
    # len(strategies)

    def try_memory_retrieval(self, sub_nn, a1, a2):
        index = y_index(a1, a2)
        if (a1 > 5) or (a2 > 5):
            return None
        # Collect the values that come above cc.
        results_above_cc = [x for x in range(sub_nn.beg, sub_nn.end) if self.predictions[index][x] > sub_nn.cc]
        l = len(results_above_cc)
        if l > 0:
            # At the moment this chooses randomly from all those
            # (either strats or results) above the respective cc,
            # although this could be changed to choose in a weighted
            # manner. FFF ???
            return int(results_above_cc[randint(0, l - 1)])
        return None

    # Used for analysis output, this just gets the prediction values
    # for a particular sum. FFF Maybe this could be used inside guess?
    # FFF Anyway, see notes for guess to explain the begin and end
    # things.

    def guess_vector(self, a1, a2, beg, end):
        vec = []
        self.predict(addends_matrix(a1, a2))
        for i in range(beg, end):
            vec.append(round(self.predictions[y_index(a1, a2)][i], 5))
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
        self.target.append([settings.non_result_y_filler] * (13 + len(settings.strategies)))
        self.target = numpy.array(self.target)

    def update_target(self, sub_nn, our_ans, ans, a1,a2):
        self.X = []
        self.X.append(addends_matrix(a1, a2))
        self.X = numpy.array(self.X)

        if a1 + a2 == our_ans:
            # RIGHT
            self.target[0][ans] += settings.INCR_on_RIGHT
        else:
            # WRONG
            self.target[0][ans] -= settings.DECR_on_WRONG
            self.target[0][a1+a2] += settings.INCR_the_right_answer_on_WRONG

def dump_nn_results_predictions():
    writer.writerow(['===== Results NN Prediction table ======'])
    for i in range(1, 6):
        for j in range(1, 6):
            writer.writerow(["%s + %s = " % (i, j)] + nn.guess_vector(i, j, 0, 13))
    writer.writerow(['========================================'])

def gen_file_name():
    file_name = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
    full_file__name = os.path.join(os.path.join(os.path.dirname(__file__), 'test_csv'), file_name + '.csv')
    return full_file__name

def is_dump_time(i):
    return i % settings.pbs == 0 or i == settings.n_problems - 1

# Set up the neural network fitted to kids' having learned how to
# count before we got here, so there is a tendency for problems what
# look like 3+4 to result in saying 5. To do this we burn in a set of
# I/O relationships that have this tendency.

def counting_network():
    writer.writerow(['Network created', 'hidden_units', settings.hidden_units, 'learning_rate',
                     settings.initial_counting_network_learning_rate])
    input_units = 14  # Addends + 1 on either side of each for
    # distributed representation -- see code in
    # NeuralNetwork.py for more detail.
    output_units = 13 + len(settings.strategies)
    lnn = NeuralNetwork([input_units, settings.hidden_units, output_units])
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
    writer.writerow(['Burning in counting results', 'burn_in_epochs', settings.initial_counting_network_burn_in_epochs])
    lnn.fit(X_count, y_count, settings.initial_counting_network_learning_rate,
           settings.initial_counting_network_burn_in_epochs)
    lnn.update_predictions()
    return lnn

# We first try a retrieval on the sum, and if that fails we have to
# use a strategy, which we try to retrieve and if that fails we choose
# a random strategy. Then we update the nn accordingly, and fit and
# update_y this is the main driver within driver that does the testing

class subNeuralNetwork:
    def __init__(self, type):
        # initializing member variables
        self.low_cc, self.high_cc, self.beg, self.end = -1, -1, -1, -1
        exec ("self.low_cc = settings." + type + "_LOW_CC")
        exec ("self.high_cc = settings." + type + "_HIGH_CC")
        if type == "RETRIEVAL":
            self.beg = 0
            self.end = 13
        elif type == "STRATEGY":
            self.beg = 13
            self.end = 13 + len(settings.strategies)
        else:
            print 'ERROR'
        self.cc = self.gen_cc()

    def gen_cc(self):
        return self.low_cc + (self.high_cc - self.low_cc) * random()

def exec_strategy():
    global SOLUTION
    nn.reset_target()
    PPA()  # Create a random problem: sets the global ADDEND to an Addend object
    # create the sub nn, which are used as parameters into the main nn for easier updating/retrieval
    add_nn = subNeuralNetwork("RETRIEVAL")
    strat_nn = subNeuralNetwork("STRATEGY")
    # try getting a random number from a list above the confidence criterion
    retrieval = nn.try_memory_retrieval(add_nn,ADDEND.ad1,ADDEND.ad2)
    SOLUTION = -666
    # Used to be 0, but why is this needed?! 
    # (DDD If this shows up, there's something really wrong!) 
    # (this is just used to initialize solution, or else it's not in the right code block
    # we have to reset the target for every problem, 
    # or else it uses the target from the last problem
    if retrieval is not None:
        SOLUTION = retrieval
        writer.writerow(["used", "retrieval", ADDEND.ad1, ADDEND.ad2, SOLUTION])
    else:
        # retrieval failed, so we get try to get a strategy from above the confidence criterion and use hands to add
        strat_num = nn.try_memory_retrieval(strat_nn,ADDEND.ad1,ADDEND.ad2)
        if strat_num is None:
            strat_num = randint(0, len(settings.strategies) - 1)
        else:
            strat_num = strat_num - 13  # Remove the offset from the nn
        SOLUTION = exec_explicit_strategy(settings.strategies[strat_num])
        # !!! WWW WARNING (for analysis): This gets displayed even if
        # Dynamic Retrieval was used. You have to Analyze this
        # distinction out of the log at the end by seeing that a DR
        # message appeared!
        writer.writerow(["used", settings.strategies[strat_num], ADDEND.ad1, ADDEND.ad2, SOLUTION])
        # update the target based on if the strategy worked or not
        nn.update_target(strat_nn, SOLUTION, strat_num + 13,ADDEND.ad1,ADDEND.ad2)
    # update the target based on if the sum is correct or not
    nn.update_target(add_nn, SOLUTION, ADDEND.ad1 + ADDEND.ad2,ADDEND.ad1,ADDEND.ad2)
    nn.fit(nn.X, nn.target, settings.learning_rate, settings.in_process_training_epochs)
    # update predictions in case we want to print
    nn.update_predictions()

def init_neturalnets():
    global nn
    nn = counting_network()

def present_problems():
    for i in range(settings.n_problems):
        exec_strategy()
        if is_dump_time(i):
            dump_nn_results_predictions()

# Execute with all the possible values of each parameter, scanned
# recursively.
def config_and_test(index=0):
    global scan_spec_keys, writer
    if index < len(scan_spec_keys):  # Any more scan_spec_keys to scan?
        # Get the current param_values, for instance: epochs = [100,200,300]
        # 100 200 and 300 are param+values
        for param_value in settings.scan_spec[scan_spec_keys[index]]:
            # Jeff's Ugly lisp-like metaprogramming: Set the param
            # value, e.g., epoch = 100, then recurse to the next index
            print ("settings." + scan_spec_keys[index] + '=' + str(param_value))
            exec ("settings." + scan_spec_keys[index] + '=' + str(param_value))
            config_and_test(index + 1)  # Next param (recursive!)
    else:
        # Finally we have a set of choices, do it:
        fn=gen_file_name()
        print("----------------- "+ fn + " -----------------")
        with open(fn, 'wb') as csvfile:
            # initialize the writer and neural network for each config we want to test
            writer = csv.writer(csvfile)
            writer.writerow(['Output Format Version', '20150813'])
            init_neturalnets()
            present_problems()
            # Output params
            writer.writerow([' ======= Run Parameters ======='])
            for key in settings.scan_spec:
                exec ("foo = settings." + key)
                writer.writerow([key, foo])

def top_level_run():
    global scan_spec_keys, settings, hidden_units
    start = timeit.default_timer()
    # Used in the recursive config_and_test fn.
    settings = Settings()
    scan_spec_keys = settings.scan_spec.keys()
    print "Parameter spec:"
    print scan_spec_keys
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
