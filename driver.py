from random import randint, random
import numpy as np
import os
import csv
import matplotlib.pyplot as plt
import NeuralNetwork as nn1
import ADD
import datetime
import timeit
import settings


def trp(tl, text):
    if TL > tl:
        print text


# The Distribution table records every answer given to every problem
# as they are created, and is output into the log at the end of the
# run. It live in the global DSTR. The problem with this as the only
# record of results is it's historicity; that is, it's covering not
# only the final results, but also the results at the beginning. As a
# result (pun!) of this fact, you essentially can't ever reach
# completely correct results distribution (unless you run WAY over the
# end of where the answers are all perfect, thereby masking the early
# mistakes in a swamp of correct answers). The prediction table is
# much more useful in assessing the state of knowledge of the system
# at any given moment.

class Distribution(object):
    # Record answers ranging from 0 to 11; 12 includes all other answers.

    def __init__(self):
        self.table = [[[0 for x in range(13)] for x in range(6)] for x in range(6)]

    # Update the distribution table when a new answer is generated.

    def update(self, a1, a2, result):
        if (a1 > 5) or (a2 > 5):
            return

        # Anything not within range(12) [i.e., 0-11] gets scored as [12]
        if result not in range(12):
            self.table[a1][a2][12] += 1
        else:
            self.table[a1][a2][result] += 1

    # Calculate relative frequency, return blank string when frequency
    # is zero so that the table looks clean when printed.

    def relative_frequency(self, a1, a2, result):
        s = sum(self.table[a1][a2])
        if (s == 0) or (self.table[a1][a2][result] == 0):
            return ''
        else:
            return round(float(self.table[a1][a2][result]) / s, 2)

    # Same function but return zero when frequency is zero so that it
    # can be plotted into graphs.

    def relative_frequency1(self, a1, a2, result):
        s = sum(self.table[a1][a2])
        if s == 0:
            return 0
        else:
            return float(self.table[a1][a2][result]) / s

    # Convert the whole frequency table to relative frequency.

    def relative_table(self, relative):
        if relative:
            return [[[self.relative_frequency1(x, y, z) for z in range(13)] for y in range(6)] for x in range(6)]
        else:
            return [[[self.table[x][y][z] for z in range(13)] for y in range(6)] for x in range(6)]

    # Export to csv file.

    def print_csv(self, relative=True):
        global writer, scan_spec
        table = self.relative_table(relative)
        writer.writerow([' ======= Run Parameters ======='])
        for key in scan_spec:
            exec ("foo = " + key)
            writer.writerow([key, foo])
        writer.writerow(['===== Results Distribution Table ======='])
        for i in range(1, 6):
            for j in range(1, 6):
                writer.writerow(["%s + %s = " % (i, j)] + [table[i][j][k] for k in range(13)])

# We first try a retrieval on the sum, and if that fails we have to
# use a strategy, which we try to retrieve and if that fails we choose
# a random strategy. Then we update the nn accordingly, and fit and
# update_y this is the main driver within driver that does the testing

def gen_cc(low_cc, high_cc):
    return low_cc + (high_cc - low_cc) * random()

def exec_strategy():
    global writer, DSTR
    ADD.PPA() # Create a random problem: sets the global ADDEND to an Addend object
    # try getting a random number from a list above the confidence criterion
    cc = gen_cc(settings.RETRIEVAL_LOW_CC, settings.RETRIEVAL_HIGH_CC)
    # Note that sum_guess ends up being a function! (Which seems really weirdly un-necessarily complex!!!)
    sum_guess = add_strat_nn.create_guess_in_range(0, 13) 
    retrieval = sum_guess(cc)
    SOLUTION = -666 # Used to be 0, but why is this needed?! (DDD If this shows up, there's something really wrong!)
    if retrieval is not None:
        SOLUTION = retrieval
        writer.writerow(["used", "retrieval", ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION])
    else:
        # retrieval failed, so we get try to get a strategy from above the confidence criterion and use hands to add
        strat_cc = gen_cc(settings.STRATEGY_LOW_CC, settings.STRATEGY_HIGH_CC)
        strat_guess = add_strat_nn.create_guess_in_range(13, 13 + len(settings.strategies))
        strat_num = strat_guess(strat_cc)
        if strat_num is None:
            strat_num = randint(0, len(settings.strategies) - 1) 
        else:
            strat_num = strat_num-13 # Remove the offset from the nn
        SOLUTION = ADD.exec_strategy(settings.strategies[strat_num])
        # !!! WWW WARNING (for analysis): This gets displayed even if
        # Dynamic Retrieval was used. You have to Analyze this
        # distinction out of the log at the end by seeing that a DR
        # message appeared!
        writer.writerow(["used", settings.strategies[strat_num], ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION])
        # update the neural networks based on if the strategy worked or not
        strat_update = add_strat_nn.create_update_in_range(13, 13 + len(settings.strategies))
        strat_update(SOLUTION, strat_num + 13)
    sum_update = add_strat_nn.create_update_in_range(0, 13)
    sum_update(SOLUTION, ADD.ADDEND.ad1 + ADD.ADDEND.ad2)
    add_strat_nn.fit(add_strat_nn.X, add_strat_nn.y, settings.learning_rate, settings.epoch)
    add_strat_nn.update_y()
    DSTR.update(ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION)


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
    NN = nn1.NeuralNetwork([input_units, settings.hidden_units, output_units])
    # Create the counting examples matrix k, the inputs are the
    # addends matrix for (1+2) , (2+3), etc and the outputs are
    # (1+2)=3 (2+3)=4.
    X_count = []
    y_count = []
    for i in range(1, 5):
        X_count.append(nn1.addends_matrix(i, i + 1))
        y_count.append(nn1.sum_matrix(i + 2))
    X_count = np.array(X_count)
    y_count = np.array(y_count)
    # Now burn it in:
    writer.writerow(['Burning in counting results', 'burn_in_epochs', settings.initial_counting_network_burn_in_epochs])
    NN.fit(X_count, y_count, settings.initial_counting_network_learning_rate,
           settings.initial_counting_network_burn_in_epochs)
    NN.update_y()
    return NN


def dump_nn_results_predictions():
    writer.writerow(['===== Results NN Prediction table ======'])
    for i in range(1, 6):
        for j in range(1, 6):
            writer.writerow(["%s + %s = " % (i, j)] + add_strat_nn.guess_vector(i, j, 0, 13))
    writer.writerow(['========================================'])


def init_problem_globals():
    global add_strat_nn, DSTR
    DSTR = Distribution()
    ADD.main()
    add_strat_nn = counting_network()  # Burn in the counting network (3+4=5)


def gen_file_name():
    file_name = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
    full_file__name = os.path.join(os.path.join(os.path.dirname(__file__), 'test_csv'), file_name + '.csv')
    return full_file__name


def is_dump_time(i):
    return i % settings.pbs == 0 or i == settings.n_problems - 1


# Execute with all the possible values of each parameter, scanned
# recursively.

def config_and_test(index=0):
    global scan_spec, param_keys
    if index < len(param_keys):  # Any more param_keys to scan?
        # Get the current param_values, for instance: epochs = [100,200,300]
        # 100 200 and 300 are param+values
        for param_value in scan_spec[param_keys[index]]:
            # Jeff's Ugly lisp-like metaprogramming: Set the param
            # value, e.g., epoch = 100, then recurse to the next index
            exec (param_keys[index] + '=' + str(param_value))
            print (param_keys[index] + '=' + str(param_value))
            config_and_test(index + 1)  # Next param (recursive!)
    else:  # Finally we have a set of choices, do it!
        test_n_times(settings.n_problems)


def test_n_times(n_times):
    global writer
    print "---Running!---"
    with open(gen_file_name(), 'wb') as csvfile:
        # initialize the writer, DSTR, neural network for each config we want to test
        writer = csv.writer(csvfile)
        writer.writerow(['Output Format Version', '20150813'])
        init_problem_globals()
        for i in range(n_times):
            exec_strategy()
            if is_dump_time(i):
                dump_nn_results_predictions()
        # Output tables for analysis
        DSTR.print_csv()


def main():
    global TL, param_keys, scan_spec
    start = timeit.default_timer()
    TL = 0  # trace level, 0 means off
    # Used in the recursive config_and_test fn.
    scan_spec = settings.scan_spec
    param_keys = scan_spec.keys()
    print "Paremeter spec:"
    print settings.scan_spec
    print "Strategies in play:"
    print settings.strategies
    print "-----"
    for i in range(settings.ndups):
        print ">>>>> Rep #" + str(i + 1) + " <<<<<"
        config_and_test()
    stop = timeit.default_timer()
    print stop - start
