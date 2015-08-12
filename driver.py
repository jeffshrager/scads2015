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


# Answer distribution table

class Distribution(object):
    # Record answers ranging from 0 to 11; 12 includes all other answers.

    def __init__(self):
        self.table = [[[0 for x in range(13)] for x in range(6)] for x in range(6)]

    # Update the distribution table when a new answer is generated.

    def update(self, eq):
        # eq is [a1,a2,result]
        a1 = eq[0]
        a2 = eq[1]
        result = eq[2]
        if (a1 > 5) or (a2 > 5):
            # trp(1, "Addends (%s+%s) is/are larger than the distribution table limits -- Ignored!" % (a1, a2))
            return

        if result not in range(12):
            self.table[a1][a2][12] += 1
        else:
            self.table[a1][a2][result] += 1

    # Calculate relative frequency, return blank string when frequency is zero
    # so that the table looks clean when printed.

    def relative_frequency(self, a1, a2, result):
        s = sum(self.table[a1][a2])
        if (s == 0) or (self.table[a1][a2][result] == 0):
            return ''
        else:
            return round(float(self.table[a1][a2][result]) / s, 2)

    # Same function but return zero when frequency is zero so that
    # it can be plotted into graphs.

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

    def print_csv(self, relative=False):
        global writer, scan_spec
        table = self.relative_table(relative)

        writer.writerow(['======================================='])

        for key in scan_spec:
            exec ("foo = " + key)
            writer.writerow([key, foo])

        writer.writerow(['======================================='])

        for i in range(1, 6):
            for j in range(1, 6):
                writer.writerow(["%s + %s = " % (i, j)] + [table[i][j][k] for k in range(13)])


# we first try a retrieval on the sum, and if that fails we have to use a strategy, which we try to retrieve
# and if that fails we gotta use a random strategy. then we update the nn accordingly, and fit and update_y
# this is the main driver within driver that does the testing
def exec_strategy():
    global writer
    global strat_list
    ADD.PPA()
    # try getting a random number from a list above the confidence criterion
    cc = settings.RETRIEVAL_LOW_CC + (settings.RETRIEVAL_HIGH_CC - settings.RETRIEVAL_LOW_CC) * random()
    strat_cc = settings.STRATEGY_LOW_CC + (settings.STRATEGY_HIGH_CC - settings.STRATEGY_LOW_CC) * random()
    retrieval = add_strat_nn.guess(ADD.ADDEND.ad1, ADD.ADDEND.ad2, 0, 13, cc)
    SOLUTION = 0
    if retrieval is not None:
        # trp(1, "Used Retrieval")
        SOLUTION = retrieval
        writer.writerow(["used", "retrieval", ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION])
    else:
        # retrieval failed, so we get try to get a strategy from above the confidence criterion and use hands to add
        strat_num = add_strat_nn.guess(ADD.ADDEND.ad1, ADD.ADDEND.ad2, 13, 13 + len(settings.strategies), strat_cc)
        if strat_num is None:
            strat_num = randint(0, len(settings.strategies) - 1)
        else:
            strat_num -= 13
        SOLUTION = ADD.exec_strategy(settings.strategies[strat_num])
        # !!! WWW WARNING: This gets displayed even if Dynamic
        # Retrieval was used. You have to Analyze this distinction out
        # of the log at the end by seeing that a DR message appeared!
        writer.writerow(["used", settings.strategies[strat_num], ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION])
        # update the neural networks based on if the strategy worked or not
        add_strat_nn.update(ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION, 13 + strat_num, 13, 13 + len(settings.strategies))
    add_strat_nn.update(ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION, ADD.ADDEND.ad1 + ADD.ADDEND.ad2, 0, 13)
    add_strat_nn.fit(add_strat_nn.X, add_strat_nn.y, settings.learning_rate, settings.epoch)
    add_strat_nn.update_y()
    # add method here to get what strategy is used

    return [ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION]


def test(n_times):
    # Repeat n times.
    for i in range(n_times):
        eq = exec_strategy()
        DSTR.update(eq)
    # Output tables for analysis:
    DSTR.print_csv(relative=True)

# Set up the neural network fitted to kids' having learned how to
# count before we got here, so there is a tendency for problems what
# look like 3+4 to result in saying 5. To do this we burn in a set of
# I/O relationships that have this tendency.

def counting_network(hidden_units=30, learning_rate=0.15):
    input_units = 14 # Addends + 1 on either side of each for
                     # distributed representation -- see code in
                     # NeuralNetwork.py for more detail.
    output_units = 13 + len(settings.strategies)
    NN = nn1.NeuralNetwork([input_units, hidden_units, output_units])
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
    NN.fit(X_count, y_count, learning_rate, 15000)
    NN.update_y()
    return NN

# Execute with all the possible values of each parameter, scanned
# recursively.

def config_and_test(index=0):
    global file_name, DSTR, add_strat_nn, writer, scan_spec, param_keys
    if index < len(param_keys): # Any more param_keys to scan?
        # Get the current param_values, for instance: epochs = [100,200,300]
        # 100 200 and 300 are param+values
        for param_value in scan_spec[param_keys[index]]:
            # Jeff's Ugly lisp-like metaprogramming: Set the param
            # value, e.g., epoch = 100, then recurse to the next index
            exec (param_keys[index] + '=' + str(param_value))
            print (param_keys[index] + '=' + str(param_value))
            config_and_test(index + 1) # Next param (recursive!)
    else: # Finally we have a set of choices, do it!
        print "---Running!---"
        file_name = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
        full_name = os.path.join(os.path.join(os.path.dirname(__file__), 'test_csv'), file_name + '.csv')
        with open(full_name, 'wb') as csvfile:
            # initialize the writer, DSTR, neural network for each config we want to test
            writer = csv.writer(csvfile)
            writer.writerow(['Output Format Version', '20150807'])
            # Set up the NN
            DSTR = Distribution()
            ADD.main() 
            add_strat_nn = counting_network() # Burn in the counting network (3+4=5)
            test(settings.n_problems) # Now run the real experiment!

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
        print ">>>>> Rep #"+ str(i+1)+" <<<<<"
        config_and_test()
    stop = timeit.default_timer()
    print stop - start


if __name__ == "__main__":
    main()
