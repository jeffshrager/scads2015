from random import randint
import numpy as np
import os
import csv
import matplotlib.pyplot as plt
import NeuralNetwork as nn1
import ADD
import datetime
import timeit
import settings

global add_strat_nn


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
            #trp(1, "Addends (%s+%s) is/are larger than the distribution table limits -- Ignored!" % (a1, a2))
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

    # Print the table.

    def show(self, relative=True):
        table = self.relative_table(relative)
        full_name = os.path.join(os.path.join(os.path.dirname(__file__), 'test_txt'), file_name + '.txt')
        f = open(full_name, 'w')
        f.write('N_PROBLEMS: ' + str(settings.n_problems) + '\n')
        f.write('EPOCHS: ' + str(settings.epoch) + '\n')
        f.write('LEARNING_RATE: ' + str(settings.learning_rate) + '\n')
        f.write('INCR_RIGHT: ' + str(settings.INCR_RIGHT) + '\n')
        f.write('STRATEGIES: ' + str(settings.strategies) + '\n')
        for i in range(1, 6):
            for j in range(1, 6):
                f.write("%s + %s = " % (i, j)),
                for k in range(13):
                    f.write("%s (%0.03f), " % (k, table[i][j][k])),
                f.write('\n')
            f.write('\n')

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

    # Plot the distribution table into bar charts.

    def bar_plot(self, relative=False):
        if relative:
            table = [[[self.relative_frequency1(x, y, z) for z in range(13)] for y in range(6)] for x in range(6)]

        else:
            table = [[[self.table[x][y][z] for z in range(13)] for y in range(6)] for x in range(6)]

        maxheight = max([max([max(table[x][y]) for x in range(6)]) for y in range(6)])

        plt.figure()

        for i in range(1, 6):
            for j in range(1, 6):
                ax = plt.subplot(5, 5, (i - 1) * 5 + j)
                plt.bar([x - 0.4 for x in range(13)], table[i][j], linewidth=0, color="steelblue")
                plt.xlim(-0.5, 12.5)
                plt.ylim(0, maxheight * 1.1)
                plt.text(.5, 1.03, "%s + %s" % (i, j), horizontalalignment='center', transform=ax.transAxes)
                plt.tick_params( \
                    axis='both',
                    which='both',
                    bottom='off',
                    top='off',
                    left='off',
                    right='off',
                    labelleft='on',
                    labelbottom='on', labelsize=8)
        plt.tight_layout(h_pad=1)
        plt.show()

# we first try a retrieval on the sum, and if that fails we have to use a strategy, which we try to retrieve
# , and if that fails we gotta use a random strategy. then we update the nn accordingly, and fit and update_y
def exec_strategy():
    global writer
    global strat_list
    ADD.PPA()
    # try getting a random number from a list above the confidence criterion
    retrieval = add_strat_nn.guess(ADD.ADDEND.ad1, ADD.ADDEND.ad2, 0, 13)
    SOLUTION = 0
    if retrieval is not None:
        #trp(1, "Used Retrieval")
        SOLUTION = retrieval
        writer.writerow(["used","retrieval", ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION])
    else:
        # retrieval failed, so we get try to get a strategy from above the confidence criterion and use hands to add
        strat_num = add_strat_nn.guess(ADD.ADDEND.ad1, ADD.ADDEND.ad2, 13, 13 + len(settings.strategies))
        if strat_num is None:
            strat_num = randint(0, len(settings.strategies) - 1)
        else:
            strat_num -= 13
        SOLUTION = ADD.exec_strategy(settings.strategies[strat_num])
        writer.writerow(["used",settings.strategies[strat_num], ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION])
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

    # Output tables and charts.
    # DSTR.show(relative=True)  # Useful for debugging, but most analysis is now done by code.
    DSTR.print_csv(relative=True)
    # DSTR.bar_plot(relative=True)


# sets up the neural network fitted to counting
def counting_network(hidden_units=30, learning_rate=0.15):
    # this is the addends matrix
    input_units = 14
    # this is the output matrix
    output_units = 13 + len(settings.strategies)

    # fits to counting network, the inputs are the addends matrix for (1+2) , (2+3), etc and the outputs are (1+2)=3 (2+3)=4
    NN = nn1.NeuralNetwork([input_units, hidden_units, output_units])
    X_count = []
    y_count = []
    for i in range(1, 5):
        X_count.append(nn1.addends_matrix(i, i + 1))
        y_count.append(nn1.sum_matrix(i + 2))
    X_count = np.array(X_count)
    y_count = np.array(y_count)
    NN.fit(X_count, y_count, learning_rate, 15000)
    NN.update_y()
    return NN


# Depth first search through all the possible configurations of parameters
def config_and_test(index):
    global file_name, DSTR, add_strat_nn, writer, scan_spec
    # checks if we have any more params to scan
    if index < len(params):
        # gets the current param, for instance epochs: [100,200,300] 100 200 and 300 are param
        for param in scan_spec[params[index]]:
            # metaprogramming stuff, just sets the param value, e.g. epoch = 100, then recurses down to the next index
            exec (params[index] + '=' + str(param))
            config_and_test(index + 1)
    else:
        print settings.strategies
        # this part is broken, it should print what the current params are but I can't get it to work
        print settings.scan_spec
        # the file name is the current time
        file_name = datetime.datetime.now().strftime("%Y%m%d%H%M%S")
        full_name = os.path.join(os.path.join(os.path.dirname(__file__), 'test_csv'), file_name + '.csv')

        with open(full_name, 'wb') as csvfile:
            # initialize the writer, DSTR, neural network for each config we want to test
            writer = csv.writer(csvfile)
            writer.writerow(['Output Format Version', '20150807'])
            DSTR = Distribution()
            ADD.main()
            add_strat_nn = counting_network()
            test(settings.n_problems)


def main():
    global TL, params, scan_spec
    scan_spec = settings.scan_spec
    start = timeit.default_timer()
    TL = 0  # trace level, 0 means off
    params = scan_spec.keys()
    for i in range(settings.ndups):
        config_and_test(0)
    stop = timeit.default_timer()
    print stop - start


if __name__ == "__main__":
    main()
