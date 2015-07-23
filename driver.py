from random import randint, random
import numpy as np
import os
import csv
import matplotlib.pyplot as plt
import NeuralNetwork as nn1
import ADD
import datetime
import timeit


def trp(tl, text):
    if TL > tl:
        print text

# Associative Problem->Solution Memory (APSM) For the moment
# this will just a table that counts the number of times each
# problem leads to each solution. APSM will be pre-loaded with
# a small tendency to recall n+1 for m+n problems (i.e., 3+4
# give a small bump to 5).


''' 960514 - Added direct recall memory.  This is stored in an
associative array, just like in Siegler & Shrager, and is updated
at problem conclusion time, just like Siegler and Shrager.  And,
just like... it is consulted ... um, okay, so this is different;
it's consulted first, and then only when the cognitive system has
some time with nothing to do.  Note that the array is actually one
wider in each direction than the possible solutions; the zero index
isn't used. '''


class Apsm(object):
    def __init__(self):
        self.table = [[[0.01 for x in range(11)] for x in range(6)] for x in range(6)]
        self.y = []


        # generate the table so it contains reference to mutable list y, thus changing the table when we update will also change y for when we do the fit (hopefully)
        y_index = 0
        for i in range(1, 6):
            for j in range(1, 6):
                self.y.append(nn.predict(nn1.addends_matrix(i, j)))
                self.table[i][j] = self.y[y_index]
                y_index += 1

    # When the problem is completed, update the memory table
    # appropriately depending upon whether we got it right or wrong.
    # Ignore the results of challenge problems.
    # then update the neural network

    def update(self, eq):
        global epoch, learning_rate
        # eq is [a1,a2,result]
        a1 = eq[0]
        a2 = eq[1]
        result = eq[2]

        if (a1 > 5) or (a2 > 5) or (result > 10):
            trp(1, "Addends (%s+%s) or result (%s) is/are larger than the memory table limits -- Ignored!" % (
            a1, a2, result))
        else:
            if a1 + a2 == result:
                self.table[a1][a2][a1 + a2] += INCR_RIGHT
            else:
                self.table[a1][a2][a1 + a2] += INCR_WRONG
        nn.fit(X_count, np.array(self.y),learning_rate, epoch)

    # Print the table.

    def show(self):
        for i in range(1, 6):
            for j in range(1, 6):
                print "%s + %s = " % (i, j),
                for k in range(1, 11):
                    print "%s (%s), " % (k, self.table[i][j][k]),
                print
            print

    # Pick at random from among the results that come above the cc, or
    # return None if nothing comes over the cc.

    def guess(self, a1, a2):
        if (a1 > 5) or (a2 > 5):
            return (None)

        cc = RETRIEVAL_LOW_CC + (RETRIEVAL_HIGH_CC - RETRIEVAL_LOW_CC) * random()
        trp(1, "Choose confidence criterion = %s" % cc)

        results_above_cc = []

        for i in range(1, 11):
            if self.table[a1][a2][i] >= cc:
                results_above_cc.append(i)

        l = len(results_above_cc)
        if l > 0:
            return (results_above_cc[randint(0, l - 1)])
        return (None)


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
            trp(1, "Addends (%s+%s) is/are larger than the distribution table limits -- Ignored!" % (a1, a2))
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
        full_name = os.path.join(os.path.join(os.path.dirname(__file__),'test_txt'),file_name + '.txt')
        f = open(full_name, 'w')
        f.write('EPOCHS: ' + str(epoch) + '\n')
        f.write('LEARNING_RATE: ' + str(learning_rate) + '\n')
        f.write('INCR_RIGHT: ' + str(INCR_RIGHT) + '\n')
        f.write('STRATEGY: ' + str(strategy) + '\n')
        for i in range(1, 6):
            for j in range(1, 6):
                f.write("%s + %s = " % (i, j)),
                for k in range(13):
                    f.write("%s (%0.03f), " % (k, table[i][j][k])),
                f.write('\n')
            f.write('\n')

    # Export to csv file.

    def print_csv(self, relative=False):
        table = self.relative_table(relative)
        full_name = os.path.join(os.path.join(os.path.dirname(__file__),'test_csv'),file_name + '.csv')
        with open(full_name, 'wb') as csvfile:
            writer = csv.writer(csvfile)
            writer.writerow(['EPOCHS: ', epoch, 'LEARNING_RATE: ', learning_rate])
            writer.writerow(['INCR_RIGHT: ', INCR_RIGHT, 'STRATEGY: ', strategy])
            writer.writerow(['TEST: ', ])
            writer.writerow(['PROBLEM', 'ANSWER'])
            writer.writerow([''] + [str(x) for x in range(12)] + ['OTHER'])
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

# try retrieval first, if it fails then
# Initialize hand, echoic buffer and
# counting buffer,and carry out the strategy.
# Update memory and distribution table at the end.

def exec_strategy(strategy_choice):
    retrieval = APSM.guess(ADD.ADDEND.ad1, ADD.ADDEND.ad2)
    SOLUTION = 0
    if retrieval is not None:
        trp(1, "Used Retrieval")
        SOLUTION = retrieval
    else:
        SOLUTION = ADD.exec_strategy(strategy_choice)

    return [ADD.ADDEND.ad1, ADD.ADDEND.ad2, SOLUTION]


def test(n_times, strategy_choice):
    # Repeat n times.

    for i in range(n_times):
        ADD.PPA()
        eq = exec_strategy(strategy_choice)
        APSM.update(eq)
        DSTR.update(eq)

    # Output the distribution table.

    DSTR.show(relative=True)
    DSTR.print_csv(relative=True)
    #DSTR.bar_plot(relative=True)

    # sets up the neural network fitted to counting


def counting_network(hidden_units=30, learning_rate=0.15):
    global X_count, y_count
    # this is the addends matrix
    input_units = 14
    # this is the output matrix
    output_units = 13

    # fits to counting network
    NN = nn1.NeuralNetwork([input_units, hidden_units, output_units])
    X_count = []
    y_count = []
    for i in range(1, 5):
        X_count.append(nn1.addends_matrix(i, i + 1))
        y_count.append(nn1.sum_matrix(i + 2))
    X_count = np.array(X_count)
    y_count = np.array(y_count)
    NN.fit(X_count, y_count, learning_rate)
    return NN


def main():
    global TL, RETRIEVAL_LOW_CC, RETRIEVAL_HIGH_CC
    global INCR_RIGHT, INCR_WRONG
    global APSM, DSTR
    global nn
    global epoch, learning_rate
    global file_name
    global strategy
    global test_num

    TL = 0  # trace level, 0 means off

    INCR_RIGHT = 6.00  # Add this to solution memory when you get a problem right
    INCR_WRONG = 0.03  # Add this when you get one wrong

    # Retrieval cc ranges are used in select-strategy to determine when
    # to actually choose retrieval (via setting the cc randomly).

    RETRIEVAL_LOW_CC = 0.1
    RETRIEVAL_HIGH_CC = 0.9

    start = timeit.default_timer()


    epoch = 100
    learning_rate = 0.1

    test_num = 1000
    strategy = ADD.count_from_either_strategy
    arr_of_learning_rates = [0.1,0.2,0.3]
    arr_of_epochs = [250, 500, 750]
    arr_of_incr_right = [2, 5, 8]

    test_num = 1000
    strategy = ADD.count_from_either_strategy


    for i in arr_of_epochs:
        for j in arr_of_incr_right:
            for k in arr_of_learning_rates:
                # initialize the neural network to be from 3+4=5 problems
                nn = counting_network()
                # Set up the solution memory table and the answer distribution table
                APSM = Apsm()
                DSTR = Distribution()
                ADD.main()

                epoch = i
                INCR_RIGHT = j
                learning_rate = k
                file_name = datetime.datetime.now().strftime("%Y%m%d%H%M%S")

                test(test_num,strategy)

    stop = timeit.default_timer()
    print stop-start



if __name__ == "__main__":
    main()
