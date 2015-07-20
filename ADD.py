from random import randint, shuffle, random
import numpy as np
import os
import csv
import matplotlib.pyplot as plt
import NeuralNetwork as nn1

# ----- Operators for actual addition strategies and test routines.

# General utilities for reporting, etc.

def trp(tl,text):
    if TL>tl:
        print text

# The peripheral system.This is a very simple representation of the
# stuff needed for the addition domains: ten "fingers", a focus of
# attention, and an echoic memory into which numbers are stored.
# Operations on these constitute the basic operations of the
# domain.

class Hand(object):
    
    def __init__(self):
        
        # The fingers memory structure; five on each of two hands, 
        # each of which my be up or down.
        
        self.s = {'left':['d']*5,'right':['d']*5}
        
        # The focus of attention.
        
        self.foa = {'hand':'left','finger':0}
        self.hand = 'left'
    
    def clear(self):
        self.s['left'] = ['d']*5
        self.s['right'] = ['d']*5
    
    # Basic operations; most of which operate on what's in foa.
    
    def increment_focus(self):
        if not((self.foa['hand']=='right') and (self.foa['finger']==4)):
        
            # If we're done the left hand, move on to the rigt.
            
            if (self.foa['hand']=='left') and (self.foa['finger']==4):
                self.foa['hand']='right'
                self.foa['finger']=0
                
            # Else shift to the next finger.
            
            else:
                self.foa['finger']+=1
        self.report()
    
    # This is just a reporting function (and helpers).  The fingers are
    # shown up (u) or down (d) for each hand, and the one begin attended to
    # is capitalized.
    
    def report(self):
        text=''
        for i in ['left','right']:
            for j in range(5):
                if i == self.foa['hand'] and j == self.foa['finger']:
                    text+=self.s[i][j].upper()
                else:
                    text+=self.s[i][j]   
        text=text[:5]+'|'+text[5:]
        trp(4,text)
    
    # Finger raising; always does the focussed finger.
    
    def put_up(self):
        self.s[self.foa['hand']][self.foa['finger']] = 'u'

    # The hands are external components as well, so 
    # that all you need to do is select a hand and switch hands.

    def choose(self):
        if randint(0,1)==0:
            self.hand='left'
        else:
            self.hand='right' 
        trp(3,'Looking to the %s hand.' % self.hand)
        self.foa['hand'] = self.hand
        self.foa['finger'] = 0
        self.report()
    
    def swap(self):
        if self.hand == 'left':
            self.hand='right'
            #mempush(swap-hands, from left to right)
        else:
            self.hand='left'
            #mempush(swap-hands, from right to left)
        trp(3,'Looking to the %s hand.' % self.hand)
        self.foa['hand'] = self.hand
        self.foa['finger'] = 0
        self.report()

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
    trp(2,'<%s>' % n)
    global EB
    EB = n
    #mempush(say, list n) 

def say_next():
    if EB==0:
        say(1)
    elif PERR>random():
        say(EB)
    else:
        say(EB+1)

# Clear EB each time you're about to start a count off.  If you
# don't do this, the last number counted will be left in the echoic
# buffer and you'll count on from it, which is actually right, of
# course, for shortcut-sum.

def clear_eb():
    global EB
    EB = 0

# This tells the driver to stop.

def end():
    global SOLUTION_COMPLETED, SOLUTION
    SOLUTION_COMPLETED = True
    SOLUTION = EB
            
# Raise is an important heart of this process.  The question is how
# to do the test-for-done.  That is, when putting up fingers, how
# does the child know when he's got the right number up?  In this
# version, he uses the echoic buffer trace, but that can't be right
# for shortcut sum because the echoic buffer contains the SUM, not
# just the single hand's count, so the right hand won't work.
# Somehow the child can SAY one thing while covertly counting
# another.  This suggests a dissociation of the echoic number
# sequence from counting, which can be done covertly.  Instead of
# relying upon the echoic trace for the count, We uses a new buffer
# (*cb*) to maintain the count of fingers up on a particular hand.
# This buffer is cleared by raise2 itself, and is used for the done
# test.

def exec_op(op):
    trp(2,'Doing:(%s)' % op)
    #mempush (exect_op, lisp op)
    #funcall (car op)

# This version of raise assumes that hand control is done by the caller.

def raise_hand():
    global CB
    CB = 0
    while True:
        say_next()
        HAND.put_up()
        HAND.increment_focus()
        CB+=1
        if CB >= ADDEND.addend:
            break
        
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
    
    def __init__(self,ad1,ad2):
        self.ad1 = ad1
        self.ad2 = ad2
        self.addend = 0
        self.cla = ''
        
    def choose(self):
        if randint(0,1) == 0:
            self.addend = self.ad1
        else:
            self.addend = self.ad2
            
        # Indicate which addend we've chosen for min discovery.
        
        if self.ad1==self.ad2:
            self.cla = 'equal'
        elif ((self.addend==self.ad1) and (self.ad1>self.ad2)) or ((self.addend==self.ad2) and (self.ad1<self.ad2)):
            self.cla = 'larger'
        else:
            self.cla = 'smaller'
        trp(3, 'Choose addend %s.' % self.addend)
        
    def swap(self):
        if self.addend == self.ad1:
            self.addend = self.ad2
            #mempush (swap_addends, from 1 to 2)
        else:
            self.addend = self.ad1
            #mempush (swap_addeds, from 2 to 1)
        trp(3, 'Looking to the other addend %s.' % self.addend)
    
    def say(self):
        say(self.addend)
    
    def choose_larger(self):
        if self.ad1>self.ad2:
            self.addend = self.ad1
        else:
            self.addend = self.ad2
        
        if self.ad1 == self.ad2:
            self.cla = 'equal'
        else:
            self.cla = 'larger'
        trp(3, 'Choose addend %s.' % self.addend)

#Here are the actual strategies.

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
    
    #Final count out.
    
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



# Try retrieval first; if fails, initialize hand, echoic buffer and 
# counting buffer,and carry out the strategy.
# Update memory and distribution table at the end.

def exec_strategy(strategy_choice):
    global HAND, CB, EB, SOLUTION_COMPLETED, SOLUTION
    
    # Try retrieval.
    
    SOLUTION_COMPLETED = False
    retrieval = APSM.guess(ADDEND.ad1,ADDEND.ad2)
    
    if retrieval is not None:
        SOLUTION_COMPLETED = True
        SOLUTION = retrieval
        trp(1, "Used retrieval")
    
    EB = 0
    CB = 0
    HAND = Hand()
    
    # Get the list of operations from the strategy.
    
    list_of_operations = strategy_choice()
    
    # Carry out the operations.
    
    for i in list_of_operations:
        if SOLUTION_COMPLETED:
            break
        i()
    
    # Update APSM and DSTR tables.
    
    APSM.update (ADDEND.ad1, ADDEND.ad2, SOLUTION)
    DSTR.update (ADDEND.ad1, ADDEND.ad2, SOLUTION)
    trp(1, "Solution = %s" % SOLUTION)

# Multiple (standard) strategies and Strategy Choice Algorithm (SCA).
# For the moment, this will just be random.

def SCA():
    exec_strategy(random_strategy())


# Problem Presentation Algorithm (PPA).
# Again, just random for now.

def PPA():
    global ADDEND
    ADDEND = Addend(randint(1,5),randint(1,5))
    trp(1, "%s + %s = ?" % (ADDEND.ad1, ADDEND.ad2))

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
        for i in range(1,6):
            for j in range(1,6):
                self.y.append(nn.predict(nn1.addends_matrix(i,j)))
                self.table[i][j] = self.y[y_index]
                #self.table[i][j] = nn.predict(nn1.addends_matrix(i,j))
                y_index += 1

    # When the problem is completed, update the memory table
    # appropriately depending upon whether we got it right or wrong.
    # Ignore the results of challenge problems.
    # then update the memory table based on what we learned

    def update(self, a1, a2, result):
        if (a1>5) or (a2>5) or (result>10):
            trp(1,"Addends (%s+%s) or result (%s) is/are larger than the memory table limits -- Ignored!" % (a1,a2,result))
        else:
            if a1 + a2 == result:
                self.table[a1][a2][a1 + a2] += INCR_RIGHT
            else:
                self.table[a1][a2][a1 + a2] += INCR_WRONG
        nn.fit(X_count,np.array(self.y), epochs=2000)

    # Print the table.

    def show(self):
        for i in range(1,6):
            for j in range(1,6):
                print "%s + %s = " % (i,j) ,
                for k in range(1,11):
                    print "%s (%s), " % (k, self.table[i][j][k]),
                print
            print

    # Pick at random from among the results that come above the cc, or
    # return None if nothing comes over the cc.

    def guess(self,a1,a2):
        if (a1>5) or (a2>5):
            return (None)

        cc = RETRIEVAL_LOW_CC + (RETRIEVAL_HIGH_CC - RETRIEVAL_LOW_CC) * random()
        trp(1, "Choose confidence criterion = %s" % cc)

        results_above_cc = []

        for i in range(1,11):
            if self.table[a1][a2][i] >= cc:
                results_above_cc.append(i)

        l = len(results_above_cc)
        if l>0:
            return (results_above_cc[randint(0,l-1)])
        return (None)

# Answer distribution table

class Distribution(object):

    # Record answers ranging from 0 to 11; 12 includes all other answers.

    def __init__(self):
        self.table= [[[0 for x in range(13)] for x in range(6)] for x in range(6)]

    # Update the distribution table when a new answer is generated.

    def update(self, a1, a2, result):
        if (a1>5) or (a2>5):
            trp(1,"Addends (%s+%s) is/are larger than the distribution table limits -- Ignored!" % (a1,a2))
            return

        if result not in range(12):
            self.table[a1][a2][12] +=1
        else:
            self.table[a1][a2][result] +=1

    # Calculate relative frequency, return blank string when frequency is zero
    # so that the table looks clean when printed.

    def relative_frequency(self,a1,a2,result):
        s = sum(self.table[a1][a2])
        if (s == 0) or (self.table[a1][a2][result] == 0):
            return ''
        else:
            return round(float(self.table[a1][a2][result])/s, 2)

    # Same function but return zero when frequency is zero so that
    # it can be plotted into graphs.

    def relative_frequency1(self,a1,a2,result):
        s = sum(self.table[a1][a2])
        if s == 0:
            return 0
        else:
            return float(self.table[a1][a2][result])/s

    # Convert the whole frequency table to relative frequency.

    def relative_table(self,relative):
        if relative:
            return [[[self.relative_frequency1(x,y,z) for z in range(13)] for y in range(6)] for x in range(6)]
        else:
            return [[[self.table[x][y][z] for z in range(13)] for y in range(6)] for x in range(6)]

    # Print the table.

    def show(self,relative = True):
        table = self.relative_table(relative)

        for i in range(1,6):
            for j in range(1,6):
                print "%s + %s = " % (i,j) ,
                for k in range(13):
                    print "%s (%0.03f), " % (k, table[i][j][k]),
                print
            print

    # Export to csv file.

    def print_csv(self,relative = False):
        table = self.relative_table(relative)

        with open(os.path.join(os.path.dirname(__file__), 'DistributionTable.csv'),'wb') as csvfile:
            writer = csv.writer(csvfile)

            writer.writerow(['PROBLEM','ANSWER'])
            writer.writerow(['']+[str(x) for x in range(12)]+['OTHER'])
            for i in range(1,6):
                for j in range(1,6):
                    writer.writerow(["%s + %s = " % (i,j)]+[table[i][j][k] for k in range(13)])

    # Plot the distribution table into bar charts.

    def bar_plot(self, relative = False):
        if relative:
            table = [[[self.relative_frequency1(x,y,z) for z in range(13)] for y in range(6)] for x in range(6)]

        else:
            table = [[[self.table[x][y][z] for z in range(13)] for y in range(6)] for x in range(6)]

        maxheight = max([max([max(table[x][y]) for x in range(6)]) for y in range(6)])

        plt.figure()

        for i in range(1,6):
            for j in range(1,6):
                ax=plt.subplot(5,5,(i-1)*5+j)
                plt.bar([x-0.4 for x in range(13)],table[i][j],linewidth=0,color="steelblue")
                plt.xlim(-0.5,12.5)
                plt.ylim(0, maxheight*1.1)
                plt.text(.5,1.03,"%s + %s" % (i,j), horizontalalignment='center', transform=ax.transAxes)
                plt.tick_params(\
                    axis='both',
                    which='both',
                    bottom='off',
                    top='off',
            	    left='off',
                    right='off',
                    labelleft='on',
                    labelbottom='on',labelsize=8)
        plt.tight_layout(h_pad=1)
        plt.show()

def test(n_times,strategy_choice):

    # Repeat n times.

    for i in range(n_times):
        PPA()
        exec_strategy(strategy_choice)

    #Output the distribution table.

    DSTR.show(relative = True)
    # DSTR.print_csv(relative = True)
    # DSTR.bar_plot(relative = True)

#sets up the neural network fitted to counting
def counting_network(hidden_units = 30, learning_rate = 0.07):
    global X_count, y_count
    #this is the addends matrix
    input_units = 14
    #this is the output matrix
    output_units = 13


    #fits to counting network
    NN = nn1.NeuralNetwork([input_units,hidden_units,output_units])
    X_count = []
    y_count = []
    for i in range(1,5):
        X_count.append(nn1.addends_matrix(i,i+1))
        y_count.append(nn1.sum_matrix(i+2))
    X_count = np.array(X_count)
    y_count = np.array(y_count)
    NN.fit(X_count,y_count,learning_rate)
    return NN

def main():
    global TL, PERR, RETRIEVAL_LOW_CC, RETRIEVAL_HIGH_CC
    global INCR_RIGHT, INCR_WRONG
    global APSM, DSTR
    global nn

    TL=0 # Trace level -- 0 means off
    
    PERR = 0.04 # Probability of counting error (missing a count).
    INCR_RIGHT = 0.06 # Add this to solution memory when you get a problem right
    INCR_WRONG = 0.03 # Add this when you get one wrong
    
    # Retrieval cc ranges are used in select-strategy to determine when
    # to actually choose retrieval (via setting the cc randomly).
    
    RETRIEVAL_LOW_CC = 0.1
    RETRIEVAL_HIGH_CC = 0.9

    nn = counting_network()

    # Set up the solution memory table and the answer distribution table
    APSM = Apsm()
    DSTR = Distribution()



    test(2000,count_from_either_strategy)
    return DSTR.relative_table(relative=True)


if __name__ == "__main__":
    main()
