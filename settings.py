import ADD
import driver

global RETRIEVAL_LOW_CC, RETRIEVAL_HIGH_CC, STRATEGY_HIGH_CC, STRATEGY_LOW_CC
global INCR_RIGHT, INCR_WRONG, DECR_WRONG
global epoch, learning_rate, n_problems, strategies, ndups


# these are the parameters used in NeuralNetwork
INCR_RIGHT = 99999  # Add this to solution memory when you get a problem right
INCR_WRONG = 0.05  # Add this when you get one wrong
DECR_WRONG = 0.5

# we just need to initialize the parameters
n_problems = 99999
epoch = 99999
learning_rate = 99999
ndups = 1

# Retrieval cc ranges are used in select-strategy to determine when
# to actually choose retrieval (via setting the cc randomly).
RETRIEVAL_LOW_CC = 0.9
RETRIEVAL_HIGH_CC = 1.0
STRATEGY_LOW_CC = 0.9
STRATEGY_HIGH_CC = 1.0


# these are the parameters we want to change
strategies = [ADD.count_from_either_strategy, ADD.random_strategy, ADD.count_from_one_once_strategy,
              ADD.count_from_one_twice_strategy, ADD.min_strategy]

scan_spec = {"NPROBLEMS": [100, 200, 300],
             "EPOCH": [10, 20],
             "INCR_RIGHT": [5],
             "LEARNING_RATE": [0.1]}

if __name__ == '__main__':
    driver.main()
