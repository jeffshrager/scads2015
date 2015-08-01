import ADD
import driver

global RETRIEVAL_LOW_CC, RETRIEVAL_HIGH_CC, STRATEGY_HIGH_CC, STRATEGY_LOW_CC
global INCR_RIGHT, INCR_WRONG, DECR_WRONG
global epoch, learning_rate, strategy, n_problems, strategies
global epochs, learning_rates, incr_rights, n_problemss, ndups

INCR_RIGHT = 10  # Add this to solution memory when you get a problem right
INCR_WRONG = 0.05  # Add this when you get one wrong
DECR_WRONG = 0.5

# Retrieval cc ranges are used in select-strategy to determine when
# to actually choose retrieval (via setting the cc randomly).

RETRIEVAL_LOW_CC = 0.9
RETRIEVAL_HIGH_CC = 1.0

STRATEGY_LOW_CC = 0.9
STRATEGY_HIGH_CC = 1.0
n_problemss = [2000]
learning_rates = [0.1]
epochs = [100]
incr_rights = [5]
strategies = [ADD.count_from_either_strategy, ADD.random_strategy, ADD.count_from_one_once_strategy,
              ADD.count_from_one_twice_strategy, ADD.min_strategy]

ndups = 1

if __name__ == '__main__':
    driver.main()
