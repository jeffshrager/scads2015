import ADD
import driver

# Set things that are going to be scanned to 9999 to indicate that
# something went wrong if this value remains!

INCR_RIGHT = 9999  # Add this to solution memory when you get a problem right
INCR_WRONG = 0.05  # Add this when you get one wrong
DECR_WRONG = 0.5

# Retrieval cc ranges are used in select-strategy to determine when
# to actually choose retrieval (via setting the cc randomly).

RETRIEVAL_LOW_CC = 0.9
RETRIEVAL_HIGH_CC = 1.0

STRATEGY_LOW_CC = 0.9
STRATEGY_HIGH_CC = 1.0
STRATEGIES = [ADD.count_from_either_strategy, ADD.random_strategy, ADD.count_from_one_once_strategy,
              ADD.count_from_one_twice_strategy, ADD.min_strategy]

NPROBLEMS = 9999
EPOCH = 9999
LEARNING_RATE = 9999

scanspec = {"NPROBLEMS": [100, 200, 300],
            "EPOCH": [10, 20],
            "INCR_RIGHT": [5],
            "LEARNING_RATE": [0.1]}

NDUPS = 5

if __name__ == '__main__':
    print ("driver.main()\n")
    driver.main()
