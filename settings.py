import ADD
import driver

global RETRIEVAL_LOW_CC, RETRIEVAL_HIGH_CC, STRATEGY_HIGH_CC, STRATEGY_LOW_CC
global INCR_RIGHT, INCR_WRONG, DECR_WRONG
global epoch, learning_rate, n_problems, strategies, ndups, DR_threshold, experiment_label

ndups = 3 # Number of replicates of each combo of params -- usually 3 unless testing.

strategies = [ADD.count_from_either_strategy, ADD.random_strategy, ADD.count_from_one_once_strategy,
              ADD.count_from_one_twice_strategy, ADD.min_strategy] # ADD.random_strategy

# The settings.experiment_label is used by the analyzer to label the
# results file because we set these by exec(), this has to have an
# extra set of "\"quotes\"" around it.

scan_spec = {"settings.experiment_label": ["\"201508091017 Scanning n problems but should do some dynamic retrieval near the end\""],
             "settings.n_problems": [1000,5000],
             "settings.RETRIEVAL_LOW_CC": [0.9],
             "settings.RETRIEVAL_HIGH_CC": [1.0],
             "settings.STRATEGY_LOW_CC": [0.9],
             "settings.STRATEGY_HIGH_CC": [1.0],
             "settings.epoch": [10],
             "settings.INCR_RIGHT": [5],
             "settings.INCR_WRONG": [1],
             "settings.DECR_WRONG": [0.5],
             "settings.learning_rate": [0.1],
             "settings.DR_threshold": [0.8]}

if __name__ == '__main__':
    driver.main()
