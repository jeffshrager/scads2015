import ADD
import driver

global RETRIEVAL_LOW_CC, RETRIEVAL_HIGH_CC, STRATEGY_HIGH_CC, STRATEGY_LOW_CC
global INCR_RIGHT, INCR_WRONG, DECR_WRONG
global epoch, learning_rate, n_problems, strategies, ndups, DR_threshold, experiment_label


# we just need to initialize the parameters
ndups = 3

# Retrieval cc ranges are used in select-strategy to determine when
# to actually choose retrieval (via setting the cc randomly).



strategies = [ADD.count_from_either_strategy]

# The settings.experiment_label is used by the analyzer to label the
# results file because we set these by exec(), this has to have an
# extra set of "\"quotes\"" around it.

scan_spec = {"settings.experiment_label": ["\"this is another test\""],
             "settings.n_problems": [200,400,600],
             "settings.RETRIEVAL_LOW_CC": [0.9],
             "settings.RETRIEVAL_HIGH_CC": [1.0],
             "settings.STRATEGY_LOW_CC": [0.9],
             "settings.STRATEGY_HIGH_CC": [1.0],
             "settings.epoch": [10,20,30],
             "settings.INCR_RIGHT": [5],
             "settings.INCR_WRONG": [1],
             "settings.DECR_WRONG": [0.5],
             "settings.learning_rate": [0.1],
             "settings.DR_threshold": [0.000005]}

if __name__ == '__main__':
    driver.main()
