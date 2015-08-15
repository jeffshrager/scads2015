import ADD
import driver

global RETRIEVAL_LOW_CC, RETRIEVAL_HIGH_CC, STRATEGY_HIGH_CC, STRATEGY_LOW_CC
global INCR_RIGHT, INCR_WRONG, DECR_WRONG, DECR_RIGHT
global epoch, learning_rate, n_problems, strategies, ndups, DR_threshold, experiment_label
global initial_counting_network_burn_in_epochs, initial_counting_network_learning_rate
global hidden_units, addend_matrix_offby1_delta, PERR

hidden_units = 30
ndups = 1  # Number of replicates of each combo of params -- usually 3 unless testing.
pbs = 25  # problem bin size, every pbs problems we dump the predictions

# ADD.random_strategy -- Usually left out!

strategies = [ADD.count_from_either_strategy, ADD.count_from_one_once_strategy,
              ADD.count_from_one_twice_strategy, ADD.min_strategy]

# The settings.experiment_label is used by the analyzer to label the
# results file because we set these by exec(), this has to have an
# extra set of "\"quotes\"" around it.

scan_spec = {"settings.experiment_label": ["\"201508131550 test output format\""],
             "settings.n_problems": [5000],  # 250 = 10x CONFIRMED (peak 201508100718)
             "settings.RETRIEVAL_LOW_CC": [0.95],  # 0.8 CONFIRMED by 201508100733 (peak)
             "settings.RETRIEVAL_HIGH_CC": [1.0],  # Fixed at 1.0
             "settings.STRATEGY_LOW_CC": [0.75],  # 0.75 SUGGESTED by 201508101051 (?)
             "settings.STRATEGY_HIGH_CC": [1.0],  # Fixed at 1.0
             "settings.epoch": [10],  # 10 CONFIRMED by 201508091017 (no delta)
             "settings.INCR_RIGHT": [5],  # 5 CONFIRMED by 201508091017 (peak)
             "settings.DECR_RIGHT": [0.35],  # ?? New ??
             "settings.INCR_WRONG": [0.1],  # 0.5 SUGGESTED by 201508101051 (?)
             "settings.DECR_WRONG": [0.35],  # 2.0 CONFIRMED (peak 201508100718)
             "settings.learning_rate": [0.1],  # 0.1 CONFIRMED by 201508091017 (peak)
             "settings.initial_counting_network_burn_in_epochs": [100],
             "settings.initial_counting_network_learning_rate": [0.15],
             "settings.addend_matrix_offby1_delta": [0.5],
             "settings.DR_threshold": [0.1],
             "settings.PERR": [0.04]
             }

if __name__ == '__main__':
    driver.main()

    # Experiment: 201508100733 Scanning RET_LOW, STRAT_LOW, and INCR_WRONG
    # Gave hard to interpret results where there seemed to be an
    # interaction between STRAT_LOW and INCR_WRONG. Deep scanned these
    # (x10) in 201508101051. From that it looks like the lower end of
    # STRATEGY_LOW_CC: [0.7,0.75,0.8,0.85,0.9], or about 0.75, and the
    # higher end of settings.INCR_WRONG:
    # [0.25,0.5,0.75,1.0,1.25,1.5,1.75,2.0,2.25], or about 2.0 are the
    # best values. (These ends are a bit less variable than the res of the
    # ranges).
