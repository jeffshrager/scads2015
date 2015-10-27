import ADD
import driver

global RETRIEVAL_LOW_CC, RETRIEVAL_HIGH_CC, STRATEGY_HIGH_CC, STRATEGY_LOW_CC
global INCR_on_RIGHT, DECR_on_WRONG, INCR_the_right_answer_on_WRONG
global in_process_training_epochs, learning_rate, n_problems, strategies, ndups, DR_threshold, experiment_label
global initial_counting_network_burn_in_epochs, initial_counting_network_learning_rate
global hidden_units, addend_matrix_offby1_delta, PERR
global non_result_y_filler, dynamical_retrieval_on

# PART 1: These usually DON'T change:

hidden_units = 30
ndups = 3  # Number of replicates of each combo of params -- usually 3 unless testing.
pbs = 25  # problem bin size, every pbs problems we dump the predictions
dynamical_retrieval_on = False

# PART 2: These also usually DON'T change, although they might if you
# want to explore bringing in and out various strategies:

              # ADD.random_strategy -- Usually left out
strategies = [ADD.count_from_either_strategy, ADD.count_from_one_once_strategy,
              ADD.count_from_one_twice_strategy, ADD.min_strategy]

# PART 3: These usually DO change:

# IMPORTANT: REMEMBER TO CHANGE settings.experiment_label, which is
# used by the analyzer to label the results file. (Because we set
# these by exec(), this has to have an extra set of "\"quotes\""
# around it.)

scan_spec = {"settings.experiment_label": 
             ["\"20151027: 33333333333333\""],
             # Setting up the initial counting network
             "settings.initial_counting_network_burn_in_epochs": [1000], # 1000 based on 201509010902
             "settings.initial_counting_network_learning_rate": [0.25], # 0.25 based on 201509010902
             # Problem presentation and execution
             "settings.n_problems": [1000],
             "settings.DR_threshold": [1.0], # WWW!!! Only used if dynamical_retrieval_on = True
             "settings.PERR": [0.0], # 0.1 confirmed 201509010826
             "settings.addend_matrix_offby1_delta": [0.0,1.0], # =1 will make the "next-to" inputs 0, =0 makes them 1, and so on
             # Choosing to use retrieval v. a strategy
             "settings.RETRIEVAL_LOW_CC": [0.6], # Should be 0.6 usually; at 1.0 no retrieval will occur
             "settings.RETRIEVAL_HIGH_CC": [1.0], # Should be 1.0 usually
             "settings.STRATEGY_LOW_CC": [0.6], # If 1.0, strategies will be chosen randomly
             "settings.STRATEGY_HIGH_CC": [1.0],
             # Learning target params
             "settings.non_result_y_filler": [0.0], # Set into all outputs EXCEPT result, which is adjusted by INCR_RIGHT and DECR_WRONG
             "settings.INCR_on_RIGHT": [1.0], # Added to non_result_y_filler at the response value when you get it right.
             "settings.DECR_on_WRONG": [-1.0], # Substrated from non_result_y_filler at the response value when you get it right.
             "settings.INCR_the_right_answer_on_WRONG": [0.0], # Added to non_result_y_filler at the CORRECT value when you get it WRONG.
             "settings.learning_rate": [0.1], # Explored 201509010826
             "settings.in_process_training_epochs": [10] # Number of training epochs on EACH test problem (explored 201509010826)
             }

if __name__ == '__main__':
    driver.top_level_run()
