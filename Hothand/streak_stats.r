# function that returns basic streak statistics given a sequence
streak_stats = function(results_sequence, streak_length){
  n_trials = length(results_sequence)
  n_success = sum(results_sequence)
  
  run_lengths = rle(results_sequence)$lengths
  if_run_success = rle(results_sequence)$values
  n_runs = length(run_lengths)
  
  # runs of successes
  longest_run_success = max(run_lengths*if_run_success)
  n_runs_success = sum(if_run_success)
  
  # successes after streaks of Successes
  run_lengths_adj = run_lengths - streak_length + 1
  run_values_adj_S =
    if_run_success *
    (!((if_run_success == 1) &
         (run_lengths < streak_length)))
  n_after_Sstreak = sum(run_lengths_adj * run_values_adj_S) -
    run_values_adj_S[length(run_values_adj_S)]
  n_success_after_Sstreak = sum((run_lengths - streak_length) * run_values_adj_S)
  
  # runs of failures
  longest_run_failure = max(run_lengths*(1-if_run_success))
  n_runs_failure = sum(1-if_run_success)
  
  # successes after streaks of Failures
  run_values_adj_F =
    (1-if_run_success) *
    (!((if_run_success == 0) &
         (run_lengths < streak_length)))
  n_after_Fstreak = sum(run_lengths_adj * run_values_adj_F) -
    run_values_adj_F[length(run_values_adj_F)]
  n_success_after_Fstreak = n_after_Fstreak -
    sum((run_lengths - streak_length) * run_values_adj_F)
  
  # Proportion of hits on trials after streaks
  phat_after_Sstreak = n_success_after_Sstreak / n_after_Sstreak
  
  # Difference in proportion of hits (trials after streaks - all other trials)
  phat_after_no_Sstreak = (n_success - n_success_after_Sstreak)/
    (n_trials - n_after_Sstreak)
  phat_Sstreak_vs_others = phat_after_Sstreak - phat_after_no_Sstreak
  
  # Difference in proportion of hits (trials after hit streaks - trials after miss streaks)
  phat_after_Fstreak = n_success_after_Fstreak / n_after_Fstreak
  phat_Sstreak_vs_Fstreak = phat_after_Sstreak - phat_after_Fstreak
  
  # Hit streak frequency - proportion of trials that occur after hit streaks
  Sstreak_frequency  = n_after_Sstreak / (n_trials - streak_length)
  
  # collect the streak stats for the results sequence
  return(data.frame(phat_after_Sstreak,
                    phat_Sstreak_vs_others,
                    phat_Sstreak_vs_Fstreak,
                    Sstreak_frequency,
                    longest_run_success,
                    n_runs
                    #                     n_runs_success,
                    #                     phat_after_Fstreak,
                    #                     n_runs_failure,
                    #                     longest_run_failure,
  ))
}