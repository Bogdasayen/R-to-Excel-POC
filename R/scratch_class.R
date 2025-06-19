#' The Markov model class
#' @description Markov model class
#'
#' @field n_states Number of states for the Markov model
#' @field n_cycles Number of cycles for the cohort simulation
#' @field n_parameters Number of parameters in the model
#' @field n_samples Number of probabilistic samples
#' @field n_treatments Number of treatments, including the reference
#' @field v_state_names Vector of length n_states
#' @field v_treatment_names Vector of length n_treatments
#' @field lambda Willingness-to-pay threshold for net monetary benefit
#' @field costs_dr Discount rate for costs per cycle for cohort simulation (e.g., 5\% costs_dr = 0.05)
#' @field qalys_dr Discount rate for QALYs or other effects per cycle for cohort simulation (e.g., 5\% is qalys_dr = 0.05)
#' @field markov_inputs An object of R6 class input_parameters with description and (optional) sampled values
#' @field a_transition_matrices Array of (time-homogeneous) random transition matrices with dimensions n_samples by n_states by n_states
#' @field a_cohort_array Array of cohort vectors with dimensions n_treatments x n_samples x n_cycles x n_states
#' @field v_init_cohort Initial cohort vector shared across treatments and samples
#' @examples
#' markov_smoking <- markov_model$new(n_states = 2, n_cycles = 10, n_samples = 1000, n_treatments = 2, v_state_names = c("Smoking", "Not smoking"), v_treatment_names = c("SoC", "SoC with website"), lambda = 20000, costs_dr = 0.035, qalys_dr = 0.035)
#'
#' @export
scratch_class <- R6Class(
  "scratch_class",
  list(
    # Assign variable for all public elements of the class
    # These are defined in the initialize() function

    n_states = NULL,
    n_cycles = NULL,
    n_parameters = NULL,
    n_samples = NULL,
    n_treatments = NULL,
    v_state_names = NULL,
    v_treatment_names = NULL,
    lambda = NULL,
    costs_dr = NULL,
    qalys_dr = NULL,
    markov_inputs = NULL,
    a_transition_matrices = NULL,
    a_cohort_array = NULL,
    v_init_cohort = NULL,

    #' @description
    #' Initialise the Markov model
    #'
    #' Function to use Markov modelling to simulate total costs, QALYs and net benefit
    #' @param n_states Number of states for the Markov model
    #' @param n_cycles Number of cycles for the cohort simulation
    #' @param n_samples Number of probabilistic samples
    #' @param n_treatments Number of treatments, including the reference
    #' @param v_state_names Vector of length n_states
    #' @param v_treatment_names Vector of length n_treatments
    #' @param lambda Willingness-to-pay threshold for net monetary benefit
    #' @param costs_dr Discount rate for costs per cycle for cohort simulation (e.g., 5\% is costs_dr = 0.05)
    #' @param qalys_dr Discount rate for QALYs or other effects per cycle for cohort simulation (e.g., 5\% is qalys_dr = 0.05)
    #' @param markov_inputs An object of R6 class input_parameters description
    #' @param v_init_cohort Vector representing initial proportion in each cohort
    #' @return An initialised Markov model
    #' @examples
    #' markov_smoking <- markov_model$new(n_states = 2, n_cycles = 10, n_samples = 1000, n_treatments = 2, v_state_names = c("Smoking", "Not smoking"), v_treatment_names = c("SoC", "SoC with website"), lambda = 20000, costs_dr = 0.035, qalys_dr = 0.035, markov_inputs = smoking_inputs, v_init_cohort = c(1, 0))
    #' @export
    initialize = function(
      n_states,
      n_cycles,
      n_samples,
      n_treatments,
      v_state_names,
      v_treatment_names,
      lambda,
      costs_dr,
      qalys_dr,
      markov_inputs,
      v_init_cohort
    ) {
      self$n_states = n_states
      self$n_cycles = n_cycles
      self$n_samples = n_samples
      self$n_treatments = n_treatments
      self$v_state_names = v_state_names
      self$v_treatment_names = v_treatment_names
      self$lambda = lambda
      self$costs_dr = costs_dr
      self$qalys_dr = qalys_dr
      self$markov_inputs = markov_inputs
      self$v_init_cohort = v_init_cohort
    }
  ) # End of public list
) # End of R6Class
