# HIPS Teaching model based on published data in
# Fawsitt 2019 https://pubmed.ncbi.nlm.nih.gov/30832968/
# Howard Thom April 2023

#' Generate net benefit
#'
#' Function to use Markov modelling to simulate total costs, QALYs and net benefit
#' @param input_parameters Matrix with row for each sample and column
#' for each parameter, with values samples for the model input parameters
#' @param lambda Willingness-to-pay threshold for net monetary benefit
#' @return List of matrices total_costs, total_qalys, net_benefit and
#' incremental_net_benefit with n_implants rows and n_samples columns
#' @examples
#' # First sample the input parameters
#' input_parameters <- generate_input_parameters(n_samples)
#'
#' # Generate the net benefit using the above input parameters
#' model_output <- generate_net_benefit(input_parameters, lambda = 20000)
#'
#' # Average costs
#' rowMeans(model_output$total_costs)
#'
#' # Average QALYS
#' rowMeans(model_output$total_qalys)
#' @export
markov_model$set(
  "public",
  "generate_outputs",
  function(input_parameters, lambda = 20000) {
    # First generate components needed for simulation
    transition_matrices <- generate_transition_matrices(input_parameters)
    state_costs <- generate_state_costs(input_parameters)
    state_qalys <- input_parameters[, grepl(
      "state_utility",
      colnames(input_parameters)
    )]

    # Implant costs (transpose to keep convention of n_implants, n_samples)
    implant_costs <- t(input_parameters[, grepl(
      "implant_cost_",
      colnames(input_parameters)
    )])
    # Build an array to store the cohort vector at each cycle
    # Each cycle, implant and PSA sample has a single cohort vector of length n_states
    cohort_vectors <- array(
      dim = c(n_cycles, n_implants, n_samples, n_states),
      dimnames = list(NULL, implant_names, NULL, state_names)
    )

    # Assume everyone starts in the post_thr state
    cohort_vectors[1, , , "post_thr"] <- 1
    # All other proportions start at zero
    cohort_vectors[1, , , -1] <- 0

    # Build an array to store the costs and QALYs accrued per cycle
    # One for each cycle, implant, and PSA sample
    # These will be filled in below in the main model code
    # Then discounted and summed to contribute to total costs and total QALYs
    cycle_costs <- array(
      dim = c(n_cycles, n_implants, n_samples),
      dimnames = list(NULL, implant_names, NULL)
    )
    cycle_qalys <- array(
      dim = c(n_cycles, n_implants, n_samples),
      dimnames = list(NULL, implant_names, NULL)
    )

    # Build arrays to store the total costs and total QALYs
    # There is one for each treatment and each PSA sample
    # These are filled in below using cycle_costs,
    # implant_costs, and cycle_qalys
    total_costs <- array(
      dim = c(n_implants, n_samples),
      dimnames = list(implant_names, NULL)
    )
    total_qalys <- array(
      dim = c(n_implants, n_samples),
      dimnames = list(implant_names, NULL)
    )

    # The remainder of the cohort_vectors will be filled in by Markov updating below

    # Main model code
    # Loop over the implants
    for (i_implant in 1:n_implants) {
      # Loop over the PSA samples
      for (i_sample in 1:n_samples) {
        # Loop over the cycles
        # Cycle 1 is already defined so only need to update cycles 2:n_cycles
        for (i_cycle in 2:n_cycles) {
          # Markov update
          # Multiply previous cycle's cohort vector by transition matrix
          # i_e_ pi_j = pi_(j-1)*P
          cohort_vectors[i_cycle, i_implant, i_sample, ] <-
            cohort_vectors[i_cycle - 1, i_implant, i_sample, ] %*%
            transition_matrices[i_cycle - 1, i_implant, i_sample, , ]
        }

        # Now use the cohort vectors to calculate the
        # total costs for each cycle
        cycle_costs[, i_implant, i_sample] <-
          cohort_vectors[, i_implant, i_sample, ] %*%
          state_costs[i_implant, i_sample, ]
        # And total QALYs for each cycle
        cycle_qalys[, i_implant, i_sample] <-
          cohort_vectors[, i_implant, i_sample, ] %*% t(state_qalys[i_sample, ])

        # Combine the cycle_costs and implant_costs to get total costs
        # Apply the discount factor
        # (1 in first year, 1_035 in second, 1_035^2 in third, and so on)
        # Each year acounts for two cycles so need to repeat the discount values
        total_costs[i_implant, i_sample] <- implant_costs[i_implant, i_sample] +
          cycle_costs[, i_implant, i_sample] %*%
            (1 / 1.035)^rep(c(0:(n_cycles - 1)), each = 1)

        # Combine the cycle_qalys to get total qalys
        # Apply the discount factor
        # (1 in first year, 1_035 in second, 1_035^2 in third, and so on)
        # Each year acounts for two cycles so need to repeat the discount values
        total_qalys[i_implant, i_sample] <- cycle_qalys[,
          i_implant,
          i_sample
        ] %*%
          (1 / 1.035)^rep(c(0:(n_cycles - 1)), each = 1)
      }
    }

    # Calculate net benefit and incremental net benefit at
    # willingness-to-pay that was supplied
    net_benefit <- total_qalys * lambda - total_costs
    incremental_net_benefit <- net_benefit - net_benefit[1, ]

    #return(list("total_costs" = total_costs,
    #            "total_qalys" = total_qalys,
    #            "net_benefit" = net_benefit,
    #            "incremental_net_benefit" = incremental_net_benefit))

    # Return the object
    invisible(self)
  }
)
