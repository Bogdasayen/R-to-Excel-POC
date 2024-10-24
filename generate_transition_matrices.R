# HIPS Teaching model based on published data in 
# Fawsitt 2019 https://pubmed.ncbi.nlm.nih.gov/30832968/
# Howard Thom April 2023

#' Generate transition matrices
#' 
#' Function to generate transition matrices. Used internall by 
#' generate_net_benefit() function
#' @param input_parameters Matrix with row for each sample and column 
#' for each parameter, with values samples for the model input parameters
#' @return Array of n_cycles by n_implants by n_samples by n_states by n_states, 
#' with values equal to transition probabilities. These correspond to a n_state
#' by n_state transition matrix for each cycle, implant and sample.
#' @examples 
#' # First sample the input parameters
#' input_parameters <- generate_input_parameters(n_samples)
#' 
#' transition_matrices <- generate_transition_matrices(input_parameters)
#'
#' # The transition matrix for first cycle, first implant and first sample
#' transition_matrices[1, 1, 1, , ]
#' #' # The transition matrix for first cycle, third implant and second sample
#' transition_matrices[1, 3, 2, , ]
#' # Sampled transitions from the first state for first cycle and third implant
#' transition_matrices[1, 3, , 1, , ]
#' @export 
markov_model$set("public", "generate_transition_matrices", function() {
  
  # One 4x4 transition matrix for each cycle, implant_name and sample.
  transition_matrices <- array(0, dim = c(n_cycles, n_implants, n_samples, n_states, n_states),
                               dimnames = list(NULL, implant_names, NULL, state_names, state_names))
  
  # UK lifetables are fixed so are not part of the input parameters matrix
  # This saves memory of an n_sample*100 (i.e., number of ages) matrix
  uk_lifetables <- read_excel(paste0(data_directory, "/hips_input_data.xlsx"), sheet = "uk_lifetables")
  
  
  for(i_cycle in 1:n_cycles) {
    for(implant_name in implant_names) {
      transition_matrices[i_cycle, implant_name, , "post_thr", "post_1st_rev"] <- 
        1 - exp(-exp(input_parameters[, paste0("log_rate_1st_revision_", implant_name)]))
      transition_matrices[i_cycle, implant_name, , "post_1st_rev", "post_2nd_rev"] <- 
        1 - exp(-exp(input_parameters$log_rate_2nd_revision))
      
      # Annual probability of death the same for all states
      transition_matrices[i_cycle, implant_name, , "post_thr", "dead"] <- 
        transition_matrices[i_cycle, implant_name, , "post_1st_rev", "dead"] <- 
        transition_matrices[i_cycle, implant_name, , "post_2nd_rev", "dead"] <-
        1 - exp(-with(uk_lifetables, Males[Age = initial_age + i_cycle - 1]))
      
      # Ensure remaining patients stay in state
      # Sum probabilities of transitions to other states
      # This ensures probabilities sum to 1.
      for(i_state in 1:length(state_names)) {
        transition_matrices[i_cycle, implant_name, , i_state, i_state] <- 1 - 
          apply(transition_matrices[i_cycle, implant_name, , i_state, -i_state], c(1), sum, na.rm=TRUE)
      } # End loop over states
      
      
    } # End loop over implant_names
  } # End loop over cycles
  
  # At this point do a test and throw and error if it fails
  if(prod(rowSums(transition_matrices[sample(1:n_cycles, 1),
                                      sample(implant_names, 1),
                                      sample(1:n_samples, 1), , ]) == 1) != 1) {
    stop("Rows must sum to 1!")
  }
  
  #return(transition_matrices)
  
  # Return the object
  invisible(self)
})