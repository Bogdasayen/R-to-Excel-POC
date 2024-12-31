require(R6)
require(openxlsx)

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
#' @field costs_dr Discount rate for costs per cycle for cohort simulation (e.g., 5\% is costs_dr = 0.05)
#' @field qalys_dr Discount rate for QALYs or other effects per cycle for cohort simulation (e.g., 5\% is qalys_dr = 0.05)
#' @field markov_inputs An object of R6 class input_parameters with description and (optional) sampled values 
#' @field a_transition_matrices Array of (time-homogeneous) random transition matrices with dimensions n_samples by n_states by n_states
#' @field a_cohort_array Array of cohort vectors with dimensions n_treatments x n_samples x n_cycles x n_states
#' @field v_init_cohort Initial cohort vector shared across treatments and samples
#' @examples
#' markov_smoking <- markov_model$new(n_states = 2, n_cycles = 10, n_samples = 1000, n_treatments = 2, v_state_names = c("Smoking", "Not smoking"), v_treatment_names = c("SoC", "SoC with website"), lambda = 20000, costs_dr = 0.035, qalys_dr = 0.035)
#' 
#' @export
markov_model <- R6Class("markov_model", list(
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
  
  # This function is run when an object of this class is created
  # Arguments are modifiable
  
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
  initialize = function(n_states,
                        n_cycles,
                        n_samples,
                        n_treatments,
                        v_state_names,
                        v_treatment_names,
                        lambda,
                        costs_dr,
                        qalys_dr,
                        markov_inputs,
                        v_init_cohort) {
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
  }, # End initialize function
  
  #' @description
  #' Function to generate random parameters for the Markov model
  #' @param n_samples Number of parameter samples (default NULL and uses current markov_model number of samples)
  #' @examples 
  #' smoking_markov_model$generate_input_parameters(n_samples = 10000)
  #' @export 
  #' 
  generate_input_parameters = function(n_samples = NULL) {
    if(!is.null(n_samples)) self$n_samples <- n_samples
    self$markov_inputs$sample_values(self$n_samples)
    # Return the object
    invisible(self)
  },
  
  #' @description
  #' Function to fill in the transition matrices array
  #' @examples 
  #' smoking_markov_model$generate_transition_matrices()
  #' @export 
  #' 
  generate_transition_matrices = function() {
    
    # Initialise the transition matrices array with 0
    # Fill in values below
    self$a_transition_matrices <- array(0, 
                            dim = c(self$n_treatments,
                                    self$n_samples,
                                    self$n_states,
                                    self$n_states),
                            dimnames = list(self$treatment_names,
                                            NULL,
                                            self$state_names,
                                            self$state_names))
    
    # Loop through the parameters which represent transition parameters
    # Use each to fill in corresponding element of transition matrices
    for(i_parameter in which(!is.na(self$markov_inputs$df_spec$from))) {
      # Get the associated treatment
      # It is NA if applies to all treatments
      i_treatment <- self$markov_inputs$df_spec$v_treatment[i_parameter]
      
      # If treatment specific
      if(!is.na(i_treatment)) {
        self$a_transition_matrices[i_treatment,
                                   ,
                                   self$markov_inputs$df_spec$from[i_parameter],
                                   self$markov_inputs$df_spec$to[i_parameter]
                                   ] <- 
          self$markov_inputs$m_values[, i_parameter]
      } else {
        # Not treatment specific
        self$a_transition_matrices[,
                                   ,
                                   self$markov_inputs$df_spec$from[i_parameter],
                                   self$markov_inputs$df_spec$to[i_parameter]
        ] <- 
          # Each sampled value is repeated for each treatment
          rep(self$markov_inputs$m_values[, i_parameter], each = self$n_treatments)
      }
      
    }
    
    # Fill in diagonal elements to ensure rows sum to 1
    # Ensure remaining patients stay in state
    for(i_treatment in 1:self$n_treatments) {
      for(i_state in 1:self$n_states) {
        if(self$n_states > 2) {
          self$a_transition_matrices[i_treatment, , i_state, i_state] <- 1 - 
            apply(self$a_transition_matrices[i_treatment, , i_state, -i_state], c(1), sum, na.rm=TRUE)
        } else {
          self$a_transition_matrices[i_treatment, , i_state, i_state] <- 1 - 
            self$a_transition_matrices[i_treatment, , i_state, -i_state]
        }
      } # End loop over states
    } # End loop over treatments
    
    # Return the object
    invisible(self)
  },
  
  
  #' @description
  #' Function to generate the Markov trace (i.e., values for cohort matrix over time)
  #' @examples 
  #' smoking_markov_model$generate_markov_trace()
  #' @export 
  #' 
  generate_markov_trace = function() {

    # Initialise array to store the cohort vector at each cycle
    # There is one cohort vector for each treatment, for each PSA sample, for each cycle_
    self$a_cohort_array <- array(NA, 
                            dim = c(self$n_treatments,
                                    self$n_samples,
                                    self$n_cycles,
                                    self$n_states),
                            dimnames = list(self$v_treatment_names,
                                            NULL,
                                            NULL,
                                            self$v_state_names))
    
    # Set starting value for the cohort
    self$a_cohort_array[, , 1, ] <- rep(self$v_init_cohort, 
                                     each = self$n_treatments * self$n_samples)

    
    # The remainder of the a_cohort_array will be filled in by Markov updating below
    
    # Minor optimisation is to pre-access the transition matrices and cohort array
    a_cohort_array <- self$a_cohort_array
    a_transition_matrices <- self$a_transition_matrices
    
    # Main model code
    # Loop over the treatment options
    for (i_treatment in 1:self$n_treatments) 
    {
      # Loop over the PSA samples
      for (i_sample in 1:self$n_samples) 
      {
        # Loop over the cycles
        # Cycle 1 is already defined so only need to update cycles 2:n_cycles
        for (i_cycle in 2:self$n_cycles) 
        {
          # Markov update
          # Multiply previous cycle's cohort vector by transition matrix
          a_cohort_array[i_treatment, i_sample, i_cycle, ] <- 
            a_cohort_array[i_treatment, i_sample, i_cycle-1, ]%*%
            a_transition_matrices[i_treatment, i_sample, , ]
        }
        
     }
    }
    self$a_cohort_array <- a_cohort_array
    self$a_transition_matrices <- a_transition_matrices
    
    # Return the object
    invisible(self)
  }, # End generate_markov_trace
  
  
  #' @description
  #' Function to add the Markov trace calculation to an Excel workbook
  #' 
  #' @param wb_filename Location of the Excel workbook to which sheets will be added
  #' @param startCol Starting column number
  #' @param startRow Starting row number
  #' @param psa_startCol Location of cells to which numbers of states, treatments, and cycles are added
  #' @param psa_startRow Location of cells to which numbers of states, treatments, and cycles are added
  #' @examples 
  #' smoking_markov_model$export_to_excel("output/test_workbook.xlsx", startCol = 5, startRow = 8)
  #' smoking_markov_model$export_to_excel("output/test_workbook.xlsm", startCol = 5, startRow = 8, psa_startCol = 5, psa_startRow = 1)
  #' @export 
  #' 
  export_to_excel = function(wb_filename,
                             startCol = 5,
                             startRow = 8,
                             psa_startCol = 6,
                             psa_startRow = 1) {
    output_wb <- loadWorkbook(file = wb_filename, isUnzipped = FALSE)
    
    # Add the necessary sheets if not already included
    if(!is.element("input_parameters", names(output_wb))) {
      addWorksheet(output_wb, "input_parameters", tabColour = "green")  
    }
    
    if(!is.element("markov_trace", names(output_wb))) {
      addWorksheet(output_wb, "markov_trace", tabColour = "yellow")  
    }
    
    if(!is.element("PSA", names(output_wb))) {
      addWorksheet(output_wb, "PSA", tabColour = "orange")  
    }
    
    # Specify the Excel locations of the parameter values
    # These are used in the Markov trace
    self$markov_inputs$specify_excel(sheet = "input_parameters", 
                                     startCol = startCol, 
                                     startRow = startRow)
    
    # Add the input parameters, including random sampling formulae, to the Excel workbook
    writeData(output_wb, 
              sheet = "input_parameters", 
              x = self$markov_inputs$df_spec,
              startCol = startCol, 
              startRow = startRow)
    
    # Generate the Markov trace using transition probabilities from markov_inputs
    df_markov_trace <- data.frame(cycle = c(1:self$n_cycles))
    
    cell_formula_temp <- rep("", self$n_cycles)
    
    for(i_treatment in 1:self$n_treatments) {
      for(i_state in 1:self$n_states) {
        
        cell_formula_temp[1] <- self$v_init_cohort[i_state]
        
        # Which parameters give probabilities from this state and are relevant
        # to this treatment (or to all treatments)
        from_indices <- which(self$markov_inputs$df_spec$from == i_state & 
                                (self$markov_inputs$df_spec$v_treatment == i_treatment |
                                   is.na(self$markov_inputs$df_spec$v_treatment)))
        # Create the sum of probabilities of exiting current state
        sum_probabilities_from <- ifelse(length(from_indices) == 1,
                                         self$markov_inputs$df_spec$excel_value_location[from_indices],
                                         paste0(self$markov_inputs$df_spec$excel_value_location[from_indices], sep = "+"))
        
        for(i_cycle in 2:self$n_cycles) {
          # Numeric for row with previous cohort probabilities
          previous_row <- startRow + i_cycle - 1
          
          # Cell with probability of being in state at previous cycle
          cell_formula_temp[i_cycle] <- paste0(LETTERS[startCol + 
                                                         (i_treatment - 1) * self$n_states +
                                                         i_state], previous_row,
                                               " * (1 - ",
                                               sum_probabilities_from,")")
          # Now append the probabilities of entering the state
          from_prob_formulae <- c()
          for(j_state in c(1:self$n_states)[-i_state]) {
            # Find the parameter storing transition probabilities from j to i
            from_j_to_i_index <- self$markov_inputs$df_spec$from == j_state & 
              self$markov_inputs$df_spec$to == i_state &
              (self$markov_inputs$df_spec$v_treatment == i_treatment |
                 is.na(self$markov_inputs$df_spec$v_treatment))
            # Check that there is a transition from j to i
            if(sum(from_j_to_i_index, na.rm = TRUE) == 1) {
              # Add probability of being in j multiplied by probability of going to i
              from_prob_formulae <- c(from_prob_formulae,
                                      paste0(LETTERS[startCol + 
                                                       (i_treatment - 1) * self$n_states +
                                                       j_state], previous_row,
                                             " * ",
                                             self$markov_inputs$df_spec$excel_value_location[which(from_j_to_i_index)]))
              
            } # End if there are transitions from j to i
          } # End loop over j_state
          # Create the sum of probabilities of entering current state
          # Account for possibility that none of cohort makes this transition
          sum_probabilities_to <- ifelse(length(from_prob_formulae) == 0, "",
                                         ifelse(length(from_prob_formulae) == 1,
                                                from_prob_formulae,
                                                paste0(from_prob_formulae, sep = "+")))
          cell_formula_temp[i_cycle] <- paste0(cell_formula_temp[i_cycle], " + ", sum_probabilities_to)
        } # End loop over i_cycle
        # Append these formulae to the Markov trace
        class(cell_formula_temp) <- c(class(cell_formula_temp), "formula")
        df_markov_trace <- cbind(df_markov_trace, cell_formula_temp)
        # Give column a sensible header
        names(df_markov_trace)[(i_treatment - 1) * self$n_states + i_state + 1] <- paste0(self$v_treatment_names[i_treatment],
                                                                                     "_",
                                                                                     self$v_state_names[i_state])
      } # End loop over states
    } # End loop over treatments
    
  
    
    # Add the input parameters, including random sampling formulae, to the Excel workbook
    writeData(output_wb, 
              sheet = "markov_trace", 
              x = df_markov_trace,
              startCol = startCol, 
              startRow = startRow)
    
    # Add the model settings to guide the PSA of the Excel dummy template
    df_model_settings <- data.frame(setting_names = c("n cycles", "n states", "n treatments"),
                                      settings = c(self$n_cycles, 
                                                   self$n_states, 
                                                   self$n_treatments))
    writeData(output_wb, 
              sheet = "PSA", 
              x = df_model_settings,
              startCol = psa_startCol, 
              startRow = psa_startRow)
    
    saveWorkbook(output_wb, 
                 file = wb_filename, 
                 overwrite = TRUE)
  }
  
) # End of public list
) # End of R6Class
