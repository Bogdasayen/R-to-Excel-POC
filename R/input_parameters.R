require(R6)

#' The input parameters class
#' @description Class to store input parameter specification and samples for models
#' 
#' @field n_parameters Number of parameters in the model
#' @field n_samples Number of probabilistic samples
#' @field df_spec Data frame specifying names and distributions of parameters
#' @field m_values Matrix of sample values defined by member function sample_values()
#' @export
input_parameters <- R6Class("markov_model", list(
  # Assign variable for all public elements of the class
  # These are defined in the initialize() function
  
  n_parameters = NULL,
  n_samples = NULL,
  df_spec = NULL,
  m_values = NULL,
  
  # This function is run when an object of this class is created
  # Arguments are modifiable
  
  #' @description 
  #' Initialise the input parameters object
  #' 
  #' @param v_names Vector of string names of the parameters (can be NULL)
  #' @param v_descriptions Vector of string descriptions of the parameters (can be NULL)
  #' @param v_type Vector of strings specifying type of parameter (e.g., transition_probability, one_off_cost, cost)
  #' @param v_distributions Vector of strings specifying distributions (options are fixed, beta, normal)
  #' @param m_hyperparameters Matrix of parameters of the corresponding distribution for this parameter (at least one value, and unused are NULL)
  #' @param m_transition Matrix with from and to values identifying transitions which parameter informs (NA if not a transition probability parameter)
  #' @param v_treatment Numeric vector identifying the treatment to which this parameter corresponds (NA if all) 
  #' @param v_state Numeric vector identifying the state to which this cost or utility parameter corresponds  (NA if all)
  #' @return input parameter object with df_spec giving specifications
  #' @examples
  #' markov_inputs <- input_parameters$new(v_names = c("Probability quit smoking website",
  #'"Probability quit smoking SoC",
  #'"Probability relapse",
  #'"Utility smoking",
  #'"Utility not smoking",
  #'"Cost website"),
  #'v_descriptions = c("Probability continue smoking website, follows a beta distribution",
  #'                   "Probability continue smoking SoC, follows a beta distribution",
  #'                   "Probability relapse, which is same across treatments and follows a beta distribution",
  #'                   "Utility smoking, follows a Normal distribution",
  #'                   "Utility not smoking, follows a Normal distribution",
  #'                   "Cost website, fixed value and model assumes no cost of SoC and no state costs"
  #'                   "Cost of 6-monthly, on average, GP visit (£49 from PRSSU) for smoking related illness, follows Normal distribution",
  #'                   "Cost of roughly 20% of smokers taking statins (pravastatin at £3.45 per month), follows Normal distribution"),
  #' v_type = c("transition_probability", "transition_probability", "transition_probability", "utility", "utility", "one_off_cost", "cost", "cost"),
  #'v_distributions = c("beta", "beta", "beta", "normal", "fixed", "fixed", "normal", "normal"),
  #'m_hyperparameters = matrix(c(15, 85,
  #'                             12, 88,
  #'                             8, 92,
  #'                             0.95, 0.02,
  #'                             1.0, NA,
  #'                             50, NA,
  #'                              49, 2,
  #'                             0.69, 0.069), 
  #'                           nrow = 8, ncol = 2, byrow = TRUE,
  #'                           dimnames = list(NULL, c("hp_1", "hp_2"))),
  #' m_transition = matrix(c(1, 2,
  #'                                                   1, 2,
  #'                                                   2, 1,
  #'                                                   NA, NA,
  #'                                                   NA, NA,
  #'                                                   NA, NA,
  #'                                                   NA, NA),
  #'                                                 nrow = 8, ncol = 2, byrow = TRUE,
  #'                                                 dimnames = list(NULL, c("from", "to"))),
  #' v_treatment = c(2, 1, NA, NA, NA, 1, NA, NA),
  #' v_state = c(1, 1, 2, 1, 2, NA, 1, 1)) 
  #'
  #' @export
  initialize = function(v_names,
                        v_descriptions,
                        v_type,
                        v_distributions,
                        m_hyperparameters,
                        m_transition,
                        v_treatment,
                        v_state) {
    self$n_parameters = length(v_names)
    self$df_spec <- data.frame(v_names,
                               v_descriptions,
                               v_type,
                               v_distributions,
                               m_hyperparameters,
                               m_transition,
                               v_treatment,
                               v_state)
    self$m_values = NULL
  }, # End initialize function
  
  #' @description
  #' Function to generate random parameters based on the specification
  #' 
  #' @param n_samples Number of random samples to draw
  #' @examples 
  #' markov_inputs$sample_values(n_samples = 1000)
  #' @export 
  #' 
  sample_values = function(n_samples) {
    self$n_samples = n_samples
    self$m_values <- matrix(NA,
                            nrow = self$n_samples,
                            ncol = self$n_parameters,
                            dimnames = list(NULL, 
                                            self$df_spec$v_names))
    
    for(i_parameter in 1:self$n_parameters) {
      if(self$df_spec$v_distributions[i_parameter] == "beta") {
        self$m_values[, i_parameter] <- rbeta(self$n_samples,
                                                    self$df_spec$hp_1[i_parameter],
                                                    self$df_spec$hp_2[i_parameter])  
      }
      if(self$df_spec$v_distributions[i_parameter] == "fixed") {
        self$m_values[, i_parameter] <- self$df_spec$hp_1[i_parameter] 
      }
      if(self$df_spec$v_distributions[i_parameter] == "normal") {
        self$m_values[, i_parameter] <- rnorm(self$n_samples,
                                                    self$df_spec$hp_1[i_parameter],
                                                    self$df_spec$hp_2[i_parameter]) 
      }
    }
    # Return the object
    invisible(self)
  }, # End sample values function
  
  #' @description
  #' Function to add specification of parameters in Excel format
  #' 
  #' @param sheet Name of sheet to which input parameters are added
  #' @param startCol Column number from which the input parameters are added
  #' @param startRow Row number from which the input parameters are added
  #' @examples 
  #' markov_inputs$specify_excel()
  #' @export 
  #' 
  specify_excel = function(sheet, startCol = 1, startRow = 1) {
    excel_formulae <- rep("", self$n_parameters)
    
    # In which column letters would the hyper parameters be stored
    hp_1_col <- LETTERS[startCol + which(names(self$df_spec) == "hp_1") - 1]
    hp_2_col <- LETTERS[startCol + which(names(self$df_spec) == "hp_2") - 1]
    
    # Export the Excel equivalent of each distribution function
    for(i_parameter in 1:self$n_parameters) {
      if(self$df_spec$v_distributions[i_parameter] == "beta") {
        # Note that previously the deprecated inverse distribution BETAINV() was used and not BETA.INV()
        excel_formulae[i_parameter] <- paste0("_xlfn.BETA.INV(RAND(), ",
                                              paste0(hp_1_col, startRow + i_parameter),
                                              ", ",
                                              paste0(hp_2_col, startRow + i_parameter),
                                              ")")
      }
      if(self$df_spec$v_distributions[i_parameter] == "fixed") {
        excel_formulae[i_parameter] <- paste0(hp_1_col, startRow + i_parameter)
      }
      if(self$df_spec$v_distributions[i_parameter] == "normal") {
        # Note that previously the deprecated inverse distribution NORMINV() was used and not NORM.INV()
        excel_formulae[i_parameter] <- paste0("_xlfn.NORM.INV(RAND(), ",
                                              paste0(hp_1_col, startRow + i_parameter),
                                              ", ",
                                              paste0(hp_2_col, startRow + i_parameter),
                                              ")")
      }
    }
    class(excel_formulae) <- c(class(excel_formulae), "formula")
    self$df_spec$excel_formulae <- excel_formulae
    
    
    # Specify the cell holding the sampled value
    excel_value_location <- rep(paste0(sheet, 
                                       "!", 
                                       LETTERS[startCol + which(names(self$df_spec) == "excel_formulae") - 1], 
                                       c((startRow + 1):(startRow + self$n_parameters))))
    self$df_spec$excel_value_location <- excel_value_location
    
    # Return the object
    invisible(self)
  } # End Excel specification function
  
) # End of public list
) # End of R6Class
