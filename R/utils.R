#' Summarise MCMC samples
#'
#' Function to calculate mean and 95% credible intervals using MCMC or PSA samples
#' @param v_samples Vector of samples from an MCMC or PSA simulation
#' @param n_digits Number of significant digits to include
#' @return Summary statistics in Mean (95% CrI) style (Or single value if only one sample provided)
#' @examples summarise_samples(v_samples = c(0.2, 0.3, 0.25, 0.4, 0.3, 0.22), n_digits = 2)
#' @export
summarise_samples <- function(v_samples, n_digits = 3) {
  if(length(v_samples) == 1) {
    return(v_samples)
  } else {
    return(paste0(
      format(mean(v_samples), digits = n_digits, nsmall = n_digits),
      " (",
      format(
        quantile(v_samples, probs = 0.025),
        digits = n_digits,
        nsmall = n_digits
      ),
      ", ",
      format(
        quantile(v_samples, probs = 0.975),
        digits = n_digits,
        nsmall = n_digits
      ),
      ")"
    ))
    
  }
}

#' Calculate means
#' 
#' Function to calculate means based on type and hyperparameters for a range of accepted distributions
#' @param v_distributions Vector of strings specifying distributions (options are fixed, beta, normal)
#' @param m_hyperparameters Matrix of parameters of the corresponding distribution for this parameter (at least one value, and unused are NULL)
#' @return Vector of means for each of the supplied distributions
#' @export
calculate_means <- function(v_distributions, m_hyperparameters) {
  
  v_means <- rep(NA, length(v_distributions))
  for(i_distribution in 1:length(v_distributions)) {
    # Check that distribution is supported
    if(!is.element(v_distributions[i_distribution], c("fixed", "normal", "beta"))) {
      stop(paste0("Unsupported distribution ", v_distributions[i_distribution], " passed to function calculate_means"))
    }
    if(v_distributions[i_distribution] == "fixed") {
      v_means[i_distribution] <- m_hyperparameters[i_distribution, 1]
    } 
    if(v_distributions[i_distribution] == "normal") {
      v_means[i_distribution] <- m_hyperparameters[i_distribution, 1]
    }
    if(v_distributions[i_distribution] == "beta") {
      v_means[i_distribution] <- m_hyperparameters[i_distribution, 1] / sum(m_hyperparameters[i_distribution, c(1:2)])
    } 
  }
  return(v_means)
}

#' Calculate change in survival probability
#' 
#' Function calculates change in probability from start to end time for vectors of sampled parameters
#' Used by markov_model class function generate_transition_matrices() for the time-dependent case
#' Supports vectors of parameters for probabilistic sensitivity analysis
#' @param start_time Start time in years
#' @param end_time End time in years
#' @param distribution_name Name of distribution (currently supports exp and Weibull)
#' @param v_par1 Vector of parameter 1 on linear predictor scale (e.g., log rate for exponential or log shape for Weibull)
#' @param v_par2 Vector of parameter 2 on linear predictor scale (e.g., log scale for Weibull)
#' @param v_par3 Parameter 3 on linear predictor scale (e.g., to allow future generalised gamma)
#' @return Vector of probabilities for each set of supplied parameters
#' @export
calculate_survival_probability <- function(start_time, 
                                           end_time, 
                                           distribution_name, 
                                           v_par1, 
                                           v_par2 = NULL, 
                                           v_par3 = NULL) {
  
  if(distribution_name == "weibull") {
    probs <- 
      pweibull(end_time, shape = exp(v_par1), scale = exp(v_par2)) - 
      pweibull(start_time, shape = exp(v_par1), scale = exp(v_par2))
  }
  if(distribution_name == "exp") {
    probs <- 
      pexp(end_time, exp(v_par1)) - 
      pexp(start_time, exp(v_par1))
  }
  return(probs)
}