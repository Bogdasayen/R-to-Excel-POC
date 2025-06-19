#' Summarise MCMC samples
#'
#' Function to calculate mean and 95% credible intervals using MCMC or PSA samples
#' @param v_samples Vector of samples from an MCMC or PSA simulation
#' @param n_digits Number of significant digits to include
#' @return Summary statistics in Mean (95% CrI) style
#' @examples summarise_samples(v_samples = c(0.2, 0.3, 0.25, 0.4, 0.3, 0.22), n_digits = 2)
#' @export
summarise_samples <- function(v_samples, n_digits = 3) {
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
