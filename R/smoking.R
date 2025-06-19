#' Smoking-Related Model Inputs
#'
#' A structured list containing inputs for a smoking-related health economic model.
#' This dataset includes variable names, descriptions, types, distributions, and
#' associated parameters for use in cost-effectiveness or Markov modeling.
#'
#' @format A list with 7 components:
#' \describe{
#'   \item{v_names}{A character vector of variable names (length 8).}
#'   \item{v_descriptions}{A character vector describing each variable (length 8).}
#'   \item{v_type}{A character vector indicating the type of each variable, e.g., \code{"transition_probability"}, \code{"utility"}, \code{"cost"}, etc. (length 8).}
#'   \item{v_distributions}{A character vector indicating the probability distribution used for each variable (length 8).}
#'   \item{m_hyperparameters}{An 8x2 matrix of distribution hyperparameters. The meaning of the two columns depends on the distribution type (e.g., alpha and beta for beta distribution, mean and SD for normal).}
#'   \item{m_transition}{An 8x2 matrix showing the transition from one health state to another for transition probability variables. \code{NA} for non-transition variables. Columns are \code{from} and \code{to}.}
#'   \item{v_treatment}{A numeric vector indicating which treatment the variable applies to (1 = SoC, 2 = Website, NA = not treatment-specific).}
#'   \item{v_state}{A numeric vector indicating which health state the variable applies to (1 = Smoking, 2 = Not smoking, NA = not state-specific).}
#' }
#'
#' @usage data(smoking)
#'
#' @examples
#' data(smoking)
#' str(smoking)
#' smoking$v_names
#' smoking$m_hyperparameters
"smoking"

# smoking <- list(
#   v_names = c(
#     "Probability quit smoking website",
#     "Probability quit smoking SoC",
#     "Probability relapse",
#     "Utility smoking",
#     "Utility not smoking",
#     "Cost website",
#     "Cost GP smoking",
#     "Cost statin smoking"
#   ),
#   v_descriptions = c(
#     "Probability of quitting if on smoking website, follows a beta distribution",
#     "Probability of quitting smoking if on SoC, follows a beta distribution",
#     "Probability relapse, which is same across treatments and follows a beta distribution",
#     "Utility smoking, follows a Normal distribution",
#     "Utility not smoking, follows a Normal distribution",
#     "Cost website, fixed value and model assumes no cost of SoC and no state costs",
#     "Cost of 6-monthly, on average, GP visit (£49 from PRSSU) for smoking related illness, follows Normal distribution",
#     "Cost of roughly 20% of smokers taking statins (pravastatin at £3.45 per month), follows Normal distribution"
#   ),
#   v_type = c(
#     "transition_probability",
#     "transition_probability",
#     "transition_probability",
#     "utility",
#     "utility",
#     "one_off_cost",
#     "cost",
#     "cost"
#   ),
#   v_distributions = c(
#     "beta",
#     "beta",
#     "beta",
#     "normal",
#     "fixed",
#     "fixed",
#     "normal",
#     "normal"
#   ),
#   m_hyperparameters = matrix(
#     c(
#       15,
#       85,
#       12,
#       88,
#       8,
#       92,
#       0.95,
#       0.02,
#       1,
#       NA,
#       50,
#       NA,
#       49,
#       2,
#       0.2 * 3.45,
#       0.1 * 0.2 * 3.45
#     ),
#     nrow = 8,
#     ncol = 2,
#     byrow = TRUE,
#     dimnames = list(NULL, c("hp_1", "hp_2"))
#   ),
#   m_transition = matrix(
#     c(1, 2, 1, 2, 2, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
#     nrow = 8,
#     ncol = 2,
#     byrow = TRUE,
#     dimnames = list(NULL, c("from", "to"))
#   ),
#   v_treatment = c(2, 1, NA, NA, NA, 2, NA, NA),
#   v_state = c(1, 1, 2, 1, 2, NA, 1, 1)
# )
