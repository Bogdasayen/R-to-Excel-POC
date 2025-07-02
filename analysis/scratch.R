n_parameters = 6
n_samples = 1000
v_names = c(
  "Probability quit smoking website",
  "Probability quit smoking SoC",
  "Probability relapse",
  "Utility smoking",
  "Utility not smoking",
  "Cost website"
)
v_descriptions = c(
  "Probability continue smoking website, follows a beta distribution",
  "Probability continue smoking SoC, follows a beta distribution",
  "Probability relapse, which is same across treatments and follows a beta distribution",
  "Utility smoking, follows a Normal distribution",
  "Utility not smoking, follows a Normal distribution",
  "Cost website, fixed value and model assumes no cost of SoC and no state costs"
)
v_distributions = c("beta", "beta", "beta", "normal", "fixed", "fixed")
m_hyperparameters = matrix(
  c(85, 15, 88, 12, 8, 92, 0.95, 0.01, 0.5, NA, 50, NA),
  nrow = 6,
  ncol = 2,
  byrow = TRUE,
  dimnames = list(NULL, c("hp_1", "hp_2"))
)
m_transition = matrix(
  c(1, 1, 1, 1, 2, 1, NA, NA, NA, NA, NA, NA),
  nrow = 6,
  ncol = 2,
  byrow = TRUE,
  dimnames = list(NULL, c("from", "to"))
)
v_treatment = c(1, 2, NA, NA, NA, NA)

self <- list()
self$n_parameters = n_parameters
self$n_samples = n_samples

self$df_spec <- data.frame(
  v_names,
  v_descriptions,
  v_distributions,
  m_hyperparameters,
  m_transition,
  v_treatment
)

# First parameter
i_parameter <- 1
# Starting position in Excel sheet
startRow = 5
startCol = 8

# In which column letters would the hyper parameters be stored
hp_1_col <- LETTERS[startCol + 1]
hp_2_col <- LETTERS[startCol + 2]

paste0(
  "NORMINV(RAND(), ",
  paste0(hp_1_col, startRow + i_parameter),
  ", ",
  paste0(hp_2_col, startRow + i_parameter),
  ")"
)
