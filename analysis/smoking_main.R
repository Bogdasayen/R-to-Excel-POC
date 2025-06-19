# Test of the R to Excel conversion proof of concept
# Creates a Markov model for smoking cessation following University of Bristol
# Only calculates, converts, and compares the Markov trace

# Medical School short course examples from Intro to EE and EE Modelling using R
# Howard Thom, Clifton Insight 20-October-2024

library(R2ExcelPOC)

set.seed(2345295)

# Update the documentation (if necessary)
#roxygen2::roxygenise()

# Load necessary functions from the package
# devtools::load_all()

# Specify the input parameters for the Markov smoking model
smoking_inputs <- input_parameters$new(
  v_names = c(
    "Probability quit smoking website",
    "Probability quit smoking SoC",
    "Probability relapse",
    "Utility smoking",
    "Utility not smoking",
    "Cost website",
    "Cost GP smoking",
    "Cost statin smoking"
  ),
  v_descriptions = c(
    "Probability of quitting if on smoking website, follows a beta distribution",
    "Probability of quitting smoking if on SoC, follows a beta distribution",
    "Probability relapse, which is same across treatments and follows a beta distribution",
    "Utility smoking, follows a Normal distribution",
    "Utility not smoking, follows a Normal distribution",
    "Cost website, fixed value and model assumes no cost of SoC and no state costs",
    "Cost of 6-monthly, on average, GP visit (£49 from PRSSU) for smoking related illness, follows Normal distribution",
    "Cost of roughly 20% of smokers taking statins (pravastatin at £3.45 per month), follows Normal distribution"
  ),
  v_type = c(
    "transition_probability",
    "transition_probability",
    "transition_probability",
    "utility",
    "utility",
    "one_off_cost",
    "cost",
    "cost"
  ),
  v_distributions = c(
    "beta",
    "beta",
    "beta",
    "normal",
    "fixed",
    "fixed",
    "normal",
    "normal"
  ),
  m_hyperparameters = matrix(
    c(
      15,
      85,
      12,
      88,
      8,
      92,
      0.95,
      0.02,
      1,
      NA,
      50,
      NA,
      49,
      2,
      0.2 * 3.45,
      0.1 * 0.2 * 3.45
    ),
    nrow = 8,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("hp_1", "hp_2"))
  ),
  m_transition = matrix(
    c(1, 2, 1, 2, 2, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    nrow = 8,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("from", "to"))
  ),
  v_treatment = c(2, 1, NA, NA, NA, 2, NA, NA),
  v_state = c(1, 1, 2, 1, 2, NA, 1, 1)
)


# Define the Markov smoking model
markov_smoking <- markov_model$new(
  n_states = 2,
  n_cycles = 10,
  cycle_length = 0.5,
  n_samples = 1000,
  n_treatments = 2,
  v_state_names = c("Smoking", "Not smoking"),
  v_treatment_names = c("SoC", "SoC with website"),
  lambda = 20000,
  costs_dr = 0.035,
  qalys_dr = 0.035,
  markov_inputs = smoking_inputs,
  v_init_cohort = c(1, 0)
)

# Sample values for the fixed and random parameters
markov_smoking$generate_input_parameters()

# Check the sampled values for the input parameter matrix
markov_smoking$markov_inputs$m_values

markov_smoking$generate_transition_matrices()


# Check one sample of the transition matrices for each treatment
# Note that probability of relapse is the same across website and SoC
# And note that transition matrices sum to 1
markov_smoking$a_transition_matrices[1, 1, , ]
markov_smoking$a_transition_matrices[2, 1, , ]

# Generate the Markov trace
markov_smoking$generate_markov_trace()


markov_smoking$generate_costs_qalys()

# Summarise the results
markov_smoking$generate_results_table()
write.csv(
  markov_smoking$generate_results_table(),
  file = "output/results_table_smoking_1.csv"
)


# And export to Excel
# Need to export model settings (states, cycles, treatments) and
# sensible names to PSa. Ideally also add formatting to Markov trace and input
# parameter headings
markov_smoking$export_to_excel(
  wb_filename = "output/test_output_smoking_1.xlsm"
)


# Check properties of the Markov trace
dim(markov_smoking$a_cohort_array)
# On SoC patients quit smoking at average probability of 12%
markov_smoking$a_cohort_array["SoC", 1, , ]
# On Website patients quit smoking at average probability of 15%
markov_smoking$a_cohort_array["SoC with website", 1, , ]

# For comparison with Excel, average time in each state across PSA samples
# Average time not smoking on SoC is about 0.617 while with website is 0.672
apply(markov_smoking$a_cohort_array, c(1, 4), mean)

openxlsx2::xl_open("output/test_output_smoking_1.xlsm")
