# Test of the R to Excel conversion proof of concept
# Creates a Markov model for smoking cessation following University of Bristol
# Adds a 'dead' state to the example

# Medical School short course examples from Intro to EE and EE Modelling using R
# Howard Thom, Clifton Insight 13-July-2026

# To load the library would first need to click the menu "Build"  and button "Install"
# library(R2ExcelPOC)

set.seed(2345295)

# Update the documentation (if necessary)
#roxygen2::roxygenise()

# Load necessary functions from the package
devtools::load_all()

# Specify the input parameters for the Markov smoking model
smoking_inputs <- input_parameters$new(
  v_names = c(
    "Probability quit smoking website",
    "Probability quit smoking SoC",
    "Probability relapse",
    "Probability death smoking",
    "Probability death not smoking",
    "Utility smoking",
    "Utility not smoking",
    "Utility dead",
    "Cost website",
    "Cost GP smoking",
    "Cost statin smoking",
    "Cost dead"
  ),
  v_descriptions = c(
    "Probability of quitting if on smoking website, follows a beta distribution",
    "Probability of quitting smoking if on SoC, follows a beta distribution",
    "Probability relapse, which is same across treatments and follows a beta distribution",
    "Probability of death from smoking, which is same across treatments and follows a beta distribution",
    "Probability of death from not smoking, which is same across treatments and follows a beta distribution",
    "Utility smoking, follows a Normal distribution",
    "Utility not smoking, follows a Normal distribution",
    "Utility dead, fixed at zero",
    "Cost website, fixed value and model assumes no cost of SoC and no state costs",
    "Cost of 6-monthly, on average, GP visit (£49 from PRSSU) for smoking related illness, follows Normal distribution",
    "Cost of roughly 20% of smokers taking statins (pravastatin at £3.45 per month), follows Normal distribution",
    "Cost per cycle in dead, fixed at zero"
  ),
  v_type = c(
    "transition_probability",
    "transition_probability",
    "transition_probability",
    "transition_probability",
    "transition_probability",
    "utility",
    "utility",
    "utility",
    "one_off_cost",
    "cost",
    "cost",
    "cost"
  ),
  v_distributions = c(
    "beta",
    "beta",
    "beta",
    "beta",
    "beta",
    "normal",
    "fixed",
    "fixed",
    "fixed",
    "normal",
    "normal",
    "fixed"
  ),
  m_hyperparameters = matrix(
    c(
      15,
      85,
      12,
      88,
      8,
      92,
      2,
      98,
      1,
      99,
      0.95,
      0.02,
      1,
      NA,
      0,
      NA,
      50,
      NA,
      49,
      2,
      0.2 * 3.45,
      0.1 * 0.2 * 3.45,
      0,
      NA
    ),
    nrow = 12,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("hp_1", "hp_2"))
  ),
  m_transition = matrix(
    c(1, 2, 1, 2, 2, 1, 1, 3, 2, 3, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    nrow = 12,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("from", "to"))
  ),
  v_treatment = c(2, 1, NA, NA, NA, NA, NA, NA, 2, NA, NA, NA),
  v_state = c(1, 1, 2, 3, 3, 1, 2, 3, NA, 1, 1, 3)
)


# Define the Markov smoking model
markov_smoking <- markov_model$new(
  n_states = 3,
  n_cycles = 10,
  cycle_length = 0.5,
  n_samples = 1000,
  n_treatments = 2,
  deterministic_flag = FALSE,
  v_state_names = c("Smoking", "Not smoking", "Dead"),
  v_treatment_names = c("SoC", "SoC with website"),
  lambda = 20000,
  costs_dr = 0.035,
  qalys_dr = 0.035,
  markov_inputs = smoking_inputs,
  v_init_cohort = c(1, 0, 0)
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
  file = "output/results_table_smoking_added_state.csv"
)


# And export to Excel
# Need to export model settings (states, cycles, treatments) and
# sensible names to PSA. Ideally also add formatting to Markov trace and input
# parameter headings
markov_smoking$export_to_excel(
  wb_filename = "output/test_output_smoking_added_state.xlsm"
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

openxlsx2::xl_open("output/test_output_smoking.xlsm")
