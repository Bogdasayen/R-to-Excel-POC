# Test of the R to Excel conversion proof of concept
# Creates a Markov model for smoking cessation following University of Bristol
# Only calculates, converts, and compares the Markov trace

# Medical School short course examples from Intro to EE and EE Modelling using R
# Howard Thom, Clifton Insight 20-October-2024

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
    "quit_log_shape_soc",
    "quit_log_shape_soc_website", 
    "quit_log_scale_soc", 
    "quit_log_scale_soc_website",
    "relapse_log_rate",
    "Utility smoking",
    "Utility not smoking",
    "Cost website",
    "Cost GP smoking",
    "Cost statin smoking"
  ),
  v_descriptions = c(
    "Shape of Weibull for probability of quitting on website",
    "Shape of Weibull for probability of quitting on SoC",
    "Scale of Weibull for probability of quitting on website",
    "Scale of Weibull for probability of quitting on SoC",
    "Fixed log rate for probability of relapse",
    "Utility smoking, follows a Normal distribution",
    "Utility not smoking, follows a Normal distribution",
    "Cost website, fixed value and model assumes no cost of SoC and no state costs",
    "Cost of 6-monthly, on average, GP visit (£49 from PRSSU) for smoking related illness, follows Normal distribution",
    "Cost of roughly 20% of smokers taking statins (pravastatin at £3.45 per month), follows Normal distribution"
  ),
  v_type = c(
    "time_dependent_hyperparameter",
    "time_dependent_hyperparameter",
    "time_dependent_hyperparameter",
    "time_dependent_hyperparameter",
    "time_dependent_hyperparameter",
    "utility",
    "utility",
    "one_off_cost",
    "cost",
    "cost"
  ),
  v_distributions = c(
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "fixed",
    "fixed",
    "normal",
    "normal"
  ),
  m_hyperparameters = matrix(
    c(
      0.3, # Chose values to give probability in first cycle close to time-independent model
      0.03, # Set SD to 10% of mean
      0.3,
      0.03,
      0.653,
      0.0653,
      0.831,
      0.0831,
      -1.79, # Relapse log rate corresponds to 8% probability
      0.179, # Set SD to 10% of mean
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
    nrow = 10,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("hp_1", "hp_2"))
  ),
  m_transition = matrix(
    c(1, 2, 1, 2, 1, 2, 1, 2, 2, 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
    nrow = 10,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("from", "to"))
  ),
  v_treatment = c(2, 1, 2, 1, NA, NA, NA, 2, NA, NA),
  v_state = c(1, 1, 1, 1, 2, 1, 2, NA, 1, 1)
)

# Specify the time-dependent transition probabilities
smoking_time_dependent_settings <- data.frame(
  from = c(1, 1, 2),
  to = c(2, 2, 1),
  v_treatment = c(1, 2, NA),
  v_distributions = c("weibull", "weibull", "exp"),
  v_par1 = c("quit_log_shape_soc", "quit_log_shape_soc_website", "relapse_log_rate"),
  v_par2 = c("quit_log_scale_soc", "quit_log_scale_soc_website", NA),
  v_par3 = c(NA, NA, NA),
  v_description = c("Model for probability of quitting on website, follows a Weibull curve",
                    "Model for probability of quitting on SoC, follows a Weibull curve",
                    "Model for probability of relapse, follows a fixed rate (exponential model) over time")
)


# Define the Markov smoking model
markov_smoking <- markov_model$new(
  n_states = 2,
  n_cycles = 10,
  cycle_length = 0.5,
  n_samples = 1000,
  n_treatments = 2,
  deterministic_flag = FALSE,
  time_dependent_flag = TRUE,
  v_state_names = c("Smoking", "Not smoking"),
  v_treatment_names = c("SoC", "SoC with website"),
  lambda = 20000,
  costs_dr = 0.035,
  qalys_dr = 0.035,
  markov_inputs = smoking_inputs,
  df_time_dependent_settings = smoking_time_dependent_settings,
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
markov_smoking$a_transition_matrices[1, 1, 1, , ]
markov_smoking$a_transition_matrices[2, 1, 1, , ]

# Generate the Markov trace
markov_smoking$generate_markov_trace()


markov_smoking$generate_costs_qalys()

# Summarise the results
markov_smoking$generate_results_table()

write.csv(
  markov_smoking$generate_results_table(),
  file = "output/results_table_smoking.csv"
)


# And export to Excel
# Need to export model settings (states, cycles, treatments) and
# sensible names to PSa. Ideally also add formatting to Markov trace and input
# parameter headings
markov_smoking$export_to_excel(
  wb_filename = "output/test_output_smoking.xlsm"
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
