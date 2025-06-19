# Test of the R to Excel conversion proof of concept
# Creates a Markov model for hip replacement following University of Bristol
# Only calculates, converts, and compares the Markov trace

# Medical School short course examples from Advanced Markov Models
# Howard Thom, Clifton Insight 20-October-2024

library(R2ExcelPOC)

set.seed(2345295)

# Update the documentation (if necessary)
#roxygen2::roxygenise()

# Load necessary functions from the package
# devtools::load_all()

state_names = c("post_thr", "post_1st_rev", "post_2nd_rev", "dead")
treatment_names = c("cemented", "uncemented", "hybrid", "reverse_hybrid")


# Specify the input parameters for the Markov hips model
hips_inputs <- input_parameters$new(
  v_names = c(
    paste0("Probability_1st_revision_", treatment_names),
    "Probability_2nd_revision",
    rep("Probability_death", 3),
    paste0("cost_1st_revision_", treatment_names),
    "cost_2nd_revision",
    "cost_higher_revision",
    paste0("state_utility_", state_names),
    paste0("implant_cost_", treatment_names)
  ),
  v_descriptions = c(
    paste0(
      "Probability of 1st revision if using ",
      treatment_names,
      ", follows a normal distribution"
    ),
    "Probability of 2nd revision, follows a normal distribution",
    rep("Probability of death, follows a normal distribution", 3),
    paste0(
      "Cost of revision of post THR state if using ",
      treatment_names,
      ", follows a normal distribution, with fixed revision cost and normally distributed probability"
    ),
    "Cost of revision of post 1st revision state, follows a normal distribution, with fixed revision cost and normally distributed probability",
    "Cost of revision of post 2nd revision state, follows a normal distribution, with fixed revision cost and normally distributed probability",
    "Utility of post THR state, follows a normal distribution ",
    "Utility of post 1st revision state, follows a normal distribution ",
    "Utility of post 2nd revision state, follows a normal distribution ",
    "Utility of death, fixed value and model assumes no cost",
    paste0("Cost of", treatment_names, ", fixed value")
  ),
  v_type = c(
    "transition_probability",
    "transition_probability",
    "transition_probability",
    "transition_probability",
    "transition_probability",
    "transition_probability",
    "transition_probability",
    "transition_probability",
    "cost",
    "cost",
    "cost",
    "cost",
    "cost",
    "cost",
    "utility",
    "utility",
    "utility",
    "utility",
    "one_off_cost",
    "one_off_cost",
    "one_off_cost",
    "one_off_cost"
  ),
  v_distributions = c(
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "normal",
    "fixed",
    "fixed",
    "fixed",
    "fixed",
    "fixed"
  ),
  m_hyperparameters = matrix(
    c(
      c(rbind(
        p1 <- c(0.0047, 0.0030, 0.0037, 0.0040, 0.042),
        se1 <- c(0.031, 0.031, 0.031, 0.031, 0.073) * (-log(1 - p1)) * (1 - p1)
      )),
      rep(c(0.066, 0.066 * 0.1), 3),
      9004.95 *
        c(rbind(
          p2 <- c(0.0047, 0.0030, 0.0037, 0.0040, 0.042, 0.056),
          se2 <- c(0.031, 0.031, 0.031, 0.031, 0.073, 0.053) *
            (-log(1 - p2)) *
            (1 - p2)
        )),
      0.762,
      0.004,
      0.575,
      0.009,
      0.575,
      0.009,
      0,
      NA,
      756.00,
      NA,
      2386.00,
      NA,
      1597.00,
      NA,
      1446.00,
      NA
    ),
    nrow = 22,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("hp_1", "hp_2"))
  ),
  m_transition = matrix(
    c(
      1,
      2,
      1,
      2,
      1,
      2,
      1,
      2,
      2,
      3,
      1,
      4,
      2,
      4,
      3,
      4,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA,
      NA
    ),
    nrow = 22,
    ncol = 2,
    byrow = TRUE,
    dimnames = list(NULL, c("from", "to"))
  ),
  v_treatment = c(
    1,
    2,
    3,
    4,
    NA,
    NA,
    NA,
    NA,
    1,
    2,
    3,
    4,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    1,
    2,
    3,
    4
  ),
  v_state = c(
    1,
    1,
    1,
    1,
    2,
    1,
    2,
    3,
    1,
    1,
    1,
    1,
    2,
    3,
    1,
    2,
    3,
    4,
    NA,
    NA,
    NA,
    NA
  )
)


# Define the Markov hips model
markov_hips <- markov_model$new(
  n_states = 4,
  n_cycles = 30,
  cycle_length = 1,
  n_samples = 1000,
  n_treatments = 4,
  v_state_names = state_names,
  v_treatment_names = treatment_names,
  lambda = 20000,
  costs_dr = 0.035,
  qalys_dr = 0.035,
  markov_inputs = hips_inputs,
  v_init_cohort = c(1, 0, 0, 0)
)

# Sample values for the fixed and random parameters
markov_hips$generate_input_parameters()

# Check the sampled values for the input parameter matrix
markov_hips$markov_inputs$m_values

markov_hips$generate_transition_matrices()


# Check one sample of the transition matrices for each treatment
# Note that probabilities after post 1st revision state is the same across treatments
# Note that transition matrices sum to 1
markov_hips$a_transition_matrices[1, 1, , ]
markov_hips$a_transition_matrices[2, 1, , ]
markov_hips$a_transition_matrices[3, 1, , ]
markov_hips$a_transition_matrices[4, 1, , ]

# Generate the Markov trace
markov_hips$generate_markov_trace()


markov_hips$generate_costs_qalys()

# Summarise the results
markov_hips$generate_results_table()
write.csv(
  markov_hips$generate_results_table(),
  file = "output/results_table_hips_1.csv"
)


# And export to Excel
# Need to export model settings (states, cycles, treatments) and
# sensible names to PSa. Ideally also add formatting to Markov trace and input
# parameter headings
markov_hips$export_to_excel(wb_filename = "output/test_output_hips_1.xlsm")


# Check properties of the Markov trace
dim(markov_hips$a_cohort_array)
#
markov_hips$a_cohort_array["cemented", 1, , ]
#
markov_hips$a_cohort_array["uncemented", 1, , ]

# For comparison with Excel, average time in each state across PSA samples
apply(markov_hips$a_cohort_array, c(1, 4), mean)

openxlsx2::xl_open("output/test_output_hips_1.xlsm")
