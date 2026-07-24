# R-to-Excel-POC
Proof of Excel for cost-effectiveness model template that exports an Excel version

## Installation

``` r
# install.packages("remotes")
remotes::install_github("Bogdasayen/R-to-Excel-POC")

# load the library
library(R2ExcelPOC)
```

From within RStudio click on “File” and then "Open Project..." and then navigate to “R-to-Excel-POC.Rproj”

Ensure you see the text "R-to-Excel-POC" in the top right corner of RStudio

In the ‘analysis’ subfolder open the “smoking_main.R” script

Run all. This loads the Markov smoking model into the template, including input data from "data/smoking_inputs.xlsx", and generates an Excel equivalent in "output/test_output_smoking.xlsm"

Load the Excel sheet "output/test_output_smoking.xlsm" in Excel and navigate to the “PSA” tab. The exported input parameters are stored in the “input_parameters” tab and the reconstructed Markov trace in “markov_trace”

Click the “Run PSA” button and look at total times in state, costs, and QALYs in J3:U3

Comparing these verifies that the R and Excel models are approximately in agreement. 

Other example models are provided in the 'analysis' subfolder including "hips_main.R" (a 4-state Markov model for hip replacement surgery), "smoking_main_added_state.R" (which adds a death state to the smoking cessation example), and "smoking_main_time_dependent.R" (which makes the cycle-specific quit probability follow a time-dependent Weibull distribution).
