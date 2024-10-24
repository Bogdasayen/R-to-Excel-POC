# R-to-Excel-POC
Proof of Excel for cost-effectiveness model template that exports an Excel version

From within RStudio click on “File” and then "Open Project..." and then navigate to “R-to-Excel-POC.Rproj”

Ensure you see the text "R-to-Excel-POC" in the top right corner of RStudio

In the ‘analysis’ subfolder open the “smoking_main.R” script

Run all. This loads the Markov smoking model into the template, generates an Excel equivalent in “output/test_output_1_1.xlsm”, and estimates the average time spent in each state on each treatment.

Load the Excel sheet “output/test_output_1_1.xlsm” in Excel and navigate to the “PSA” tab. The exported input parameters are stored in the “input_parameters” tab and the reconstructed Markov trace in “markov_trace”

Click the “Run PSA” button and look at average times spent in each state that are saved in cells J3:M3.

Comparing these average times verifies that the R and Excel models are in agreement. 

