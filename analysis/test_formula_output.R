library(openxlsx)
# TODO could be changed, was to lazy late at night

output_wb <- loadWorkbook(
  file = "output/test_output_1_1.xlsm",
  isUnzipped = FALSE
)


if (!is.element("markov_trace", names(output_wb))) {
  addWorksheet(output_wb, "markov_trace", tabColour = "yellow")
}

n_states <- 2
n_treatments <- 2
n_cycles <- 10

#dummy <- rep("A1*B2", n_cycles)
dummy <- rep("BETAINV(RAND(), 25, 23)", n_cycles)
#dummy <- rep("RAND()", n_cycles)
#dummy <- rep("BETAINV(0.4, 25, 33)", n_cycles)
excel_markov_trace <- data.frame(i_cycle = c(1:n_cycles))
for (i_treatment in 1:n_treatments) {
  for (i_state in 1:n_states) {
    excel_markov_trace[[paste0(i_treatment, "_", i_state)]] <- dummy
    class(excel_markov_trace[[paste0(i_treatment, "_", i_state)]]) <-
      c(
        class(excel_markov_trace[[paste0(i_treatment, "_", i_state)]]),
        "formula"
      )
  }
}


writeData(
  output_wb,
  sheet = "markov_trace",
  x = excel_markov_trace,
  startCol = 5,
  startRow = 8
)


writeFormula(
  output_wb,
  sheet = "markov_trace",
  x = excel_markov_trace[[2]],
  startCol = 5,
  startRow = 8
)

saveWorkbook(output_wb, file = "output/test_output_1_1.xlsm", overwrite = TRUE)
