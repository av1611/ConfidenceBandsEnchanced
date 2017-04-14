confint.csv.export <- function(final.info) {
  # Building a parameters info line, "_"-separated
  params.info <- paste("src", final.info[1, 4],
                       "rc",  final.info[1, 5],
                       "ss", final.info[1, 6],
                       "a", final.info[1, 7],
                       "sigma", final.info[1, 8],
                       "mu", final.info[1, 9], 
                       sep = "_")
  final.info.csv.full.name = paste0("confint.norm.mu_", params.info, ".csv")
  # Creating a sub-dir "csv_output" in the current dir
  dir.create(file.path("./", "csv_output"), showWarnings = FALSE)
    final.info.csv.path <- file.path("./", "csv_output", final.info.csv.full.name)
  write.csv(final.info, final.info.csv.path)
}

confint.csv.export(final.info)
