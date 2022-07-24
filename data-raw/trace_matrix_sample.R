## cost data for testing
file <- "data-raw/trace_matrix_sample.csv"

trace_data.df <- read.csv(file, stringsAsFactors = F)

# Create .rda object for trial_data in data folder
usethis::use_data(trace_data.df, overwrite = TRUE)
