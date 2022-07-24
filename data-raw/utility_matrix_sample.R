## cost data for testing
file <- "data-raw/utility_matrix_sample.csv"

utility_data.df <- read.csv(file, stringsAsFactors = F)

# Create .rda object for trial_data in data folder
usethis::use_data(utility_data.df, overwrite = TRUE)
