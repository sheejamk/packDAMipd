## parameter file data for testing
file <- "data-raw/blank.csv"

blank.df <- read.csv(file, stringsAsFactors = F)

# Create .rda object for trial_data in data folder
usethis::use_data(blank.df, overwrite = TRUE)
