## parameter file data for testing
file <- "data-raw/table_param.csv"

table_param.df <- read.csv(file, stringsAsFactors = F)

# Create .rda object for trial_data in data folder
usethis::use_data(table_param.df, overwrite = TRUE)
