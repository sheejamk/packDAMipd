## code to prepare `trial_data` dataset goes here
file.init <- "data-raw/trial_data.csv"

trial_data.df <- read.csv(file = file.init, stringsAsFactors = F)

# Create .rda object for trial_data in data folder
usethis::use_data(trial_data.df, overwrite = TRUE)
