## code to prepare `promis 3a scoring table` dataset goes here
this_file <- "data-raw/promis3a_scoring.csv"

promis3a_scoring.df = read.csv(file = this_file, stringsAsFactors = F)

# Create .rda object for trial_data in data folder
usethis::use_data(promis3a_scoring.df, overwrite = T)

