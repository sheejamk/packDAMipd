#' parameter file data for testing
file <- "data-raw/table_param.csv"

table_param <- read.csv(file, stringsAsFactors = F)

usethis::use_data(table_param, overwrite = TRUE)
