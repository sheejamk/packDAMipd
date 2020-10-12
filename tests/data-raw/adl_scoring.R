#' code to prepare `adl scoring table` data set goes here
adl_scoring <- "data-raw/Promis-8a-scoring-table.csv"
adl_scoring <- read.csv(file = adl_scoring, stringsAsFactors = F)

usethis::use_data(adl_scoring)
