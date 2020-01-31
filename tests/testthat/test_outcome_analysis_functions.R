###############################################################################
context("testing mean and sd of age from data")
test_that("testing mean and sd of age from data",  {
  this_data <- data.table::data.table("Age" = c(21, 15), "sex" = c("m", "f"))
  results <- get_mean_sd_age(this_data, NA)
  expect_equal(results$mean, 18.00)
  expect_equal(results$sd, 4.242641,tolerance = 1e-4)
  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  expect_error(get_mean_sd_age(this_data, NA))
})
###############################################################################
context("testing adding EQ5D5L values to the data")
test_that("testing adding EQ5D5L values to the data",  {
  trial_data <- data.frame("qol.MO" = c(1,2), "qol.SC" = c(1,5), "qol.UA" = c(1,2),  "qol.PD" = c(1,2), "qol.AD" = c(1,2))
  results <- value_eq5d5L_IPD(trial_data,NA)
  expect_equal(results$EQ5D5LIndex, c(1,0.548))
  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  expect_error(value_eq5d5L_IPD(this_data, NA))
  trial_data <- data.frame("qol.MO" = c(1,2), "qol.SC" = c(1,8), "qol.UA" = c(1,2),  "qol.PD" = c(1,2), "qol.AD" = c(1,2))
  expect_error(value_eq5d5L_IPD(trial_data,NA))
})
###############################################################################
context("testing mapping EQ5D5L values to 3L ")
test_that("testing adding EQ5D5L values to the data",  {
  trial_data <- data.frame("qol.MO" = c(1,2), "qol.SC" = c(1,2), "qol.UA" = c(1,2),  "qol.PD" = c(1,2), "qol.AD" = c(1,2))
  results <- map_eq5d5Lto3L_VanHout(trial_data,NA)
  expect_equal(results$EQ5D3L_from5L, c(1,0.5919117))
  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  expect_error(map_eq5d5Lto3L_VanHout(this_data, NA))
  trial_data <- data.frame("qol.MO" = c(1,2), "qol.SC" = c(1,8), "qol.UA" = c(1,2),  "qol.PD" = c(1,2), "qol.AD" = c(1,2))
  expect_error(map_eq5d5Lto3L_VanHout(trial_data,NA))
})
##############################################################################

context("testing conversion of ADL responses to scores ")
test_that("testing adding EQ5D5L values to the data",  {
  trial_data <- data.frame("tpi.q1" = c(1,2), "tpi.q2" = c(1,2), "tpi.q3" = c(1,2), "tpi.q4" = c(1,2),
                           "tpi.q5" = c(1,2), "tpi.q6" = c(1,2),"tpi.q7" = c(1,2),"tpi.q8" = c(1,2))

  results <- value_ADL_scores_IPD(trial_data,c("tpi"),adl_scoring,colnames(adl_scoring),NA)
  expect_equal(results$ADLTscore, c(40.7,55.8))
  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  expect_error(value_ADL_scores_IPD(this_data, NA))
  trial_data <- data.frame("qol.MO" = c(1,2), "qol.SC" = c(1,8), "qol.UA" = c(1,2),  "qol.PD" = c(1,2), "qol.AD"= c(1,2))
  expect_error(value_ADL_scores_IPD(trial_data,NA))
})
###############################################################################

context("testing conversion of ADL responses to scores ")
test_that("testing adding EQ5D5L values to the data",  {
  trial_data <- data.frame("shows.q1"=c(1,2), "shows.q2"=c(1,2), "shows.q3"= c(1,2),
                           "shows.q4"=c(1,2),"shows.q5"= c(1,2), "shows.q6"= c(1,2),
                           "shows.q7"= c(1,2),"shows.q8"= c(1,2),"shows.q9"= c(1,2),
                           "shows.q10"= c(1,2))
  results <- value_Shows_IPD(trial_data,c("shows"),NA)
  expect_equal(results$ShOWSscore, c(0,10))
  this_data <- data.table::data.table("Age" = c("k", 15), "sex" = c("m", "f"))
  expect_error(value_ADL_scores_IPD(this_data, NA))
  trial_data <- data.frame("qol.MO"=c(1,2), "qol.SC"=c(1,8), "qol.UA"= c(1,2),  "qol.PD"=c(1,2), "qol.AD"= c(1,2))
  expect_error(value_ADL_scores_IPD(trial_data,NA))
})
