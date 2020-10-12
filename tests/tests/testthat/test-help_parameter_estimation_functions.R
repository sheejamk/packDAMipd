###############################################################################
test_that("find the keyword for random number generation", {
  expect_equal(find_keyword_rand_generation("gamma"), "rgamma")
  expect_equal(find_keyword_rand_generation("expo"), "rexp")
  expect_equal(find_keyword_rand_generation("normal"), "rnorm")
  # error multi normal is not a keyword
  expect_error(find_keyword_rand_generation("multi normal"))
  # text can not be empty
  expect_error(find_keyword_rand_generation(" "))
  # text can not be null or NA
  expect_error(find_keyword_rand_generation(NA))
  expect_error(find_keyword_rand_generation(NULL))
})
###############################################################################
test_that("find the required parameters for given distribution", {
  expect_equal(find_required_parameter_combs("gamma"), c("shape", "rate"))
  expect_equal(find_required_parameter_combs("binomial"), c("size", "prob"))
  # error no suitable prob distribution found
  expect_error(find_required_parameter_combs("multi normal"))

  # name of the distribuion can not be null or NA
  expect_error(find_required_parameter_combs(NA))
  expect_error(find_required_parameter_combs(NULL))
})
###############################################################################
test_that("get the name and value of parameters in the prob distribution", {
  ans <- get_name_value_probdistrb_def("gamma(mean = 10, sd =1)")
  expect_equal(ans$params, c("mean", "sd"))
  expect_equal(ans$values, c(10, 1))
  ans <- get_name_value_probdistrb_def("gamma(mean = sqrt(2), b =17)")
  expect_equal(ans$params, c("mean", "b"))
  expect_equal(ans$values, c(sqrt(2), 17))
  # error as it is not separated by comma-
  expect_error(get_name_value_probdistrb_def("gamma(shape =1 & scale =1)"))
  # eror the expression has no commas
  expect_error(get_name_value_probdistrb_def("sheeja"))
  # error more than one equal sign
  expect_error(get_name_value_probdistrb_def("gamma(shape = 1 & scale =1, r=2)"))
  # expression can not be null or NA
  expect_error(get_name_value_probdistrb_def(NA))
  expect_error(get_name_value_probdistrb_def(NULL))
})

###############################################################################
test_that("get the name and value of parameters in the prob distribution", {
  ans <- check_estimate_required_params("gamma(mean = 10 ,sd=1)", "gamma")
  expect_equal(ans$shape, 100)
  expect_equal(ans$rate, 10)
  # Error - gamma distribution but parameters are not mean and sd
  expect_error(check_estimate_required_params("gamma(mean = sqrt(2), b = 17)", "gamma"))
  # Error - distribution can not be null or NA
  expect_error(check_estimate_required_params("gamma(mean = sqrt(2), sd = 17)", NA))
  # Error - expression can not be NA or NULL
  expect_error(check_estimate_required_params(NULL, "gamma"))
  expect_error(check_estimate_required_params(NA, "gamma"))
})
###############################################################################
test_that("get the name and value of parameters in the prob distribution", {
  ans <- check_estimate_substitute_proper_params("gamma(mean = 10 ,sd=1)")
  expect_equal(ans, "rgamma(1, shape = 100, rate = 10)")
  # Error - expression can not be NA or NULL
  expect_error(check_estimate_substitute_proper_params(NULL))
  expect_error(check_estimate_substitute_proper_params(NA))
  # Error - expression should have bracket
  expect_error(check_estimate_substitute_proper_params("sheeja"))
  # Error - gamma distribution but parameters are not mean and sd
  expect_error(check_estimate_substitute_proper_params("gamma(mean = sqrt(2), b = 17)"))
  # Error - gamma distribution but parameters are not mean and sd
  expect_error(check_estimate_substitute_proper_params("gamma(mean = sqrt(2), sd = e)"))
})
###############################################################################
test_that("get the extension of a filename", {
  expect_equal(get_extension_file("data.txt"), "txt")
  expect_equal(get_extension_file("data.jpg"), "jpg")
  # error no extension given after the "." sign
  expect_error(get_extension_file("data"))
})
###############################################################################
test_that("loading a datafile", {
  load_trial_data(system.file("extdata", "trial_data.csv", package = "packDAMipd"))
  #  file name given as NA (Null is the defualt and it will upload default trial data)
  expect_error(load_trial_data(NA))
  #  Error - file doesn't exists
  expect_error(load_trial_data("blank"))
})
###############################################################################
test_that("making a string of covariates", {
  expect_equal(make_string_covariates(c("open", "grade")), "open + grade")
})
