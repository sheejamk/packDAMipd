###############################################################################
context("test getting extension of a file")
test_that("test getting extension of a file",  {
  expect_equal(get_extension_file("black.txt"),"txt")
  expect_equal(get_extension_file("black.dta"),"dta")
  expect_equal(get_extension_file("black.csv"),"csv")
  expect_warning(get_extension_file("blackcsv"))
})
###############################################################################
context("testing add_probabilities")
test_that("testing add_probabilities",  {

  expect_equal(add_probabilities(c(0.2, 0.1)), 0.3)
  expect_error(add_probabilities(c(0.2, "g")))
  expect_error(add_probabilities(c(0.2, -0.1)))
  expect_error(add_probabilities(c(2, -0.1)))
  expect_error(add_probabilities(c(2, 0.1)))
})
###############################################################################

test_that("testing row sum of probabiltiy vector",  {
  expect_equal(checksum_rowprob(c(0.7, 0.3)), 1)
  expect_error(checksum_rowprob(c(0.4, 0.3)))
})
context("testing checksum row probabilities")
test_that("testing checksum row probabilities",  {
  expect_equal(checksum_rowprob(c(0.1, 0.3, 0.4, 0.2)),1)
  expect_equal(checksum_rowprob(c(0.2, 0.8)),1)
  expect_error(checksum_rowprob(c(0.2, 0.1)))
  expect_error(checksum_rowprob(c(0.2, "g")))
  expect_error(checksum_rowprob(c(0.2, -0.1)))
})
###############################################################################

test_that("testing probability distributions",  {
  expect_error(random_number_prob_distbn("unif", c(0.5)))
  expect_error(random_number_prob_distbn("unif", c(0.4, 0.1)))
  expect_error(random_number_prob_distbn("beta", c(0.4)))
  expect_error(random_number_prob_distbn("beta", c(0.4, "g")))
})
context("testing random_number_prob_distbn")
test_that("testing random_number_prob_distbn",  {
  expect_error(random_number_prob_distbn("unif",c(1, 0.2)))
  expect_error(random_number_prob_distbn("beta"))
  expect_error(random_number_prob_distbn("beta", c(1)))
})

###############################################################################
context("testing getting parameters between operators")
test_that("testing getting parameters between operators",  {
  expect_equal(find_parameters_btn_operators("a+b"),c("a","b"))
  expect_equal(find_parameters_btn_operators("cost_a + cost_b * rr - cost_c"),c("cost_a", "cost_b", "rr" , "cost_c"))
  expect_equal(find_parameters_btn_operators("cost_a + cost_b |  rr - cost_c"),c("cost_a", "cost_b", "rr" , "cost_c"))
  expect_equal(find_parameters_btn_operators("cost_a + cost_b ||  rr - cost_c"),c("cost_a", "cost_b", "rr" , "cost_c"))
})
###############################################################################
context("testing  getting parameters from a given expression separated by comma")
test_that("testing getting parameters between operators",  {
  expect_equal(find_param_from_def_prob_distrbn("gamma(mean = 10, sd =1)"),c("mean","sd"))
  expect_equal(find_param_from_def_prob_distrbn("uniform(min = 10, max =17)"),c("min","max"))
  expect_equal(find_param_from_def_prob_distrbn("uniform(a = 10, b =17, c = 5)"), c("a","b","c"))
  expect_equal(find_param_from_def_prob_distrbn("c(a = 10, b =17)"),c("a","b"))
  expect_equal(find_param_from_def_prob_distrbn("gamma(mean = sqrt(2), b =17)"),c("mean","b"))
})
###############################################################################
context("testing  getting required parameters to define probabilty distribution")
test_that("testing  getting required parameters to define probabilty distribution",  {
  expect_equal(find_required_parameter_combs("weibull"),c("shape","scale"))
  expect_equal(find_required_parameter_combs("normal"),c("mean","sd"))
  expect_equal(find_required_parameter_combs("gamma"),c("shape","rate"))
})
###############################################################################
context("testing  getting required parameters to define probabilty distribution")
test_that("testing  getting required parameters to define probabilty distribution",  {
  expect_equal(find_keyword_rand_generation("lognormal"),"rlnorm")
  expect_equal(find_keyword_rand_generation("normal"),"rnorm")
  expect_equal(find_keyword_rand_generation("gamma"),"rgamma")
})
###############################################################################
context("testing  getting required parameters to define probabilty distribution")
test_that("testing  getting required parameters to define probabilty distribution",  {
  expect_equal(check_estimating_required_params(c("mean","sd"),c("shape", "rate"),
                                                "gamma(mean = 10 ,sd=1)", "gamma"),c(100, 10), tolerance = 1e-4)
  expect_equal(check_estimating_required_params(c("mean", "sd"),find_required_parameter_combs("weibull"),
                                                "weibull(mean = 10 ,sd=1)", "weibull"),0)
  expect_equal(check_estimating_required_params(c("mean","sd"),c("shape", "rate"),
                                                "gamma(mean = sqrt(10) ,sd=1)", "gamma"),
               c(10.000000,3.162278), tolerance = 1e-4)
  expect_error(check_estimating_required_params(c("mean", "sd"),find_required_parameter_combs("weibull"),
                                                "weibull(mean = av(10) ,sd=1)", "weibull"))
})
###############################################################################
context("testing check, estimate and substitue proper params")
test_that("testing check, estimate and substitue proper params",  {
  expect_equal(check_estimate_substitute_proper_params("gamma(mean = 10 ,sd=1)"), "rgamma(1, shape = 100, rate = 10)")
  expect_error(check_estimate_substitute_proper_params("gamma(mean = ev(1) ,sd=1)"))
  expect_equal(check_estimate_substitute_proper_params("weibull(shape = 10, scale = 1)"), "rweibull(1, shape = 10, scale = 1)")

 })
###############################################################################
context("testing  getting keyword for regression methods")
test_that("testing  getting keyword for regression methods",  {
  expect_equal(find_keyword_regression_method("linear"),"lm")
  expect_equal(find_keyword_regression_method("logistic"),"glm")
  expect_equal(find_keyword_regression_method("survival","km"),"survival::survfit")
  expect_equal(find_keyword_regression_method("survival","parametric"),"flexsurv::flexsurvreg")
  expect_equal(find_keyword_regression_method("multilevel"),"lmer")
  expect_error(find_keyword_regression_method("logical"))

})
###############################################################################
context("testing  getting keyword for surevreg method")
test_that("testing  getting keyword for surevreg methods",  {
  expect_equal(find_survreg_distribution("expo"),"exponential")
  expect_equal(find_survreg_distribution("logistic"),"logistic")
  expect_error(find_survreg_distribution("logical"))
})
###############################################################################
context("testing  getting family of distribution for glm")
test_that("testing  getting family of distribution for glm",  {
  expect_equal(find_glm_distribution("gaussian"),"gaussian")
  expect_equal(find_glm_distribution("poisson"),"poisson")
  expect_error(find_glm_distribution("logical"))
})
###############################################################################
context("testing  getting link function for the family of distribution for glm")
test_that("testing  getting link function for the family of distribution for glm",  {
  expect_equal(check_link_glm("gaussian", "log"),"log")
  expect_equal(check_link_glm("poisson", "identity"),"identity")
  expect_error(check_link_glm("logical"))
  expect_error(check_link_glm("poisson", "probit"))
})
###############################################################################
