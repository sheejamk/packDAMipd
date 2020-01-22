###############################################################################
context("testing add_probabilities")
test_that("testing add_probabilities",  {

  expect_equal(add_probabilities(c(0.2, 0.1)), 0.3)
  expect_error(add_probabilities(c(0.2, "g")))
  expect_error(add_probabilities(c(0.2, -0.1)))
  expect_error(add_probabilities(c(2, -0.1)))
  expect_error(add_probabilities(c(2, 0.1)))
})

test_that("testing row sum of probabiltiy vector",  {
  expect_equal(checksum_rowprob(c(0.7, 0.3)), 1)
  expect_error(checksum_rowprob(c(0.4, 0.3)))
})

test_that("testing probability distributions",  {
  expect_error(random_number_prob_distbn("unif", c(0.5)))
  expect_error(random_number_prob_distbn("unif", c(0.4, 0.1)))
  expect_error(random_number_prob_distbn("beta", c(0.4)))
  expect_error(random_number_prob_distbn("beta", c(0.4, "g")))
})
###############################################################################
context("testing checksum row probabilities")
test_that("testing checksum row probabilities",  {
  expect_equal(checksum_rowprob(c(0.1, 0.3, 0.4,0.2)),1)
  expect_equal(checksum_rowprob(c(0.2, 0.8)),1)
  expect_error(checksum_rowprob(c(0.2, 0.1)))
  expect_error(checksum_rowprob(c(0.2, "g")))
  expect_error(checksum_rowprob(c(0.2, -0.1)))
})
###############################################################################
context("testing random_number_prob_distbn")
test_that("testing random_number_prob_distbn",  {
  expect_error(random_number_prob_distbn("unif",c(1,0.2)))
  expect_error(random_number_prob_distbn("beta"))
  expect_error(random_number_prob_distbn("beta", c(1)))
})

###############################################################################

