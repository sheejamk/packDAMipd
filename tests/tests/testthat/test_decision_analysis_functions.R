###############################################################################
context("testing checking null or NA")
test_that("testing checking null or NA", {

  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 100, utility = 1)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.6, 0.2, 0.2, 0.6, 0.4, 1), colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this.strategy <- strategy(tm, health_states, "control")
  this_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), discount = c(0, 0))
  well <- health_state("well", cost = 0, utility = 1)
  disabled <- health_state("disabled", cost = 10, utility = 0.5)
  dead <- health_state("dead", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("well", "disabled", "dead")
  tm <- populate_transition_matrix(3, tmat, c(0.4, 0.4, 0.2, 0.6, 0.4, 1), colnames(tmat))
  health_states <- combine_state(well, disabled, dead)
  this.strategy <- strategy(tm, health_states, "intervention")
  sec_markov <- markov_model(this.strategy, 24, c(1000, 0, 0), discount = c(0, 0))
  list_markov <- combine_markov(this_markov, sec_markov)
  icer_nmb = calculate_icer_nmb(list_markov, 20000, comparator = "control")
  expect_equal(as.numeric(icer_nmb$ICER[2]), 86.67, tol = 1e-3)

  expect_equal(as.numeric(icer_nmb$ICER[2]), 86.67, tol = 1e-3)
  # Error comparator con is not valid
  expect_error(calculate_icer_nmb(list_markov, 20000, comparator = "con"))
  # Error threshold value  0 is not valid
  expect_error(calculate_icer_nmb(list_markov, 0, comparator = "control"))

  # Error list markov can not be null
  expect_error(calculate_icer_nmb(NULL, 200, comparator = "control"))
})
