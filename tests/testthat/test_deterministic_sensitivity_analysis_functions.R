###############################################################################
context("testing defining parameter for deterministic sensitivity analysis")
test_that("testing defining parameter for deterministic sensitivity analysis", {
  param_list <- define_parameters(
    cost_direct_med_B = 1774,
    cost_comm_care_C = 2059
  )
  low_values <- define_parameters(cost_direct_med_B = 177.4, cost_comm_care_C = 205.9)
  upp_values <- define_parameters(cost_direct_med_B = 17740, cost_comm_care_C = 20590)
  param_table <- define_parameters_sens_anal(param_list, low_values, upp_values)
  expect_equal(param_table$low_values$cost_direct_med_B, 177.4)
  expect_equal(param_table$upp_values$cost_comm_care_C, 20590)
  low_values <- define_parameters(cost_direct_med_B = 177.4)
  upp_values <- define_parameters(cost_direct_med_B = 17740)
  param_table <- define_parameters_sens_anal(param_list, low_values, upp_values)
  expect_equal(param_table$low_values$cost_direct_med_B, 177.4)
  expect_equal(param_table$upp_values$cost_direct_med_B, 17740)
  upp_values <- define_parameters(cost_direct_med_B = 17740, cost_comm_care_C = 20590)
  expect_error(define_parameters_sens_anal(param_list, low_values, upp_values))
})
###############################################################################
context("testing doing deterministic sensitivity analysis")
test_that("testing doing deterministic sensitivity analysis", {
  param_list <- define_parameters(
    cost_direct_med_A = 1701,
    cost_direct_med_B = 1774, tpAtoA = 0.2,
    tpAtoB = 0.5, tpAtoC = 0.3,
    tpBtoB = 0.3, tpBtoC = 0.7,
    tpCtoC = 1,
    cost_health_A = "cost_direct_med_A",
    cost_health_B = "cost_direct_med_B"
  )
  low_values <- define_parameters(cost_direct_med_B = 177.4)
  upp_values <- define_parameters(cost_direct_med_B = 17740)
  A <- health_state("A", cost = "cost_health_A ", utility = 1)
  B <- health_state("B", cost = "cost_health_B", utility = 1)
  C <- health_state("C", cost = 0, utility = 0)
  tmat <- rbind(c(1, 2, 3), c(NA, 4, 5), c(NA, NA, 6))
  colnames(tmat) <- rownames(tmat) <- c("A", "B", "C")
  tm <- populate_transition_matrix(3, tmat, c(
    "tpAtoA", "tpAtoB", "tpAtoC",
    "tpBtoB", "tpBtoC", "tpCtoC"
  ), colnames(tmat))
  health_states <- combine_state(A, B, C)
  mono_strategy <- strategy(tm, health_states, "mono")
  mono_markov <- markov_model(mono_strategy, 20, c(1, 0, 0), c(0, 0, 0), c(0, 0, 0),
    discount = c(0.06, 0), param_list
  )
  param_table <- define_parameters_sens_anal(param_list, low_values, upp_values)
  result <- do_sensitivity_analysis(mono_markov, param_table)
  this_names <- c("result_cost_direct_med_B", "low_result_cost_direct_med_B", "upp_result_cost_direct_med_B")
  expect_equal(names(result), this_names)
  expect_equal(result$result_cost_direct_med_B$strategy$name_strategy, "mono")
  expect_equal(result$low_result_cost_direct_med_B$strategy$name_strategy, "mono")
  expect_equal(result$upp_result_cost_direct_med_B$strategy$name_strategy, "mono")
  expect_error(do_sensitivity_analysis(mono_markov, define_parameters_sens_anal(param_list, low_values)))
  expect_error(do_sensitivity_analysis(mono_strategy, param_table))
})
