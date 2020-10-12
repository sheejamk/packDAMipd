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
  #Error - minimum and max value lists should have same entries
  expect_error(define_parameters_sens_anal(param_list, low_values, upp_values))
  # Error - null parameter
  expect_error(define_parameters_sens_anal(param_list, low_values, NULL))
  # Error -  parameter is not of type list
  expect_error(define_parameters_sens_anal(param_list, low_values, 12))

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
  mono_markov <- markov_model(mono_strategy, 20, discount = c(0.06, 0),
                              initial_state = c(1, 0, 0), param_list)
  param_table <- define_parameters_sens_anal(param_list, low_values, upp_values)
  result <- do_sensitivity_analysis(mono_markov, param_table)
  this_names <- c("result_cost_direct_med_B", "low_result_cost_direct_med_B", "upp_result_cost_direct_med_B")
  expect_equal(names(result), this_names)
  expect_equal(result$result_cost_direct_med_B$strategy$name_strategy, "mono")
  expect_equal(result$low_result_cost_direct_med_B$strategy$name_strategy, "mono")
  expect_equal(result$upp_result_cost_direct_med_B$strategy$name_strategy, "mono")
  #Error - type of third argument should be list
  expect_error(do_sensitivity_analysis(mono_markov, define_parameters_sens_anal(param_list, low_values, 1)))
  #Error - the first parameter should be of type markov_model
  expect_error(do_sensitivity_analysis(mono_strategy, param_table))

})
###############################################################################
context("testing reporting deterministic sensitivity analysis")
test_that("testing reporting deterministic sensitivity analysis", {
  param_list <- define_parameters(
    cost_direct_med_A = 1701,
    cost_direct_med_B = 1774, tpAtoA = 0.2,
    tpAtoB = 0.5, tpAtoC = 0.3,
    tpBtoB = 0.3, tpBtoC = 0.7,
    tpCtoC = 1,
    cost_health_A = "cost_direct_med_A",
    cost_health_B = "cost_direct_med_B"
  )
  low_values <- define_parameters(cost_direct_med_A = 170.1, cost_direct_med_B = 177.4)
  upp_values <- define_parameters(cost_direct_med_A = 17010, cost_direct_med_B = 17740)
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
  mono_markov <- markov_model(mono_strategy, 20, initial_state = c(1, 0, 0),
                              discount = c(0.06, 0), param_list)
  param_table <- define_parameters_sens_anal(param_list, low_values, upp_values)
  result <- do_sensitivity_analysis(mono_markov, param_table)
  reporting <- report_sensitivity_analysis(result)


  #Error - type of third argument should be list
  expect_error(do_sensitivity_analysis(mono_markov, define_parameters_sens_anal(param_list, low_values, 1)))
  #Error - the first parameter should be of type markov_model
  expect_error(do_sensitivity_analysis(mono_strategy, param_table))

})
###############################################################################
context("testing plotting deterministic sensitivity analysis")
test_that("testing plotting deterministic sensitivity analysis", {

param_list <- define_parameters(
  cost_zido = 2278, cost_direct_med_A = 1701,
  cost_comm_care_A = 1055, cost_direct_med_B = 1774, cost_comm_care_B = 1278,
  cost_direct_med_C = 6948, cost_comm_care_C = 2059, tpAtoA = 1251 / (1251 + 483),
  tpAtoB = 350 / (350 + 1384), tpAtoC = 116 / (116 + 1618), tpAtoD = 17 / (17 + 1717),
  tpBtoB = 731 / (731 + 527), tpBtoC = 512 / (512 + 746), tpBtoD = 15 / (15 + 1243),
  tpCtoC = 1312 / (1312 + 437), tpCtoD = 437 / (437 + 1312), tpDtoD = 1,
  cost_health_A = "cost_direct_med_A +  cost_comm_care_A",
  cost_health_B = "cost_direct_med_B +  cost_comm_care_B",
  cost_health_C = "cost_direct_med_C +  cost_comm_care_C",
  cost_drug = "cost_zido"
)
low_values <- define_parameters(cost_direct_med_B = 177.4, cost_comm_care_C = 205.9)
upp_values <- define_parameters(cost_direct_med_B = 17740, cost_comm_care_C = 20590)
A <- health_state("A", cost = "cost_health_A +  cost_drug ", utility = 1)
B <- health_state("B", cost = "cost_health_B + cost_drug", utility = 1)
C <- health_state("C", cost = "cost_health_C + cost_drug", utility = 1)
D <- health_state("D", cost = 0, utility = 0)
tmat <- rbind(c(1, 2, 3, 4), c(NA, 5, 6, 7), c(NA, NA, 8, 9), c(NA, NA, NA, 10))
colnames(tmat) <- rownames(tmat) <- c("A", "B", "C", "D")
tm <- populate_transition_matrix(4, tmat, c(
  "tpAtoA", "tpAtoB", "tpAtoC", "tpAtoD",
  "tpBtoB", "tpBtoC", "tpBtoD", "tpCtoC", "tpCtoD", "tpDtoD"
), colnames(tmat))
health_states <- combine_state(A, B, C, D)
mono_strategy <- strategy(tm, health_states, "mono")
mono_markov <- markov_model(mono_strategy, 20, c(1, 0, 0, 0), discount = c(0.06, 0), param_list)
param_table <- define_parameters_sens_anal(param_list, low_values, upp_values)
result <- do_sensitivity_analysis(mono_markov, param_table)


param_list_treat <- define_parameters(
  cost_zido = 3000, cost_direct_med_A = 890,
  cost_comm_care_A = 8976, cost_direct_med_B = 2345, cost_comm_care_B = 1278,
  cost_direct_med_C = 6948, cost_comm_care_C = 2059, tpAtoA = 1251 / (1251 + 483),
  tpAtoB = 350 / (350 + 1384), tpAtoC = 116 / (116 + 1618), tpAtoD = 17 / (17 + 1717),
  tpBtoB = 731 / (731 + 527), tpBtoC = 512 / (512 + 746), tpBtoD = 15 / (15 + 1243),
  tpCtoC = 1312 / (1312 + 437), tpCtoD = 437 / (437 + 1312), tpDtoD = 1,
  cost_health_A = "cost_direct_med_A +  cost_comm_care_A",
  cost_health_B = "cost_direct_med_B +  cost_comm_care_B",
  cost_health_C = "cost_direct_med_C +  cost_comm_care_C",
  cost_drug = "cost_zido"
)
treat_strategy <- strategy(tm, health_states, "treat")
treat_markov <- markov_model(treat_strategy, 20, c(1, 0, 0, 0), discount = c(0.06, 0), param_list_treat)
treat_low_values <- define_parameters(cost_direct_med_B = 234.5, cost_comm_care_C = 694.8)
treat_upp_values <- define_parameters(cost_direct_med_B = 23450, cost_comm_care_C = 69480)
param_table_treat <- define_parameters_sens_anal(param_list_treat, treat_low_values, treat_upp_values)
result_treat <- do_sensitivity_analysis(treat_markov, param_table)

# Error - plotting for NMB option and threshold should not be NULL
expect_error(plot_dsa(result, "NMB", type = "range", result_treat))

# Error - if plotting for NMB, threshold should not be of NULL
expect_error(plot_dsa(result, "NMB", type = "range", result_treat, NULL))

# Error - type can not be NULL
expect_error(plot_dsa(result, "NMB", type = NULL, result_treat))
# Error - type can not be other than range or difference
expect_error(plot_dsa(result, "NMB", type = "mean", result_treat))

# Error - if type is difference, result_dsa_treat should not be of NULL
expect_error(plot_dsa(result, "cost", type = "difference", NULL))

# Error - if plotting for ICER, type should be range
expect_error(plot_dsa(result, "ICER", type = "difference", result_treat,
                      20000, comparator = "treat"))

# Error - if plotting for ICER, comparator should not be null
expect_error(plot_dsa(result, "ICER", type = "range", result_treat,
                      20000))
# Error - if plotting for ICER, threshold should not be null
expect_error(plot_dsa(result, "ICER", type = "range", result_treat))

# Error - if plotting for ICER, result_dsa_treat should not be null
expect_error(plot_dsa(result, "ICER", type = "range"))

# Error - if plotting for NMB, type should be range
expect_error(plot_dsa(result, "NMB", type = "difference", result_treat,
                      20000, comparator = "treat"))

})
