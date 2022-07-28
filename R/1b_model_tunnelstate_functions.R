#######################################################################
# D2.Markov model and trace
#' Definition of Markov model and trace
#' @param current_strategy  strategy object
#' @param nCycles no of cycles
#' @param initial_state value of states initially
#' @param discount rate of discount for costs and qalys
#' @param half_cycle_correction boolean to indicate half cycle correction
#' @param parameter_values parameters for assigning health states and
#' probabilities
#' @param state_cost_only_prevalent boolean parameter to indicate if the costs
#' for state occupancy is only for those in the state excluding those that
#' transitioned new. This is relevant when the transition cost is provided for
#' eg. in a state with dialysis the cost of previous dialysis is different from
#' the newly dialysis cases. Then the state_cost_only_prevalent should be TRUE
#' @param state_util_only_prevalent boolean parameter to indicate if the
#' utilities for state occupancy is only for those in the state excluding
#' those that transitioned new.
#' @param method what type of half cycle correction needed
#' @param startup_cost cost of states initially
#' @param startup_util utility of states initially if any
#' @return Markov trace
#' @examples
#' tmat <- rbind(c(1, 2), c(3, 4))
#' colnames(tmat) <- rownames(tmat) <- c("Healthy", "Dead")
#' tm <- populate_transition_matrix(2, tmat, c(0.5, 0.5, 0, 1))
#' a <- health_state("Healthy", 1, 1, 0, FALSE)
#' b <- health_state("Dead", 1, 0, 0, TRUE)
#' health_states <- combine_state(a, b)
#' this.strategy <- strategy(tm, health_states, "intervention")
#' markov_model(this.strategy, 10, c(1, 0))
#' @export
#' @details
#' Use the strategy, cycles, initial state values creating the markov model
#' and trace. As many probabilities /cost/utility value depend on age/time the
#' evaluation and assignment happens during each cycle. At the heart it does
#' a matrix multiplication using the previous row of the trace matrix and the
#' columns of the transition matrix. Also checks for population loss, calculates
#' cumulative costs and qalys (accounts for discounting
#'  and half cycle correction)
markov_model_sojourntime = function (current_strategy, nCycles, initial_state, discount = c(0,
                                                                                            0), parameter_values = NULL, half_cycle_correction = TRUE,
                                     state_cost_only_prevalent = FALSE, state_util_only_prevalent = FALSE,
                                     method = "half cycle correction", startup_cost = NULL, startup_util = NULL)
{
  changed_method <- checks_markov_pick_method(current_strategy,
                                              initial_state, discount, method, half_cycle_correction,
                                              startup_cost, startup_util, state_cost_only_prevalent,
                                              state_util_only_prevalent)
  health_states <- current_strategy$states
  no_states <- length(health_states)
  trans_mat <- current_strategy$transition_matrix
  trace_matrix <- init_trace_sjtime(health_states, nCycles +
                                      1)
  utility_matrix <- init_trace_sjtime(health_states, nCycles +
                                        1)
  cost_matrix <- init_trace_sjtime(health_states, nCycles +
                                     1)
  param_matrix <- array(0, c(nCycles + 1, length(parameter_values) +
                               2, nCycles))
  ending_cycles = nCycles + 1
  extended_parameter_values <- c(markov_cycle = 0, sojourn_time = 0,
                                 parameter_values)
  assigned_param <- assign_parameters(extended_parameter_values)
  health_states_assigned <- eval_assign_values_states(health_states,
                                                      assigned_param)
  trace_matrix[1, , 1] <- initial_state
  if (is.null(startup_cost))
    startup_cost = rep(0, no_states)
  if (is.null(startup_util))
    startup_util = rep(0, no_states)
  cost_matrix[1, , 1] <- trace_matrix[1, , 1] * (startup_cost +
                                                   get_current_cycle_cost(health_states_assigned))
  utility_matrix[1, , 1] <- trace_matrix[1, , 1] * (startup_util +
                                                      get_current_cycle_utility(health_states_assigned))
  for (cycle in 2:ending_cycles) {
    markov_cycle <- cycle - 1
    for (sojourn in 1:markov_cycle) {
      sojourn_time = sojourn
      extended_parameter_values <- c(markov_cycle = markov_cycle,
                                     sojourn_time = sojourn_time, parameter_values)
      assigned_param <- assign_parameters(extended_parameter_values)
      health_states_assigned <- eval_assign_values_states(health_states,
                                                          assigned_param)
      trans_mat_cycle_sojourn <- eval_assign_trans_prob(current_strategy$transition_matrix,
                                                        assigned_param)
      param_matrix[cycle, , sojourn] <- unlist(assigned_param)
      if (check_trans_prob(trans_mat_cycle_sojourn) !=
          0)
        stop("transition probabilites dont add to 1")
      conditional.trans <- trans_mat_cycle_sojourn$trans_matrix
      current.state <- trace_matrix[cycle - 1, , sojourn]
      same.state <- current.state * diag(conditional.trans)
      trace_matrix[cycle, , sojourn + 1] <- same.state
      cost_matrix[cycle, , sojourn + 1] <- same.state *
        get_current_cycle_cost(health_states_assigned)
      utility_matrix[cycle, , sojourn + 1] <- same.state *
        get_current_cycle_utility(health_states_assigned)
      diag(conditional.trans) <- 0
      new.state <- current.state %*% conditional.trans
      if (is.null(current_strategy$transition_cost)) {
        cost_occured_due_transitions <- 0
      }
      else {
        trans_cost <- eval_assign_trans_prob(current_strategy$transition_cost,
                                             assigned_param)
        cost_occured_due_transitions <- current.state %*%
          (conditional.trans * trans_cost$trans_matrix)
      }
      if (is.null(current_strategy$transition_utility)) {
        utility_due_transitions <- 0
      }
      else {
        trans_util <- eval_assign_trans_prob(current_strategy$transition_utility,
                                             assigned_param)
        utility_due_transitions <- current.state %*%
          (conditional.trans * trans_util$trans_matrix)
      }
      trace_matrix[cycle, , 1] <- new.state[] + trace_matrix[cycle,
                                                             , 1]
      if (state_cost_only_prevalent) {
        cost_matrix[cycle, , 1] <- cost_occured_due_transitions +
          cost_matrix[cycle, , 1]
      }
      else {
        cost_matrix[cycle, , 1] <- cost_occured_due_transitions +
          new.state[] * (get_current_cycle_cost(health_states_assigned)) +
          cost_matrix[cycle, , 1]
      }
      if (state_util_only_prevalent) {
        utility_matrix[cycle, , 1] <- utility_due_transitions +
          utility_matrix[cycle, , 1]
      }
      else {
        utility_matrix[cycle, , 1] <- utility_due_transitions +
          new.state[] * (get_current_cycle_utility(health_states_assigned)) +
          utility_matrix[cycle, , 1]
      }
    }
  }
  population <- apply(trace_matrix, c(1, 2), sum)
  costs <- apply(cost_matrix, c(1, 2), sum)
  utilities <- apply(utility_matrix, c(1, 2), sum)
  index_pop_loss <- which(rowSums(population) - sum(initial_state) >
                            5e-04)
  if (length(index_pop_loss) >= 1)
    stop(paste("Population loss - check at cycle ", index_pop_loss[1],
               sep = ""))
  nozeros <- rep(0, ending_cycles)
  costs <- cbind(costs, nozeros, nozeros)
  utilities <- cbind(utilities, nozeros, nozeros)
  population <- cbind(population, nozeros)
  for (i in 1:ending_cycles) {
    costs[i, no_states + 1] <- sum(costs[i, ])/((1 + discount[1])^(i -
                                                                     1))
    utilities[i, no_states + 1] <- sum(utilities[i, ])/((1 +
                                                           discount[2])^(i - 1))
  }
  if (changed_method == "hc_correction" & half_cycle_correction) {
    costs[1, no_states + 1] <- costs[1, no_states + 1] *
      0.5
    utilities[1, no_states + 1] <- utilities[1, no_states +
                                               1] * 0.5
    costs[nCycles + 1, no_states + 1] <- costs[nCycles +
                                                 1, no_states + 1] * 0.5
    utilities[nCycles + 1, no_states + 1] <- utilities[nCycles +
                                                         1, no_states + 1] * 0.5
  }
  costs[, no_states + 2] <- cumsum(costs[, no_states + 1])
  utilities[, no_states + 2] <- cumsum(utilities[, no_states +
                                                   1])
  names_states <- get_names_healthstates(health_states)
  names_trace_matrix <- names_cost_matrix <- names_utility_matrix <- names_states
  names_cost_matrix[no_states + 1] <- "Total discounted cost"
  names_cost_matrix[no_states + 2] <- "Cumulative cost"
  names_utility_matrix[no_states + 1] <- "Total discounted utility"
  names_utility_matrix[no_states + 2] <- "Cumulative utility"
  population[, no_states + 1] <- c(0, seq_len(nCycles))
  names_trace_matrix[no_states + 1] <- "Cycles"
  n3 <- dim(param_matrix)[3]
  params_matrix_all <- lapply(1:n3, function(i) param_matrix[,
                                                             , i]) %>% purrr::imap_dfr(function(mtx, i) {
                                                               as.data.frame(mtx) %>% dplyr::mutate(table = paste("sojourn",
                                                                                                                  i, sep = "_"))
                                                             })
  colnames(params_matrix_all) <- c("cycle", "sojourn times",
                                   names(parameter_values))
  colnames(costs) <- names_cost_matrix
  colnames(population) <- names_trace_matrix
  colnames(utilities) <- names_utility_matrix
  value <- list(strategy = current_strategy, method = method,
                half_cycle_correction = half_cycle_correction, transition_matrix = trans_mat,
                param_matrix = params_matrix_all, list_param_values = parameter_values,
                health_states = health_states, cycles = nCycles, initial_state = initial_state,
                discount = discount, trace_matrix = population, cost_matrix = costs,
                utility_matrix = utilities, startup_cost = startup_cost,
                startup_util = startup_util, state_cost_only_prevalent = state_cost_only_prevalent,
                state_util_only_prevalent = state_util_only_prevalent)
  attr(value, "class") <- "markov_model"
  value
}
##################################################################################3
get_names_healthstates <- function (health_state_assinged)
{
  nstates = length(health_state_assinged)
  names <- c()
  for (i in 1:nstates) {
    names <- c(names, health_state_assinged[[i]]$name)
  }
  return(names)
}
##################################################################################3
get_current_cycle_utility <- function (health_states_assigned)
{
  nstates = length(health_states_assigned)
  utility_cycle_states <- c()
  for (i in 1:nstates) {
    utility_cycle_states <- c(utility_cycle_states, health_states_assigned[[i]]$utility)
  }
  return(utility_cycle_states)
}
##################################################################################3
get_current_cycle_cost <- function (health_states_assigned)
{
  nstates = length(health_states_assigned)
  costs_cycle_states <- c()
  for (i in 1:nstates) {
    costs_cycle_states <- c(costs_cycle_states, health_states_assigned[[i]]$cost)
  }
  return(costs_cycle_states)
}

##################################################################################3
# D1b. All zero trace matrix
#' Define an all zero trace matrix
#' @param health_states health states
#' @param cycles no of cycles
#' @return trace matrix -all zero
#' @examples
#' a <- health_state("Healthy", 1, 1, 0, FALSE)
#' b <- health_state("Dead", 1, 0.5, 0, FALSE)
#' health_states <- combine_state(a, b)
#' init_trace(health_states, 10)
#' @export
#' @details
#' Initialise the trace matrix with all zeros
#' trace matrix will be with no_cycles+1 by no_states matrix
init_trace_sjtime= function (health_states, cycles)
{
  no_states <- length(health_states)
  trace = array(0, c(cycles, no_states, cycles))
  return(trace)
}

##################################################################################3
