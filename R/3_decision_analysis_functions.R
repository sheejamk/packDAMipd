# 5. Estimate ICER and NMB

#' Estimation of ICER and NMB
#' @param list_markov  list of markov model objects with their markov trace, cost matrix and utility matrix
#' @param threshold threshold value of WTP
#' @param comparator the strategy to be compared with
#' @param currency currency
#' @return ICER and NMB for all the stratgies compared to comparator
#' @examples
#' well <-  health_state("well", cost=0,utility=1)
#' disabled <- health_state("disabled", cost=100,utility=1)
#' dead <- health_state("dead", cost=0,utility=0)
#' tmat <- rbind(c(1, 2,3), c(NA, 4,5),c(NA,NA,6))
#' colnames(tmat) <- rownames(tmat) <- c("well","disabled" ,"dead")
#' tm <- transition_matrix(3, tmat, c(0.6,0.2,0.2,0.6,0.4,1), colnames(tmat) )
#' health_states <- combine_state(well,disabled,dead)
#' this.strategy <- strategy(tm, health_states, "control")
#' this_markov <-markov_model(this.strategy, 24, c(1000, 0,0),c(0,0,0),c(0,0,0))
#' well <-  health_state("well", cost=0,utility=1)
#' disabled <- health_state("disabled", cost=10,utility=0.5)
#' dead <- health_state("dead", cost=0,utility=0)
#' tmat <- rbind(c(1, 2,3), c(NA, 4,5),c(NA,NA,6))
#' colnames(tmat) <- rownames(tmat) <- c("well","disabled" ,"dead")
#' tm <- transition_matrix(3, tmat, c(0.4,0.4,0.2,0.6,0.4,1), colnames(tmat))
#' health_states <- combine_state(well,disabled,dead)
#' this.strategy <- strategy(tm, health_states, "intervention")
#' sec_markov <-markov_model(this.strategy, 24, c(1000, 0,0),c(0,0,0),c(0,0,0))
#' list_markov <- combine_markov(this_markov, sec_markov)
#' calculate_icer_nmb(list_markov,20000, comparator = "control")
#' @export
calculate_icer_nmb <- function(list_markov, threshold, comparator, currency = "GBP") {
  no_comparison <- nrow(list_markov)
  list_names_strategy <- list()
  for (i in seq_len(no_comparison)) {
    list_names_strategy <- append(list_names_strategy,list_markov[,"strategy"][[i]]$name_strategy)
  }
  if (!any(list_names_strategy == comparator)) {
    stop("Comparator has to be any of the strategies provided- please check")
  }else{
    comparator_index = which(list_names_strategy == comparator)
  }
  if (sum(colnames(list_markov) == c("strategy","transition_matrix", "param_matrix","health_states",
                                     "cycles",  "initial_state","initial_state_costs","initial_state_utilities","discount",
                                     "trace_matrix", "cost_matrix", "utility_matrix"))
                                                     != ncol(list_markov)) {
    stop("column names of list of markov_model objects look different")
  }
  if (check_list_markov_models(list_markov) != 0) {
    stop("Error in defining or creating markov model for compairing strategies")
  }
  cost_matrix_all <- list_markov[,"cost_matrix"]
  utility_matrix_all <- list_markov[,"utility_matrix"]
  cycles_all <- list_markov[,"cycles"]
  cycles <- unlist(cycles_all[1])
  av_cum_cost_pp <- list()
  av_cum_utility_pp <- list()
  for (i in seq_len(no_comparison)) {
      no_states <- list_markov[,"strategy"][[i]]$transition_matrix$no_states
      start_cohort <- sum(list_markov[,"trace_matrix"][[i]][1,])
      this_cost_matrix <- matrix(unlist(cost_matrix_all[i]), ncol = no_states + 2, byrow = FALSE)
      av_cum_cost <-  this_cost_matrix[cycles + 1,no_states + 2]
      av_cum_cost_pp <- append(av_cum_cost_pp,av_cum_cost / start_cohort)
      this_utility_matrix <-  matrix(unlist(utility_matrix_all[i]), ncol = no_states + 2, byrow = FALSE)
      av_cum_utility <- this_utility_matrix[cycles + 1,no_states + 2]
      av_cum_utility_pp <- append(av_cum_utility_pp,av_cum_utility / start_cohort)
  }
  list_icer <- list()
  list_nmb <- list()
  list_del_E = list()
  list_del_C = list()
  for (i in seq_len(no_comparison)) {
    if (i != comparator_index) {
      del_E <- (av_cum_utility_pp[[i]] - av_cum_utility_pp[[comparator_index]])
      del_c <- (av_cum_cost_pp[[i]] - av_cum_cost_pp[[comparator_index]])
      icer <- del_c / del_E
      nmb <- del_E  - del_c/threshold
    }else{
      del_E <- NA
      del_c <- NA
      icer <- NA
      nmb <- NA

    }
    list_icer <- append(list_icer, icer)
    list_nmb <- append(list_nmb, nmb)
    list_del_E <- append(list_del_E, del_E)
    list_del_C <- append(list_del_C, del_c)

  }
  results_cea <- matrix(c(unlist(list_names_strategy),unlist(av_cum_cost_pp),unlist(av_cum_utility_pp), unlist(list_del_C), unlist(list_del_E),
                         unlist(list_icer), unlist(list_nmb)), ncol = 7 , byrow = FALSE)

  colnames(results_cea) <- c("Strategy", "Cost", "Effect", "Inc_Cost", "Inc_Effect","ICER", "NMB")
  rownames(results_cea) <- NULL

  return(results_cea)
}
#######################################################################

#' check the list of markov models
#' @param list_markov  list of markov model objects with their markov trace, cost matrix and utility matrix
#' @return 0 if success else error
#' @examples
#' well <-  health_state("well", cost=0,utility=1)
#' disabled <- health_state("disabled", cost=100,utility=1)
#' dead <- health_state("dead", cost=0,utility=0)
#' tmat <- rbind(c(1, 2,3), c(NA, 4,5),c(NA,NA,6))
#' colnames(tmat) <- rownames(tmat) <- c("well","disabled" ,"dead")
#' tm <- transition_matrix(3, tmat, c(0.6,0.2,0.2,0.6,0.4,1), colnames(tmat) )
#' health_states <- combine_state(well,disabled,dead)
#' this.strategy <- strategy(tm, health_states, "example")
#' this_markov <-markov_model(this.strategy, 24, c(1000, 0,0),c(0,0,0),c(0,0,0))
#' well <-  health_state("well", cost=0,utility=1)
#' disabled <- health_state("disabled", cost=10,utility=0.5)
#' dead <- health_state("dead", cost=0,utility=0)
#' tmat <- rbind(c(1, 2,3), c(NA, 4,5),c(NA,NA,6))
#' colnames(tmat) <- rownames(tmat) <- c("well","disabled" ,"dead")
#' tm <- transition_matrix(3, tmat, c(0.4,0.4,0.2,0.6,0.4,1), colnames(tmat))
#' health_states <- combine_state(well,disabled,dead)
#' this.strategy <- strategy(tm, health_states, "example")
#' sec_markov <-markov_model(this.strategy, 24, c(1000, 0,0),c(0,0,0),c(0,0,0))
#' list_markov <- combine_markov(this_markov, sec_markov)
#' check_list_markov_models(list_markov)
#' @export
check_list_markov_models <- function(list_markov){
  no_comparison <- nrow(list_markov)
  no_states_all <- list()
  start_cohort_all <- list()
  cycles_all <- list_markov[,"cycles"]
  if (length(unique(cycles_all)) != 1) {
    stop("All strategies should have same number of cycles")
  }
  for (i in seq_len(no_comparison)) {
    no_states_all <- append(no_states_all,list_markov[,"strategy"][[1]]$transition_matrix$no_states)
    start_cohort_all <- append(start_cohort_all,sum(list_markov[,"trace_matrix"][[i]][1,]))
  }
  if (length(unique(no_states_all)) != 1) {
    stop("All strategies should have same number of states")
  }
  if (length(unique(start_cohort_all)) != 1) {
    stop("All strategies should have same number of total population at start")
  }
  return(0)
}
#######################################################################

#' Plot cost effectivess efficiency frontier
#' @param list_markov  markov_model objects
#' @param threshold_values  list of threshold values
#' @param comparator  the comparator
#' @param currency currency
#' @return plots
#' @examples
#' well <-  health_state("well", cost=0,utility=1)
#' disabled <- health_state("disabled", cost=100,utility=1)
#' dead <- health_state("dead", cost=0,utility=0)
#' tmat <- rbind(c(1, 2,3), c(NA, 4,5),c(NA,NA,6))
#' colnames(tmat) <- rownames(tmat) <- c("well","disabled" ,"dead")
#' tm <- transition_matrix(3, tmat, c(0.6,0.2,0.2,0.6,0.4,1), colnames(tmat) )
#' health_states <- combine_state(well,disabled,dead)
#' this.strategy <- strategy(tm, health_states, "control")
#' this_markov <-markov_model(this.strategy, 24, c(1000, 0,0),c(0,0,0),c(0,0,0))
#' well <-  health_state("well", cost=0,utility=1)
#' disabled <- health_state("disabled", cost=10,utility=0.5)
#' dead <- health_state("dead", cost=0,utility=0)
#' tmat <- rbind(c(1, 2,3), c(NA, 4,5),c(NA,NA,6))
#' colnames(tmat) <- rownames(tmat) <- c("well","disabled" ,"dead")
#' tm <- transition_matrix(3, tmat, c(0.4,0.4,0.2,0.6,0.4,1), colnames(tmat))
#' health_states <- combine_state(well,disabled,dead)
#' this.strategy <- strategy(tm, health_states, "intervention")
#' sec_markov <-markov_model(this.strategy, 24, c(1000, 0,0),c(0,0,0),c(0,0,0))
#' list_markov <- combine_markov(this_markov, sec_markov)
#' plot_ceac(list_markov,c(1000,2000,5000,7000,10000,15000,20000), comparator = "control")
#' @export
plot_ceac <- function(list_markov,threshold_values, comparator, currency ="GBP"){
  nmb_all = matrix(0, ncol = 2)
  colnames(nmb_all) <- c("NMB", "Threshold")
  for (i in seq_len(length(threshold_values))) {
    result <- calculate_icer_nmb(list_markov,threshold_values[i],comparator)
    result_nmb <- c(as.numeric(result[2,"NMB"]),threshold_values[i])
    nmb_all <- rbind(nmb_all,result_nmb)
  }
  nmb_all <- (nmb_all[-1,])
  rownames(nmb_all) <- NULL
  nmb_all <- data.frame(nmb_all)
  p <- ggplot2::ggplot(data = nmb_all, ggplot2::aes(x = nmb_all$Threshold, y = nmb_all$NMB, group = 1)) +
    ggplot2::geom_line(color = "red") +  ggplot2::geom_point() +
    ggplot2::labs(title = "Cost-effectiveness acceptability curve",
                  x = paste("Threshold values (",currency, ")", sep = " "), y = "NMB")
  return(p)
}
