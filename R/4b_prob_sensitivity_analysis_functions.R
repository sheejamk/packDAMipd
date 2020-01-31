#' Define parameter lists for deterministic sensitivity analysis
#' @param base_param_list  list of parameters that used to define markov model
#' @param sample_list list of parameter values with their sampling distributions
#' @return table for probability sensitivity analysis
#' @examples
#' param_list <- define_parameters(cost_zido = 2278,cost_direct_med_A = 1701,
#' cost_comm_care_A = 1055,cost_direct_med_B = 1774,cost_comm_care_B = 1278,
#' cost_direct_med_C = 6948,cost_comm_care_C = 2059,tpAtoA = 1251/(1251+483),
#' tpAtoB = 350/(350+1384),tpAtoC = 116/(116+1618),tpAtoD = 17/(17+1717),
#' tpBtoB = 731/(731+527),tpBtoC = 512/(512+746),tpBtoD = 15/(15+1243),
#' tpCtoC = 1312/(1312+437), tpCtoD = 437/(437+1312),tpDtoD = 1,
#' cost_health_A = "cost_direct_med_A+ cost_comm_care_A",
#' cost_health_B = "cost_direct_med_B+ cost_comm_care_B",
#' cost_health_C = "cost_direct_med_C+ cost_comm_care_C",
#' cost_drug = "cost_zido")
#' sample_list <-define_parameters(cost_zido ="gamma(mean = 2756, sd = sqrt(2756))")
#' param_table <- define_parameters_psa(param_list, sample_list)
#' @export
define_parameters_psa  <-  function(base_param_list, sample_list){
  if (typeof(base_param_list) !=   "list" || typeof(sample_list) !=   "list") {
    stop("Error - Parameter list should be of type list")
  }
  sample_list_all  <-  base_param_list
  short1  <-  names(sample_list)
  long  <-  names(base_param_list)
  index1  <-  match(short1,long)
  len = length(index1)
  for(i in 1:len) {
    if (!is.na(index1[i])){
      this_name <- names(sample_list_all[index1[i]])
      sample_list_all[index1[i]] = sample_list[this_name]
    }
  }
  psa_table  <-  structure(list(base_param_list = base_param_list,
                                sample_list = sample_list_all))
  return(psa_table)
}
#######################################################################

#' Function to do deterministic sensitivity analysis
#' @param this_markov  markov model object
#' @param psa_table table object from define_parameters_psa()
#' @param num_rep number of repetitions
#' @return result after sensitivity analysis
#' @examples
#' param_list<-define_parameters(cost_zido = 2278,cost_direct_med_A = 1701,
#' cost_comm_care_A = 1055,cost_direct_med_B = 1774,cost_comm_care_B = 1278,
#' cost_direct_med_C = 6948,cost_comm_care_C = 2059,tpAtoA = 1251/(1251+483),
#' tpAtoB = 350/(350+1384),tpAtoC = 116/(116+1618),tpAtoD = 17/(17+1717),
#' tpBtoB = 731/(731+527),tpBtoC = 512/(512+746),tpBtoD = 15/(15+1243),
#' tpCtoC = 1312/(1312+437), tpCtoD = 437/(437+1312),tpDtoD = 1,
#' cost_health_A = "cost_direct_med_A+ cost_comm_care_A",
#' cost_health_B = "cost_direct_med_B+ cost_comm_care_B",
#' cost_health_C = "cost_direct_med_C+ cost_comm_care_C",
#' cost_drug = "cost_zido")
#' A <- health_state("A", cost="cost_health_A+ cost_drug ",utility=1)
#' B <- health_state("B", cost="cost_health_B + cost_drug",utility=1)
#' C <- health_state("C", cost="cost_health_C + cost_drug",utility=1)
#' D <- health_state("D", cost=0,utility=0)
#' tmat <- rbind(c(1, 2,3,4), c(NA, 5,6,7),c(NA, NA, 8,9), c(NA,NA,NA,10))
#' colnames(tmat) <- rownames(tmat) <- c("A","B" ,"C","D")
#' tm <- transition_matrix(4, tmat, c("tpAtoA","tpAtoB","tpAtoC","tpAtoD",
#' "tpBtoB", "tpBtoC", "tpBtoD","tpCtoC","tpCtoD","tpDtoD" ), colnames(tmat) )
#' health_states <- combine_state(A,B,C,D)
#' mono_strategy <- strategy(tm, health_states, "mono")
#' mono_markov <-markov_model(mono_strategy, 20, c(1, 0,0,0),c(0,0,0,0),c(0,0,0,0),
#' discount=c(0.06,0),param_list)
#' sample_list <-define_parameters(cost_zido ="gamma(mean = 2756, sd = sqrt(2756))")
#' param_table <- define_parameters_psa(param_list, sample_list)
#' result<-do_psa(mono_markov,param_table,10)
#' @export
do_psa <- function(this_markov,psa_table,num_rep){
  if (class(this_markov) !=   "markov_model")
    stop("Error - the model should be of class markov_model")
  check =  sum(names(psa_table) ==   c("base_param_list","sample_list"))
  if (check != 2) {
    stop("parameter table should have  2 entries  - default parameter values,
         and parameters with sampling distributions")
  }
  this_markov_param  <-  this_markov
  no_entries  <-  length(psa_table$base_param_list)
  result_all_params  <-  list()
  names_rep = list()
  this_markov_rep_all <- list()
  this_param_list <- NULL
  for (j in 1:num_rep) {
    for (i in 1:no_entries) {
      this_name =  names(psa_table$base_param_list[i])
      if (psa_table$base_param_list[i][[this_name]] !=  psa_table$sample_list[i][[this_name]]) {
          this_param_list  <-  psa_table$base_param_list
          the_expr  <-  psa_table$sample_list[i][[this_name]]
          the_real_expr  <-  check_estimate_substitute_proper_params(the_expr)
          this_param_list[i][[this_name]] =   eval(parse(text =  the_real_expr))
      }
    }
    if (is.null(this_param_list)) {
      this_param_list <- psa_table$base_param_list
    }
    this_markov_rep  <-  markov_model(this_markov$strategy, this_markov$cycles,
                                      this_markov$initial_state,
                                      this_markov$initial_state_costs,
                                      this_markov$initial_state_utilities,
                                      this_markov$discount,this_param_list)
    this_markov_rep_all  <-  append(this_markov_rep_all,list(this_markov_rep))
    names_rep =  append(names_rep, paste("rep_",j,sep =  ""))
  }
  names(this_markov_rep_all)  <-  unlist(names_rep)
  if (j ==  num_rep) {
      names_all  <-   c(names(this_markov_rep_all),"base_result")
      result_all_params  <-   append(this_markov_rep_all,list(this_markov_param))
      names(result_all_params)   <-   names_all
  }
  return(result_all_params)
}
#######################################################################
#' Function to do deterministic sensitivity analysis
#' @param result_psa_params_control  result from deterministic sensitivity analysis
#' for first or control model
#' @param result_psa_params_treat result from deterministic sensitivity analysis
#' for the comparative markov mdoel
#' @param threshold threshold value of WTP
#' @param comparator the strategy to be compared with
#' @return plot of  sensitivity analysis
#' @examples
#' param_list<-define_parameters(cost_zido = 2278,cost_direct_med_A = 1701,
#' cost_comm_care_A = 1055,cost_direct_med_B = 1774,cost_comm_care_B = 1278,
#' cost_direct_med_C = 6948,cost_comm_care_C = 2059,tpAtoA = 1251/(1251+483),
#' tpAtoB = 350/(350+1384),tpAtoC = 116/(116+1618),tpAtoD = 17/(17+1717),
#' tpBtoB = 731/(731+527),tpBtoC = 512/(512+746),tpBtoD = 15/(15+1243),
#' tpCtoC = 1312/(1312+437), tpCtoD = 437/(437+1312),tpDtoD = 1,
#' cost_health_A = "cost_direct_med_A+ cost_comm_care_A",
#' cost_health_B = "cost_direct_med_B+ cost_comm_care_B",
#' cost_health_C = "cost_direct_med_C+ cost_comm_care_C",
#' cost_drug = "cost_zido")
#' A <- health_state("A", cost="cost_health_A+ cost_drug ",utility=1)
#' B <- health_state("B", cost="cost_health_B + cost_drug",utility=1)
#' C <- health_state("C", cost="cost_health_C + cost_drug",utility=1)
#' D <- health_state("D", cost=0,utility=0)
#' tmat <- rbind(c(1, 2,3,4), c(NA, 5,6,7),c(NA, NA, 8,9), c(NA,NA,NA,10))
#' colnames(tmat) <- rownames(tmat) <- c("A","B" ,"C","D")
#' tm <- transition_matrix(4, tmat, c("tpAtoA","tpAtoB","tpAtoC","tpAtoD",
#' "tpBtoB", "tpBtoC", "tpBtoD","tpCtoC","tpCtoD","tpDtoD" ), colnames(tmat) )
#' health_states <- combine_state(A,B,C,D)
#' mono_strategy <- strategy(tm, health_states, "mono")
#' mono_markov <-markov_model(mono_strategy, 20, c(1, 0,0,0),c(0,0,0,0),c(0,0,0,0),
#' discount=c(0.06,0),param_list)
#' sample_list <-define_parameters(cost_zido ="gamma(mean = 2756, sd = sqrt(2756))")
#' param_table <- define_parameters_psa(param_list, sample_list)
#' result<-do_psa(mono_markov,param_table,10)
#' list_paramwise_psa_result(result, NULL,NULL,NULL)
#' @export
list_paramwise_psa_result  <-  function(result_psa_params_control,result_psa_params_treat,
                                        threshold,comparator){
  len =  length(result_psa_params_control) - 1
  results_all_param_mat  <-  data.frame()
  results_cost_util  <-  data.frame()
  results_icer_nmb  <-  data.frame()
  first_name <- names(result_psa_params_control)[1]
  no_params_parammatrix =  ncol(result_psa_params_control[[first_name]]$param_matrix)
  for (i in 1:len) {
    this_var_result_name <- names(result_psa_params_control)[i]
    if (!is.null(result_psa_params_treat)) {
      list_markov  <-  combine_markov(result_psa_params_control[[this_var_result_name]],
                                      result_psa_params_treat[[this_var_result_name]])
      res_icer_nmb  <-  calculate_icer_nmb(list_markov,threshold,comparator)
      res_icer_nmb_df <- data.frame(res_icer_nmb)
      comparator_row = which(res_icer_nmb_df$Strategy == comparator)

      results_icer_nmb  <-  rbind(results_icer_nmb,res_icer_nmb_df[-comparator_row,])
      colnames(results_icer_nmb) <- colnames(res_icer_nmb_df)
     }else{
       this_var_result <- result_psa_params_control[[this_var_result_name]]
       cost_matr <- this_var_result[["cost_matrix"]]
       util_matr <- this_var_result[["utility_matrix"]]
       this_cost <- cost_matr[nrow(cost_matr),ncol(cost_matr)]
       this_util <- util_matr[nrow(util_matr),ncol(util_matr)]
       results_cost_util  <-  rbind(results_cost_util,c(this_cost,this_util))
       results_this_rep <- list()
       for (j in 2:no_params_parammatrix) {
         param_matr <- this_var_result[["param_matrix"]]
         this_value <- param_matr[nrow(param_matr),j]
         results_this_rep  <-  append(results_this_rep, this_value)
       }
       results_all_param_mat <- rbind(results_all_param_mat, results_this_rep)
     }
  }
  if (!is.null(result_psa_params_treat)) {
    names(results_icer_nmb) <- colnames(res_icer_nmb)
    results_all <- as.data.frame(list(results_icer_nmb))
    results_all[["rep"]] <- seq(1:len)
  }
  else{
    names(results_cost_util) <- c("cost","utility")
    results_all <- as.data.frame(list( results_cost_util, results_all_param_mat))
    results_all[["rep"]] <- seq(1:len)
  }
  return(results_all)
}
#######################################################################
#' Function to do deterministic sensitivity analysis
#' @param result_psa_params_control  result from deterministic sensitivity analysis
#' for first or control model
#' @param result_psa_params_treat result from deterministic sensitivity analysis
#' for the comparative markov mdoel
#' @param threshold threshold value of WTP
#' @param comparator the strategy to be compared with
#' @return plot of  sensitivity analysis
#' @examples
#' param_list<-define_parameters(cost_zido = 2278,cost_direct_med_A = 1701,
#' cost_comm_care_A = 1055,cost_direct_med_B = 1774,cost_comm_care_B = 1278,
#' cost_direct_med_C = 6948,cost_comm_care_C = 2059,tpAtoA = 1251/(1251+483),
#' tpAtoB = 350/(350+1384),tpAtoC = 116/(116+1618),tpAtoD = 17/(17+1717),
#' tpBtoB = 731/(731+527),tpBtoC = 512/(512+746),tpBtoD = 15/(15+1243),
#' tpCtoC = 1312/(1312+437), tpCtoD = 437/(437+1312),tpDtoD = 1,
#' cost_health_A = "cost_direct_med_A+ cost_comm_care_A",
#' cost_health_B = "cost_direct_med_B+ cost_comm_care_B",
#' cost_health_C = "cost_direct_med_C+ cost_comm_care_C",
#' cost_drug = "cost_zido")
#' A <- health_state("A", cost="cost_health_A+ cost_drug ",utility=1)
#' B <- health_state("B", cost="cost_health_B + cost_drug",utility=1)
#' C <- health_state("C", cost="cost_health_C + cost_drug",utility=1)
#' D <- health_state("D", cost=0,utility=0)
#' tmat <- rbind(c(1, 2,3,4), c(NA, 5,6,7),c(NA, NA, 8,9), c(NA,NA,NA,10))
#' colnames(tmat) <- rownames(tmat) <- c("A","B" ,"C","D")
#' tm <- transition_matrix(4, tmat, c("tpAtoA","tpAtoB","tpAtoC","tpAtoD",
#' "tpBtoB", "tpBtoC", "tpBtoD","tpCtoC","tpCtoD","tpDtoD" ), colnames(tmat) )
#' health_states <- combine_state(A,B,C,D)
#' mono_strategy <- strategy(tm, health_states, "mono")
#' mono_markov <-markov_model(mono_strategy, 20, c(1, 0,0,0),c(0,0,0,0),c(0,0,0,0),
#' discount=c(0.06,0),param_list)
#' sample_list <-define_parameters(cost_zido ="gamma(mean = 2756, sd = sqrt(2756))")
#' param_table <- define_parameters_psa(param_list, sample_list)
#' result<-do_psa(mono_markov,param_table,10)
#' summary_plot_psa(result, NULL,NULL,NULL)
#' @export

summary_plot_psa <- function(result_psa_params_control, result_psa_params_treat =  NULL, threshold = NULL, comparator =  NULL ){
  if (!is.null(result_psa_params_treat)) {
    list_all <- list_paramwise_psa_result(result_psa_params_control, result_psa_params_treat,
                                          threshold,comparator)

    list_all_df <- data.frame(list_all, stringsAsFactors = FALSE)
    this_icer <- as.numeric(as.character(list_all_df$ICER))
    this_nmb <- as.numeric(as.character(list_all_df$NMB))
    mean_icer <- mean(this_icer)
    mean_nmb <- mean(this_nmb)
    sd_icer <- stats::sd(this_icer)
    sd_nmb <- stats::sd(this_nmb)
    plot1 <- ggplot2::ggplot(data = list_all, ggplot2::aes(x = this_icer,
                                                           y = this_nmb)) +
      ggplot2::geom_point(color = "blue", size =  3) +
      ggplot2::labs(x = "ICER") + ggplot2::labs(y = "NMB") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    plot2 <- ggplot2::ggplot(data = list_all, ggplot2::aes(x = this_icer)) +
      ggplot2::geom_histogram(bins = 20,color = "black", fill = "white") +
      ggplot2::labs(title = "Histogram of ICER", x = "ICER", y = "Count") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(this_icer)),
                          color = "blue", linetype = "dashed", size = 1) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    plot3 <- ggplot2::ggplot(data = list_all, ggplot2::aes(x = this_nmb)) +
      ggplot2::geom_histogram(bins = 20,color = "black", fill = "white") +
      ggplot2::labs(title = "Histogram of NMB",x = "NMB", y = "Count") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(this_nmb)),
                          color = "red", linetype = "dashed", size = 1) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    result  <-  structure(list(
      mean_icer = mean_icer,
      mean_nmb = mean_nmb,
      sd_icer  = sd_icer,
      sd_nmb  = sd_nmb,
      icer_nmb_plot = plot1,
      hist_icer = plot2,
      hist_nmb = plot3
    ))
  }else{
    list_all <- list_paramwise_psa_result(result_psa_params_control,
                                          result_psa_params_treat =  NULL, threshold = NULL, comparator =   NULL)
    no_entries = ncol(list_all) - 1
    mean_all  <- data.frame()
    sd_all  <-  data.frame()
    for (i in 1:no_entries) {
      mean_this_entry  <-  mean(list_all[,i])
      sd_this_entry  <-  stats::sd(list_all[,i])
      mean_all <-  append(mean_all,mean_this_entry)
      sd_all  <- append(sd_all,sd_this_entry)
    }
    names(mean_all) <- colnames(list_all[1:no_entries])
    names(sd_all) <- colnames(list_all[1:no_entries])
    plot1 <- ggplot2::ggplot(data = list_all, ggplot2::aes(x = list_all$utility,
                                                           y = list_all$cost)) + ggplot2::geom_point(color = "blue", size =  3) +
      ggplot2::labs(x = "Utility / Effect") + ggplot2::labs(y = "Cost") +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    plot2 <- ggplot2::ggplot(data = list_all, ggplot2::aes(x = list_all$cost)) +
      ggplot2::geom_histogram(bins = 20, color = "black", fill = "white") +
      ggplot2::labs(title = "Histogram of cost",x = "Cost", y = "Count") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(list_all$cost)),
                          color = "blue", linetype = "dashed", size = 1) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
    plot3 <- ggplot2::ggplot(data = list_all, ggplot2::aes(x = list_all$utility)) +
      ggplot2::geom_histogram(bins = 20,color = "black", fill = "white") +
      ggplot2::labs(title = "Histogram of utility values",x = "Utility", y = "Count") +
      ggplot2::geom_vline(ggplot2::aes(xintercept = mean(list_all$utility)),
                          color = "red", linetype = "dashed", size = 1) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

    result  <-  structure(list(
      mean_cost = mean_all$cost,
      mean_utility = mean_all$utility,
      sd_cost  =  sd_all$cost,
      sd_utility  = sd_all$utility,
      cost_utility_plot = plot1,
      hist_cost = plot2,
      hist_utility = plot3
    ))

  }
  return(result)
}

