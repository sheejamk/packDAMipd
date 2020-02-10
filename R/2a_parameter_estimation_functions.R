#######################################################################
#' Get the parameter values from reading a file
#' @param parameter  parameter of interest
#' @param param_value parameter value to be assigned
#' @return the paramvalue
#' @examples
#' a <- get_parameter_direct("cost_IT", param_value= 100)
#'@export
get_parameter_direct <- function(parameter, param_value){
  if (!is.null(param_value)) {
    assigned_value <- assign(parameter, param_value)
    return(assigned_value)
  }else{
    stop("Need to provide a parameter value")
  }
}
#######################################################################
#' Get the parameter values from reading a file
#' @param parameter  parameter of interest
#' @param paramfile parameter file to be provided
#' @param strategycol treatment strategy
#' @param strategyname treatment strategy name in the column strategycol
#' @return the paramvalue
#' @examples
#' a <- get_parameter_read("cost_IT", paramfile= system.file("extdata","table_param.csv",
#' package = "packDAMipd"))
#' @description  the file should have these column names (atleast)
#' Parameter,	Description,	Strategy,	Value
#' @export
get_parameter_read <- function(parameter, paramfile, strategycol= NA, strategyname = NA){
  if (is.null(paramfile))
    stop("Need to provide a parameter file to lookup")
  if (IPDFileCheck::test_file_exist_read(paramfile) != 0)
    stop("File doesnt exists or notable to access")
  dataset <- data.frame(read.csv(paramfile,header = TRUE, sep = ",", stringsAsFactors = FALSE))
  result <- IPDFileCheck::check_column_exists("value",dataset)
  if (result != 0)
    stop(paste("Parameter file should contain value in column name", sep = ""))
  if (!is.na(strategycol)) {
    if (IPDFileCheck::check_column_exists(strategycol,dataset) == 0) {
      dataset = dataset[dataset[[strategycol]] == strategyname,]
      answer <- dataset[dataset$Parameter == parameter,]$Value
    }else{
      stop(paste("Parameter file should contain strategy in column name", sep = ""))
    }
  }else{
    answer <- dataset[dataset$Parameter == parameter,]$Value
    if (is.na(answer))
      warning("Error- Parameter value is NA - Did you use the wrong function
              to get the parameter?")
  }
  return(answer)
}
#######################################################################
#' Get the definition of given parameter distribution defined in a file
#' @param parameter  parameter of interest
#' @param paramfile data file to be provided
#' @param strategycol treatment strategy column name
#' @param strategyname treatment strategy name in the column strategycol
#' @return the definition of parameter from the given distribution
#' @description  the file should have these column names (atleast)
#' Parameter,	Description,	Strategy,	Value,	Distribution,	Param1_name, Param1_value,
#' Param2_name,	Param2_value
#' @examples
#' a <- get_parameter_def_distribution("rr", paramfile = system.file("extdata", "table_param.csv",
#' package = "packDAMipd"))
#' @export
get_parameter_def_distribution <- function(parameter, paramfile, strategycol = NA,
                                           strategyname = NA){
  if (is.null(paramfile))
    stop("Need to provide a parameter file to lookup")
  dataset <- data.frame(read.csv(paramfile,header = TRUE, sep = ",", stringsAsFactors = FALSE))
  if (!is.na(strategycol)) {
    if (IPDFileCheck::check_column_exists(strategycol,dataset) != 0)
      dataset = dataset[dataset[[strategycol]] == strategyname,]
  }
  result <- IPDFileCheck::check_column_exists("Distribution",dataset)
  if (result != 0) {
    stop(paste("Parameter file should contain distribution in column name", sep = ""))
  }
  if (is.na(dataset[dataset$Parameter == parameter,]$Distribution))
    stop("This parameter may not be estimated from a distribution- use get_parameter_read
         instead")
  param_distribution = c("Param1_name","Param1_value")
  result <- unlist(lapply(param_distribution,IPDFileCheck::check_column_exists,dataset))
  if (sum(result) != 0)
    stop(paste("Parameter file should contain parameters for the distribtution in
               column name", sep = ""))
  this_param <- dataset[dataset$Parameter == parameter,]$Parameter
  this_distr_name <- dataset[dataset$Parameter == parameter,]$Distribution
  param1 <- dataset[dataset$Parameter == parameter,]$Param1_name
  param1_value <- dataset[dataset$Parameter == parameter,]$Param1_value
  if (!is.na(dataset[dataset$Parameter == parameter,]$Param2_name)) {
    param2 <- dataset[dataset$Parameter == parameter,]$Param2_name
    param2_value <- dataset[dataset$Parameter == parameter,]$Param2_value
    expression_created = paste(this_distr_name,"(", param1 ," = ", param1_value, "," ,
                               param2, " = ", param2_value,")", sep = "")
  }else{
    expression_created = paste(this_distr_name,"(", param1, " = " , param1_value,")", sep = "")
  }
  expression_recreated <- check_estimate_substitute_proper_params(expression_created)
  param_with_expression <- paste(this_param, " = ", expression_recreated, sep = "")
  return(param_with_expression)
}
#######################################################################

#' Get the parameter values using the provided statistical regression methods
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param method method of estimation (for example, linear, logistic regression etc)
#' @param indep_var the independent variable (column name in data file)
#' @param info_get_method additional information on methods e.g Kaplan-Meier ot hazard
#' @param info_distribution distribution name  eg. for logistic regression -binomial
#' @param covariates list of covariates - calculations to be done before passing
#' @param strategycol column name containing arm details
#' @param strategyname name of the  arm
#' @param timevar_survival time variable for survival analysis
#' @param interaction boolean value to indicate interaction in the case of linear regression
#' @param random_effect random effect variable(s) for the mixed effect models
#' @param naaction what action to be taken for the missing values, default is a missing value.
#' @param param2_to_be_estimated  parameter of interest for equation 2 in bivariate regression
#' @param covariates2 list of covariates - for equation 2 in bivariate regression
#' @param interaction2 boolean value to indicate interaction for equation 2 in bivariate regression
#' @param link link function to be provided if not using the default link for each of the info_distribtion
#' @return the results of the regression analysis
#' @examples
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' results_logit <- get_parameter_estimated_regression("admit", mydata,
#' "logistic regression", "gre", NA,"binomial", c("gpa", "factor(rank)"))
#' @export
get_parameter_estimated_regression <- function(param_to_be_estimated, dataset, method,
                                      indep_var, info_get_method, info_distribution,
                                      covariates= NA, strategycol= NA,
                                      strategyname= NA, timevar_survival= NA, interaction= NA,
                                      random_effect = NA, naaction = "stats::na.omit", param2_to_be_estimated= NA, covariates2 = NA, interaction2 = NA, link = NA){
  if (is.null(dataset))
    stop("Need to provide a data set to lookup")
  if (is.na(method))
    stop("Please provide  a statistical method")
  if (!is.na(strategycol)) {
    if (IPDFileCheck::check_column_exists(strategycol,dataset) != 0)
      dataset = dataset[dataset[[strategycol]] == strategyname,]
  }
  covariates_list = list()
  if (sum(is.na(covariates)) == 0) {
    for (i in 1:length(covariates)) {
      if (i == length(covariates))
        covariates_list = paste(covariates_list, paste(covariates[i], sep = ""))
      else
        covariates_list = paste(covariates_list, paste(covariates[i], " + ", sep = ""))
    }
  }else{
    covariates_list = NA
  }
  caps_method = toupper(method)
  if (caps_method == "SURVIVAL" | caps_method == "SURVIVAL ANALYSIS")
    results <- use_survival_analysis(param_to_be_estimated, dataset, indep_var, info_get_method,
                                     info_distribution, covariates_list , timevar_survival)
  if (caps_method == "LINEAR REGRESSION" | caps_method == "LINEAR_REGRESSION" | caps_method == "LINEAR")
    results <- use_linear_rgression(param_to_be_estimated, dataset, indep_var, covariates, interaction)
  if (caps_method == "LOGISTIC REGRESSION" | caps_method == "LOGISTIC_REGRESSION" | caps_method == "LOGISTIC")
    results <- use_logistic_rgression(param_to_be_estimated, dataset, indep_var, info_distribution, covariates_list, naaction, link)
  if (caps_method == "MULTILEVEL MODELLING" | caps_method == "MULTILEVEL_MODELLING" | caps_method == "MULTILEVEL"
     | caps_method == "MIXED EFFECT" | caps_method == "MIXED_EFFECT" )
    results <- use_mixed_effect_model(param_to_be_estimated, dataset, indep_var, covariates, random_effect)
  if (caps_method == "MULTILEVEL MODELLING" | caps_method == "MULTILEVEL_MODELLING" | caps_method == "MULTILEVEL"
      | caps_method == "MIXED EFFECT" | caps_method == "MIXED_EFFECT" )
    results <- use_mixed_effect_model(param_to_be_estimated, dataset, indep_var, covariates, random_effect)
  if (caps_method == "SEEMINGLY UNRELATED REGRESSION" | caps_method == "SEEMINGLY_UNRELATED_REGRESSION"
      | caps_method == "SEEMINGLY_UNRELATED" | caps_method == "SEEMINGLY UNRELATED"
      | caps_method == "BIVARIATE_REGRESSION" | caps_method == "BIVARIATE REGRESSION"
      | caps_method == "BIVARIATE" )
    results <- use_seemingly_unrelated_regression(param1_to_be_estimated  = param_to_be_estimated, param2_to_be_estimated,dataset,
                                                  indep_var, covariates1 = covariates, covariates2, interaction1 = interaction, interaction2)
  return(results)
}
#' ##########################################################################################################
#' Get the parameter values using the survival analysis
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param info_get_method additional information on methods e.g Kaplan-Meier ot hazard
#' @param info_distribution distribution name  eg. for logistic regression -binomial
#' @param covariates_list list of covariates - calculations to be done before passing
#' @param timevar_survival time variable for survival analysis, default is NA
#' @return the results of the regression analysis
#' @examples
#' data_for_survival <- survival::aml
#' surv_estimated_aml <- use_survival_analysis("status", data_for_survival,
#' "x", info_get_method="parametric", info_distribution = "weibull",
#' covariates_list= NA,"time")
#' @export
use_survival_analysis <- function(param_to_be_estimated, dataset,
                                 indep_var, info_get_method, info_distribution,
                                 covariates_list, timevar_survival) {
  if (is.na(info_get_method))
    stop("Please provide  information statistical method")
  caps_info_method <- toupper(info_get_method)
  if (caps_info_method == "PARAMETRIC SURVIVAL" | caps_info_method == "PARAMETRIC")
    results <- use_parametric_survival(param_to_be_estimated, dataset, indep_var,
                                         info_distribution, covariates_list,timevar_survival)
  if (caps_info_method == "KAPLAN-MEIER" | caps_info_method == "KM")
    results <- use_km_survival(param_to_be_estimated, dataset, indep_var, covariates_list,
                               timevar_survival)
  if (caps_info_method == "FLEMING-HARRINGTON" | caps_info_method == "FH")
    results <- use_fh_survival(param_to_be_estimated, dataset, indep_var, covariates_list,
                               timevar_survival)
  if (caps_info_method == "FH2")
    results <- use_fh2_survival(param_to_be_estimated, dataset, indep_var, covariates_list,
                                timevar_survival)
  if (caps_info_method %in% c("COX-PROPORTIONAL-HAZARD","COX PROPORTIONAL HAZARD","COX-PH",
                              "COX PH","COXPH"))
    results <- use_coxph_survival(param_to_be_estimated, dataset, indep_var, covariates_list,
                                  timevar_survival)
  return(results)

}
#' ##########################################################################################################
#' Get the parameter values using the survival analysis parametric survival
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param info_distribution distribution name  eg. for logistic regression -binomial
#' @param covariates_list list of covariates - calculations to be done before passing
#' @param timevar_survival time variable for survival analysis, default is NA
#' @return the results of the regression analysis
#' @examples
#' data_for_survival <- survival::aml
#' surv_estimated_aml <- use_parametric_survival("status", data_for_survival,"x",
#' info_distribution="weibull", covariates_list= NA,"time")
#' @export
use_parametric_survival <- function(param_to_be_estimated, dataset,
                                      indep_var, info_distribution, covariates_list,
                                      timevar_survival){
  if (is.na(timevar_survival))
    stop("For survival analysis, please provide the varaible to use as time ")
  else
    surv_object <- paste("survival::Surv(", timevar_survival, ",", param_to_be_estimated, ")", sep = "")
  if (is.na(info_distribution))
    stop("Error - information on distribution is missing")
  else
    this_dist <- find_survreg_distribution(info_distribution)
  if (is.na(covariates_list)){
      expression_recreated <- paste0("survival::survreg","(", surv_object, " ~ ", indep_var, ", ",
                                   "data = dataset,  dist = \"", this_dist, "\" ) ", sep = "")
      expression_recreated_forsurfit <- paste0("survival::survfit","(", surv_object, " ~ ", indep_var, ", ",
                                     "data = dataset)", sep = "")
  }else{
      expression_recreated <- paste0("survival::survreg","(", surv_object, " ~ ", covariates_list, " + ",
                                   indep_var, ", ","data = dataset,  dist = \"",
                                   this_dist, "\" ) ", sep = "")
      expression_recreated_forsurfit <- paste0("survival::survfit","(", surv_object, " ~ ", covariates_list, " + ",
                                     indep_var, ", ","data = dataset)", sep = "")
  }
  param_estimated <- eval(parse(text = expression_recreated))
  param_estimated_survift <- eval(parse(text = expression_recreated_forsurfit))
  summary_regression_results = summary(param_estimated)
  if (toupper(this_dist) == "WEIBULL")
    distribution_parameters <- SurvRegCensCov::ConvertWeibull(param_estimated, conf.level = 0.95)
  vcov_param_estimated <- stats::vcov(param_estimated)
  chol_decomp_matrix <- chol(vcov_param_estimated)
  values = levels(dataset[[indep_var]])
  name = indep_var
  newdata1 = list(name = values[1])
  names(newdata1) <-  name
  newdata2 = list(name = values[2])
  names(newdata2) <-  name
  plot_result <- plot(param_estimated_survift, xlab = indep_var, ylab = "Survival probability", cex.lab = 1.2)
  lines(predict(param_estimated, newdata = newdata1, type = "quantile",p = seq(.01,.99,by=.01)),
        seq(.99,.01, by = -.01),col = "red")
  lines(predict(param_estimated, newdata = newdata2, type = "quantile",p = seq(.01,.99,by=.01)),
        seq(.99,.01, by = -.01),col = "blue")
  legend("topright", inset = c(-0.4,0), legend = c(values[1], values[2]), col = c("red", "blue"),
         seg.len = 0.6, y.intersp = 0.1, lty = 1, bty = 'n', cex= 1)
   results =  structure(list(
    param_estimated = param_estimated,
    summary_regression_results = summary_regression_results,
    variance_covariance = vcov_param_estimated,
    cholesky_decomp_matrix = chol_decomp_matrix,
    distribution_parameters = distribution_parameters,
    plot = plot_result
  ))
  return(results)
}
#' ##########################################################################################################
#' Get the parameter values using the Kaplan-Meier survival analysis
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates_list list of covariates - calculations to be done before passing
#' @param timevar_survival time variable for survival analysis, default is NA
#' @return the results of the regression analysis
#' @examples
#' data_for_survival <- survival::aml
#' surv_estimated_aml <-use_km_survival("status", data_for_survival, "x", covariates_list= NA, "time")
#' @export
use_km_survival <- function(param_to_be_estimated, dataset,
                            indep_var, covariates_list, timevar_survival){
  if (is.na(timevar_survival))
    stop("For survival analysis, please provide the varaible to use as time ")
  else
    surv_object <- paste("survival::Surv(", timevar_survival, ",", param_to_be_estimated, ")",
                         sep = "")
  if (is.na(covariates_list))
    expression_recreated <- paste0("survival::survfit","(", surv_object, " ~ ", indep_var,",type =",
                                   "\"kaplan-meier\"", ", ","data = dataset) ", sep = "")
  else
    expression_recreated <- paste0("survival::survfit","(", surv_object, " ~ ", covariates_list, " + ",
                                   indep_var,", type =", "\"kaplan-meier\"",", "
                                   ,"data = dataset) ", sep = "")
  param_estimated <- eval(parse(text = expression_recreated))
  summary_regression_results = summary(param_estimated)
  fit <- param_estimated
  fit$call$formula <- param_estimated$call$formula
  fit$call$data = param_estimated$call$data
  the_data = eval(fit$call$data)
  plot_result <- survminer::ggsurvplot(fit,data = the_data)
  results =  structure(list(
    param_estimated = param_estimated,
    summary_regression_results = summary_regression_results,
    plot = plot_result
  ))
  return(results)
}
#' ##########################################################################################################
#' Get the parameter values using the survival analysis method FH
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates_list list of covariates - calculations to be done before passing
#' @param timevar_survival time variable for survival analysis, default is NA
#' @return the results of the regression analysis
#' @examples
#' data_for_survival <- survival::aml
#' surv_estimated_aml <-use_fh_survival("status", data_for_survival, "x", covariates_list= NA, "time")
#' @export
use_fh_survival <- function(param_to_be_estimated, dataset,
                            indep_var, covariates_list, timevar_survival){
  if (is.na(timevar_survival))
    stop("For survival analysis, please provide the varaible to use as time ")
  else
    surv_object <- paste("survival::Surv(", timevar_survival, ",", param_to_be_estimated, ")",
                         sep = "")
  if (is.na(covariates_list))
    expression_recreated <- paste0("survival::survfit","(", surv_object, " ~ ", indep_var,",
                                   type =","\"fleming-harrington\"",", ","data = dataset) ",
                                   sep = "")
  else
    expression_recreated <- paste0("survival::survfit","(", surv_object, " ~ ", covariates_list, " + ",
                                   indep_var,", type =", "\"fleming-harrington\"",", ",
                                   "data = dataset) ", sep = "")
  param_estimated <- eval(parse(text = expression_recreated))
  summary_regression_results = summary(param_estimated)
  fit <- param_estimated
  fit$call$formula <- param_estimated$call$formula
  fit$call$data = param_estimated$call$data
  the_data = eval(fit$call$data)
  plot_result <- survminer::ggsurvplot(fit,data = the_data)
  results =  structure(list(
    param_estimated = param_estimated,
    summary_regression_results = summary_regression_results,
    plot = plot_result
  ))
  return(results)
}
#' ##########################################################################################################
#' Get the parameter values using the survival analysis using FH2 method
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates_list list of covariates - calculations to be done before passing
#' @param timevar_survival time variable for survival analysis, default is NA
#' false by default
#' @return the results of the regression analysis
#' @examples
#' data_for_survival <- survival::aml
#' surv_estimated_aml <-use_fh2_survival("status", data_for_survival, "x", covariates_list= NA, "time")
#' @export
use_fh2_survival <- function(param_to_be_estimated, dataset,
                             indep_var, covariates_list, timevar_survival){
  if (is.na(timevar_survival))
    stop("For survival analysis, please provide the varaible to use as time ")
  else
    surv_object <- paste("survival::Surv(", timevar_survival, ",", param_to_be_estimated, ")",
                         sep = "")
  if (is.na(covariates_list))
    expression_recreated <- paste0("survival::survfit","(", surv_object, " ~ ", indep_var,
                                   ", type =", "\"fh2\"",
                                   ", ","data = dataset) ", sep = "")
  else
    expression_recreated <- paste0("survival::survfit","(", surv_object, " ~ ", covariates_list,
                                   " + ", indep_var,", type =", "\"fh2\"",", ","data = dataset) ", sep = "")
  param_estimated <- eval(parse(text = expression_recreated))
  summary_regression_results = summary(param_estimated)
  fit <- param_estimated
  fit$call$formula <- param_estimated$call$formula
  fit$call$data = param_estimated$call$data
  the_data = eval(fit$call$data)
  plot_result <- survminer::ggsurvplot(fit,data = the_data)
  results =  structure(list(
    param_estimated = param_estimated,
    summary_regression_results = summary_regression_results,
    plot = plot_result
  ))
  return(results)
}
#' ##########################################################################################################
#' Get the parameter values using the survival analysis using cox proportional hazard
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates_list list of covariates - calculations to be done before passing
#' @param timevar_survival time variable for survival analysis, default is NA
#' false by default
#' @return the results of the regression analysis
#' @examples
#' data_for_survival <- survival::aml
#' surv_estimated_aml <- use_coxph_survival("status", data_for_survival, "x",
#' covariates_list = NA, "time")
#' @export
use_coxph_survival <- function(param_to_be_estimated, dataset, indep_var,
                               covariates_list, timevar_survival){
  if (is.na(timevar_survival))
    stop("For survival analysis, please provide the varaible to use as time ")
  else
    surv_object <- paste("survival::Surv(", timevar_survival, ",", param_to_be_estimated, ")",
                         sep = "")
  if (is.na(covariates_list))
    expression_recreated <- paste0("survival::coxph","(", surv_object, " ~ ", indep_var, ", ",
                                   "data = dataset) ", sep = "")
  else
    expression_recreated <- paste0("survival::coxph","(", surv_object, " ~ ", covariates_list, " + ",
                                   indep_var,", ", "data = dataset) ", sep = "")
  param_estimated <- eval(parse(text = expression_recreated))
  summary_regression_results = summary(param_estimated)
  test.ph <- cox.zph(param_estimated)
  ggcoxzph(test.ph)
  fit <- survival::survfit(param_estimated, data = "param_estimated$call$data")
  fit$call$formula <- param_estimated$call$formula
  fit$call$data = param_estimated$call$data
  the_data = eval(fit$call$data)
  plot_result <- survminer::ggsurvplot(fit,data = the_data)
  results =  structure(list(
    param_estimated = param_estimated,
    summary_regression_results = summary_regression_results,
    plot = plot_result
  ))
  return(results)
}
#' ##########################################################################################################
#' Get the parameter values using logistic regression
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param family distribution name  eg. for logistic regression -binomial
#' @param covariates_list list of covariates - calculations to be done before passing
#' @param naaction action to be taken with the missing values
#' @param link link function if not the default for each family
#' @return the results of the regression analysis
#' @examples
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' mydata$rank <- factor(mydata$rank)
#' results_logit <- use_logistic_rgression("admit", dataset=mydata,
#' indep_var = "gre", family ="binomial", covariates_list = NA, naaction = "stats::na.omit", link = NA)
#' @export
use_logistic_rgression <- function(param_to_be_estimated, dataset, indep_var,
                                   family, covariates_list, naaction, link){
  if (is.na(family))
    stop("Error - information on distribution is missing")
  else
    this_dist <- find_glm_distribution(family)
  if (!is.na(link)) {
    link = check_link_glm(this_dist, link)
    family_def = paste(this_dist,"(link = ", link, ")", sep = "")
  }else{
    family_def = paste(this_dist,"( )", sep = "")
  }
  if (is.na(covariates_list))
    expression_recreated <- paste0("glm","(", param_to_be_estimated, " ~ ", indep_var,
                                   ", family = ", family_def, ", data = dataset, na.action =", naaction, ") ", sep = "")
  else
    expression_recreated <- paste0("glm","(", param_to_be_estimated, " ~ ", covariates_list, " + ",
                                   indep_var, ", family = ", family_def, ", data = dataset, na.action = ", naaction, ") ",
                                   sep = "")

  param_estimated <- eval(parse(text = expression_recreated))
  summary_regression_results = summary(param_estimated)
  vcov_param_estimated <- stats::vcov(param_estimated)
  chol_decomp_matrix <- chol(vcov_param_estimated)
  plot_result <- ggplot2::autoplot(param_estimated)
  table_this <- broom::tidy(param_estimated)
  or <- exp(table_this$estimate)
  lci <- exp(table_this$estimate - stats::qnorm(0.975,  0,  1) * table_this$std.error)
  uci <- exp(table_this$estimate + stats::qnorm(0.975,  0,  1) * table_this$std.error)
  or_from_glm <- data.frame(cbind(term = table_this$term,lci, or, uci))
  name_file_plot <- paste0("glm_", param_estimated, "_", indep_var, ".pdf", sep = "")
  plot_result <- ggplot2::autoplot(param_estimated, toPdf = TRUE, file = name_file_plot)
  table_this <- broom::tidy(param_estimated)
  OR <- exp(table_this$estimate)
  LCI <- exp(table_this$estimate - stats::qnorm(0.975,  0,  1) * table_this$std.error)
  UCI <- exp(table_this$estimate + stats::qnorm(0.975,  0,  1) * table_this$std.error)
  or_from_glm <- data.frame(cbind(term = table_this$term, LCI, OR, UCI))
  results =  structure(list(
    param_estimated = param_estimated,
    variance_covariance = vcov_param_estimated,
    summary_regression_results = summary_regression_results,
    cholesky_decomp_matrix = chol_decomp_matrix,
    odds_ratio_ci = or_from_glm,
    plot = plot_result
  ))
  return(results)
}
#' ##########################################################################################################
#' Get the parameter values using the linear regression
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates - calculations to be done before passing
#' @param interaction boolean value to indicate interaction in the case of linear regression,
#' false by default
#' @return the results of the regression analysis
#' @examples
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' results_lm <- use_linear_rgression("gre", dataset = mydata,
#' indep_var = "gpa", covariates = NA, interaction = FALSE)
#' @export
use_linear_rgression <- function(param_to_be_estimated, dataset, indep_var, covariates, interaction){
  formula <- form_expression_lm(param_to_be_estimated, indep_var, covariates, interaction)
  fit <- stats::lm(formula,data = dataset)
  summary_regression_results = summary(fit)
  ci_estimates <- stats::confint(fit)
  vcov_param_estimated <- stats::vcov(fit)
  chol_decomp_matrix <- chol(vcov_param_estimated)
  name_file_plot <- paste0("lm_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  plot_result <- ggplot2::autoplot(fit, toPdf = TRUE, file = name_file_plot)
  test_asumptions <- summary(gvlma::gvlma(fit))
  corr_test <- lmtest::dwtest(fit)
  Adj_R2 <- summary(fit)$adj.r.squared
  AIC <- stats::AIC(fit)
  BIC <- stats::BIC(fit)
  check_model_fit <- data.frame(cbind(Adj_R2, AIC , BIC))
  colnames(check_model_fit) <- c("Adj_R2", "AIC", "BIC")
  results =  structure(list(
    param_estimated = fit,
    test_asumptions = test_asumptions,
    corre_test = corr_test,
    check_model_fit  = check_model_fit,
    ci_estimates = ci_estimates,
    variance_covariance = vcov_param_estimated,
    summary_regression_results = summary_regression_results,
    cholesky_decomp_matrix = chol_decomp_matrix,
    plot = plot_result
  ))
  return(results)
}
##########################################################################################################
#' Function for mixed effect regression
#' @param param_to_be_estimated column name of dependent variable
#' @param dataset a dataframe
#' @param indep_var column name of independent variable
#' @param covariates, independent variables, NA by default
#' @return result regression result with plot if success and -1, if failure
#' @param random_effect random effect variable(s) for the mixed effect models
#' @examples
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#  results_logit <- use_mixed_effect_model("gre", dataset=mydata,
#                                          indep_var = "gpa", covariates = NA)
#' @export
use_mixed_effect_model <- function(param_to_be_estimated, dataset, indep_var, covariates, random_effect){
    random = list()
    expre = list()
    if (sum(is.na(covariates)) == 0) {
      expre = paste(covariates[1], sep = "")
      i = 2
      while (i <= length(covariates)) {
        this = paste(covariates[i], sep = "")
        expre = paste(expre, this, sep = "+")
        i = i + 1
      }
    }
    if (sum(is.na(random_effect)) == 0) {
       random = paste("1|",random_effect[1], sep = "")
       j = 2
       while (i <= length(random_effect)) {
         this = paste("1|",random_effect[j], sep = "")
         random = paste(random, this, sep = "+")
         j = j + 1
       }
    }
    if (length(random) != 0 & length(expre) != 0) {
      expression_recreated =  paste0("lme4::lmer(", param_to_be_estimated, " ~ ", expre, " + ", indep_var, ", random = ~", random, ", data = dataset)")
      fit <- eval(parse(text = expression_recreated))

    }else{
      if (length(random) != 0 & length(expre) == 0) {
        expression_recreated =  paste0("lme4::lmer(", param_to_be_estimated, " ~ ", indep_var, ", random = ~", random, ", data = dataset)")
        fit <- eval(parse(text = expression_recreated))
      }else{
        if (length(random) == 0 & length(expre) != 0) {
          expression_recreated =  paste0("lm(", param_to_be_estimated, " ~ ", expre, " + ", indep_var, ", data = dataset)")
          fit <- eval(parse(text = expression_recreated))
        }else{
          expression_recreated =  paste0("lm(", param_to_be_estimated, " ~ ", indep_var, ", data = dataset)")
          fit <- eval(parse(text = expression_recreated))
        }
      }

    }

  summary_regression_results = summary(fit)
  vcov_param_estimated <- stats::vcov(fit)
  chol_decomp_matrix <- chol(vcov_param_estimated)
  results =  structure(list(
    param_estimated = fit,
    summary_regression_results = summary_regression_results,
    variance_covariance = vcov_param_estimated,
    cholesky_decomp_matrix = chol_decomp_matrix
  ))
  return(results)
}
#######################################################################
#' Get the parameter values from reading a file
#' @param paramfile  parameter file to get the mortality eg.national life table data
#' @param age age to get the age specific data
#' @param gender gender details to get the gender specific mortality data
#' @return the paramvalue
#' @examples
#' paramfile= system.file("extdata","LifeTable_USA_Mx_2015.csv",
#' package = "packDAMipd")
#' a <- get_mortality_from_file(paramfile, age = 10, gender = NULL)
#'@export
get_mortality_from_file <- function(paramfile, age, gender = NULL){
  if (is.null(paramfile))
    stop("Need to provide a parameter file to lookup")
  if (IPDFileCheck::test_file_exist_read(paramfile) != 0)
    stop("File doesnt exists or not able to access")
  dataset <- data.frame(read.csv(paramfile,header = TRUE, sep = ",", stringsAsFactors = FALSE))
  result <- IPDFileCheck::check_column_exists("age",dataset)
  if (result != 0)
    stop("Expecting the life tables to contain an age column")
  age_columnno = IPDFileCheck::get_columnno_fornames(dataset, "age")
  this_row = which(dataset[age_columnno] == age)
  if (length(this_row) == 0) {
    i = 1
    while (i <= nrow(dataset)) {
      the_string = dataset[[age_columnno]][i]
      pos = stringr::str_locate(the_string, "-")
      if (sum(is.na(pos)) < 2) {
        minage = as.numeric(substr(the_string, 1, pos[1] - 1))
        maxage = as.numeric(substr(the_string, pos[1] + 1, nchar(the_string)))
      }else{
        pos = stringr::str_locate(the_string, "and over")
        minage = as.numeric(substr(the_string, 1, pos[1] - 1))
        maxage = 120
      }
      if (minage <= age & maxage >= age) {
        this_row = i
        i = nrow(dataset) + 1
      }else{
        i = i + 1
      }
    }
  }
  if (length(this_row) == 0)
      stop("Error - Row corresponding to the specific age could not be found")
  if (is.null(gender)) {
    total_columnno = IPDFileCheck::get_colno_pattern_colname("total", colnames(dataset))
    mortality <- dataset[[total_columnno]][this_row]
  }else{
    sex_columnno = IPDFileCheck::get_columnno_fornames(dataset, gender)
    mortality <- dataset[[sex_columnno]][this_row]
  }
  return(mortality)
}
######################################################################

#' Bivaraite regression for correlated values
#' @param param1_to_be_estimated  parameter of interest
#' @param param2_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates1 list of covariates - for equaiton 1
#' @param covariates2 list of covariates - for equaiton 2
#' @param interaction1 boolean value to indicate interaction - for equaiton 1
#' @param interaction2 boolean value to indicate interaction - for equaiton 2
#' false by default
#' @return the results of the regression analysis
#' @examples
#' mydata <- foreign::read.dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta")
#' results_sureg <- use_seemingly_unrelated_regression("read", "math", dataset = mydata,
#' indep_var = "female", covariates1 = c("as.numeric(ses)", "socst"),
#' covariates2 = c("as.numeric(ses)", "science"),interaction1 = FALSE,  interaction2 = FALSE)
#' @export
use_seemingly_unrelated_regression <- function(param1_to_be_estimated, param2_to_be_estimated, dataset, indep_var,
                                              covariates1, covariates2, interaction1, interaction2) {
  formula1 = form_expression_lm(param1_to_be_estimated,indep_var, covariates1,interaction1 )
  formula2 = form_expression_lm(param2_to_be_estimated,indep_var, covariates2,interaction2 )
  fitsur <- systemfit::systemfit(list(formula1, formula2), data = dataset)
  summary_regression_results <- summary(fitsur)
  ci_estimates <- stats::confint(fitsur)
  vcov_param_estimated <- summary_regression_results$coefCov
  chol_decomp_matrix <- chol(vcov_param_estimated)
  std_error <- stats::coef(summary_regression_results)
  #name_file_plot <- paste0("lm_", fitsur, "_", indep_var, ".pdf", sep = "")
  #plot_result <- ggplot2::autoplot(fit, toPdf = TRUE, file = name_file_plot)
  corre_matrix <- summary_regression_results$residCor
  OLS_R2 <- summary_regression_results$ols.r.squared
  AIC <- stats::AIC(fitsur)
  BIC <- stats::BIC(fitsur)
  check_model_fit <- data.frame(cbind(OLS_R2, AIC , BIC))
  colnames(check_model_fit) <- c("OLS_R2", "AIC", "BIC")
  results =  structure(list(
    param_estimated = fitsur,
    std_error = std_error,
    corre_matrix  = corre_matrix,
    check_model_fit = check_model_fit,
    ci_estimates = ci_estimates,
    variance_covariance = vcov_param_estimated,
    summary_regression_results = summary_regression_results,
    cholesky_decomp_matrix = chol_decomp_matrix
    #plot = plot_result
  ))
  return(results)
}

#######################################################################
##' Get the parameter values using the provided statistical regression methods
##' @param param_to_be_estimated  parameter of interest
##' @param dataset data set to be provided
##' @param method method of estimation (for example, linear, logistic regression etc)
##' @param indep_var the independent variable (column name in data file)
##' @param info_get_method additional information on methods e.g Kaplan-Meier ot hazard
##' @param info_distribution distribution name  eg. for logistic regression -binomial
##' @param covariates list of covariates - calculations to be done before passing
##' @param strategycol column name containing arm details, default is NA
##' @param strategyname name of the  arm, default is NA
##' @param timevar_survival time variable for survival analysis, default is NA
##' @return the results of the regression analysis
##' @examples
##' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
##' results_logit <- get_parameter_estimated_regression("admit", mydata,
##' "logistic regression", "gre", NA,"binomial", c("gpa", "factor(rank)"))
##' @export
# get_parameter_estimated_regression <- function(param_to_be_estimated, dataset, method,
#                                             indep_var, info_get_method, info_distribution,
#                                             covariates = NA, strategycol = NA,
#                                             strategyname = NA, timevar_survival = NA){
#   if (is.null(dataset))
#     stop("Need to provide a data set to lookup")
#   if (is.na(method))
#     stop("Please provide  a statistical method")
#   if (!is.na(strategycol)) {
#     if (IPDFileCheck::check_column_exists(strategycol,dataset) != 0)
#       dataset = dataset[dataset[[strategycol]] == strategyname,]
#   }
#   method_rpack <- find_keyword_regression_method(method, info_get_method)
#   caps_info_method <- toupper(info_get_method)
#   caps_method = toupper(method)
#   if (caps_method == "SURVIVAL" | caps_method == "SURVIVAL ANALYSIS") {
#     if (is.na(timevar_survival)) {
#       stop("For survival analysis, please provide the varaible to use as time ")
#     }else{
#       surv_object <- paste("survival::Surv(", timevar_survival, ",", param_to_be_estimated, ")", sep = "")
#       object <- surv_object
#     }
#   }else{
#     object <- param_to_be_estimated
#   }
#   if (sum(is.na(covariates)) == 0) {
#     covariates_list = list()
#     for (i in 1:length(covariates)) {
#       if (i == length(covariates))
#         covariates_list = paste(covariates_list, paste(covariates[i], sep = ""))
#       else
#         covariates_list = paste(covariates_list, paste(covariates[i], " + ", sep = ""))
#     }
#   }else{
#     covariates_list = NA
#   }
#   if (caps_method == "SURVIVAL" | caps_method == "SURVIVAL ANALYSIS") {
#     if (caps_info_method == "PARAMETRIC REGRESSION" | caps_info_method == "PARAMETRIC") {
#       this_dist <- find_survreg_distribution(info_distribution)
#       if (is.na(covariates_list))
#         expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, ", ",
#                                      "data = dataset,  dist = \"", this_dist, "\" ) ", sep = "")
#       else
#         expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, " + ",
#                                      covariates_list, ", ","data = dataset,  dist = \"",
#                                      this_dist, "\" ) ", sep = "")
#     }else{
#       if (caps_info_method == "KAPLAN-MEIER" | caps_info_method == "KM") {
#         if (is.na(covariates_list))
#               expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var,",type =",
#                                            "\"kaplan-meier\"", ", ","data = dataset) ", sep = "")
#         else
#             expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, " + ", covariates_list,
#                                          ", type =", "\"kaplan-meier\"",", ","data = dataset) ", sep = "")
#       }else{
#         if (caps_info_method == "FLEMING-HARRINGTON" | caps_info_method == "FH") {
#           if (is.na(covariates_list))
#                 expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var,", type =",
#                                              "\"fleming-harrington\"",", ","data = dataset) ", sep = "")
#           else
#                 expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, " + ", covariates_list,
#                                              ", type =", "\"fleming-harrington\"",", ","data = dataset) ", sep = "")
#         }else{
#           if (caps_info_method == "FH2") {
#             if (is.na(covariates_list))
#               expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, ", type =", "\"fh2\"",
#                                            ", ","data = dataset) ", sep = "")
#             else
#               expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, " + ", covariates_list,
#                                            ", type =", "\"fh2\"",", ","data = dataset) ", sep = "")
#           }else{
#             if (is.na(covariates_list))
#               expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, ", ","data = dataset) ", sep = "")
#             else
#               expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, " + ", covariates_list,
#                                            ", ", "data = dataset) ", sep = "")
#           }
#         }
#       }
#     }
#   }else{
#     if (is.na(covariates_list))
#       expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, ", ","data = dataset) ", sep = "")
#     else
#       expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, " + ", covariates_list,
#                                    ", ", "data = dataset) ", sep = "")
#   }
#
#   param_estimated <- eval(parse(text = expression_recreated))
#   summary_regression_results = summary(param_estimated)
#   if (caps_info_method %in% c("KAPLAN-MEIER","KM","FLEMING-HARRINGTON", "FH2","FH"))
#     vcov_param_estimated <- chol_decomp_matrix <- list()
#   else{
#     vcov_param_estimated <- stats::vcov(param_estimated)
#     chol_decomp_matrix <- chol(vcov_param_estimated)
#   }
#   if (caps_info_method %in% c("PARAMETRIC REGRESSION","PARAMETRIC"))
#     plot_result <- graphics::plot(param_estimated)
#   else{
#     if (caps_info_method %in% c("COX-PROPORTIONAL-HAZARD","COX PROPORTIONAL HAZARD","COX-PH", "COX PH","COXPH"))
#       plot_result <- ggplot2::autoplot(survival::survfit(param_estimated))
#     else
#     plot_result <- ggplot2::autoplot(param_estimated)
#   }
#
#   results =  structure(list(
#     param_estimated = param_estimated,
#     variance_covariance = vcov_param_estimated,
#     summary_regression_results = summary_regression_results,
#     cholesky_decomp_matrix = chol_decomp_matrix,
#     plot = plot_result
#   ))
#   # if(caps_method == "SURVIVAL" | caps_method == "SURVIVAL ANALYSIS"){
#   #   if(!is.na(info_distribution) & trimws(toupper(info_distribution))== "WEIBULL"){
#   #     params_hr_etr = SurvRegCensCov::ConvertWeibull(param_estimated)
#   #     results =  structure(list(
#   #       param_estimated = param_estimated,
#   #       variance_covariance = vcov_param_estimated,
#   #       summary_regression_results = summary_regression_results,
#   #       cholesky_decomp_matrix = chol_decomp_matrix,
#   #       weibull_params_survival_analysis = params_hr_etr,
#   #       plot = plot_result
#   #     ))
#   #   }
#   #}
#   return(results)
# }
