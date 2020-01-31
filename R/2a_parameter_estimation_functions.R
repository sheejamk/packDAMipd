#######################################################################
#' Get the parameter values from reading a file
#' @param parameter  parameter of interest
#' @param param_value parameter value to be assigned
#' @return the paramvalue
#' @examples
#' a <- get_parameter_direct("cost_IT", param_value=100)
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
#' a <- get_parameter_read("cost_IT", paramfile=system.file("extdata","table_param.csv",
#' package = "packDAMipd"))
#' @description  the file should have these column names (atleast)
#' Parameter,	Description,	Strategy,	Value
#' @export
get_parameter_read <- function(parameter, paramfile, strategycol=NA, strategyname = NA){
  if (is.null(paramfile))
    stop("Need to provide a parameter file to lookup")
  if (IPDFileCheck::test_file_exist_read(paramfile) != 0)
    stop("File doesnt exists or notable to access")
  dataset <- data.frame(read.csv(paramfile,header = TRUE,sep = ",", stringsAsFactors = FALSE))
  result <- IPDFileCheck::check_column_exists("value",dataset)
  if (result != 0)
    stop(paste("Parameter file should contain value in column name",sep = ""))
  if (!is.na(strategycol)) {
    if (IPDFileCheck::check_column_exists(strategycol,dataset) == 0) {
      dataset = dataset[dataset[[strategycol]] == strategyname,]
      answer <- dataset[dataset$Parameter == parameter,]$Value
    }else{
      stop(paste("Parameter file should contain strategy in column name",sep = ""))
    }
  }else{
    answer <- dataset[dataset$Parameter == parameter,]$Value
    if (is.na(answer))
      warning("Error- Parameter value is NA-Did you use the wrong function
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
  dataset <- data.frame(read.csv(paramfile,header = TRUE,sep = ",",stringsAsFactors = FALSE))
  if (!is.na(strategycol)) {
    if (IPDFileCheck::check_column_exists(strategycol,dataset) != 0)
      dataset = dataset[dataset[[strategycol]] == strategyname,]
  }
  result <- IPDFileCheck::check_column_exists("Distribution",dataset)
  if (result != 0) {
    stop(paste("Parameter file should contain distribution in column name",sep = ""))
  }
  if (is.na(dataset[dataset$Parameter == parameter,]$Distribution))
    stop("This parameter may not be estimated from a distribution- use get_parameter_read
         instead")
  param_distribution = c("Param1_name","Param1_value")
  result <- unlist(lapply(param_distribution,IPDFileCheck::check_column_exists,dataset))
  if (sum(result) != 0)
    stop(paste("Parameter file should contain parameters for the distribtution in
               column name",sep = ""))
  this_param <- dataset[dataset$Parameter == parameter,]$Parameter
  this_distr_name <- dataset[dataset$Parameter == parameter,]$Distribution
  param1 <- dataset[dataset$Parameter == parameter,]$Param1_name
  param1_value <- dataset[dataset$Parameter == parameter,]$Param1_value
  if (!is.na(dataset[dataset$Parameter == parameter,]$Param2_name)) {
    param2 <- dataset[dataset$Parameter == parameter,]$Param2_name
    param2_value <- dataset[dataset$Parameter == parameter,]$Param2_value
    expression_created = paste(this_distr_name,"(",param1 ,"=", param1_value, "," ,
                               param2, "=",param2_value,")",sep = "")
  }else{
    expression_created = paste(this_distr_name,"(",param1, "=" , param1_value,")",sep = "")
  }
  expression_recreated <- check_estimate_substitute_proper_params(expression_created)
  param_with_expression <- paste(this_param, " = ", expression_recreated, sep = "")
  return(param_with_expression)
}

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
#' @return the results of the regression analysis
#' @examples
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' results_logit <- get_parameter_estimated_regression("admit", mydata,
#' "logistic regression", "gre", NA,"binomial", c("gpa", "factor(rank)"))
#' @export
get_parameter_estimated_regression <- function(param_to_be_estimated, dataset, method,
                                      indep_var, info_get_method, info_distribution,
                                      covariates=NA, strategycol=NA,
                                      strategyname=NA, timevar_survival=NA, interaction=NA){
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
        covariates_list = paste(covariates_list,paste(covariates[i], sep = ""))
      else
        covariates_list = paste(covariates_list,paste(covariates[i], " + ", sep = ""))
    }
  }else{
    covariates_list = NA
  }
  caps_method = toupper(method)
  if (caps_method == "SURVIVAL" | caps_method == "SURVIVAL ANALYSIS")
    results <- use_survival_analysis(param_to_be_estimated, dataset, indep_var, info_get_method,
                                     info_distribution,covariates_list , timevar_survival)
  if (caps_method == "LINEAR REGRESSION" | caps_method == "LINEAR_REGRESSION" | caps_method == "LINEAR")
    results <- use_linear_rgression(param_to_be_estimated, dataset,indep_var,covariates, interaction)
  if (caps_method == "LOGISTIC REGRESSION" | caps_method == "LOGISTIC_REGRESSION" | caps_method == "LOGISTIC")
    results <- use_logistic_rgression(param_to_be_estimated, dataset,indep_var, info_distribution, covariates_list)
  if (caps_method == "MULTILEVEL MODELLING" | caps_method == "MULTILEVEL_MODELLING" | caps_method == "MULTILEVEL"
     | caps_method == "MIXED EFFECT" | caps_method == "MIXED_EFFECT" )
    results <- use_mixed_effect_model(param_to_be_estimated, dataset,indep_var,covariates)
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
#' data_for_survival<-survival::aml
#' surv_estimated_aml <- use_survival_analysis("status", data_for_survival,
#' "x", info_get_method="parametric", info_distribution = "weibull",
#' covariates_list=NA,"time")
#' @export
use_survival_analysis <- function(param_to_be_estimated, dataset,
                                 indep_var, info_get_method, info_distribution,
                                 covariates_list, timevar_survival) {
  if (is.na(info_get_method))
    stop("Please provide  information statistical method")
  caps_info_method <- toupper(info_get_method)
  if (caps_info_method == "PARAMETRIC REGRESSION" | caps_info_method == "PARAMETRIC")
    results <- use_parametric_regression(param_to_be_estimated, dataset,indep_var,
                                         info_distribution,covariates_list,timevar_survival)
  if (caps_info_method == "KAPLAN-MEIER" | caps_info_method == "KM")
    results <- use_km_survival(param_to_be_estimated, dataset, indep_var,covariates_list,
                               timevar_survival)
  if (caps_info_method == "FLEMING-HARRINGTON" | caps_info_method == "FH")
    results <- use_fh_survival(param_to_be_estimated, dataset,indep_var,covariates_list,
                               timevar_survival)
  if (caps_info_method == "FH2")
    results <- use_fh2_survival(param_to_be_estimated, dataset,indep_var,covariates_list,
                                timevar_survival)
  if (caps_info_method %in% c("COX-PROPORTIONAL-HAZARD","COX PROPORTIONAL HAZARD","COX-PH",
                              "COX PH","COXPH"))
    results <- use_coxph_survival(param_to_be_estimated, dataset, indep_var, covariates_list,
                                  timevar_survival)
  return(results)

}
#' ##########################################################################################################
#' Get the parameter values using the survival analysis parametric regression
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param info_distribution distribution name  eg. for logistic regression -binomial
#' @param covariates_list list of covariates - calculations to be done before passing
#' @param timevar_survival time variable for survival analysis, default is NA
#' @return the results of the regression analysis
#' @examples
#' data_for_survival<-survival::aml
#' surv_estimated_aml <- use_parametric_regression("status", data_for_survival,"x",
#' info_distribution="weibull", covariates_list=NA,"time")
#' @export
use_parametric_regression <- function(param_to_be_estimated, dataset,
                                      indep_var, info_distribution,covariates_list,
                                      timevar_survival){
  if (is.na(timevar_survival))
    stop("For survival analysis, please provide the varaible to use as time ")
  else
    surv_object <- paste("survival::Surv(", timevar_survival, ",", param_to_be_estimated, ")", sep = "")
  if (is.na(info_distribution))
    stop("Error - information on distribution is missing")
  else
    this_dist <- find_survreg_distribution(info_distribution)
  if (is.na(covariates_list))
    expression_recreated <- paste0("survival::survreg","(", surv_object, " ~ ", indep_var, ", ",
                                   "data = dataset,  dist = \"", this_dist, "\" ) ", sep = "")
  else
    expression_recreated <- paste0("survival::survreg","(", surv_object, " ~ ", indep_var, " + ",
                                   covariates_list, ", ","data = dataset,  dist = \"",
                                   this_dist, "\" ) ", sep = "")
  param_estimated <- eval(parse(text = expression_recreated))
  summary_regression_results = summary(param_estimated)
  vcov_param_estimated <- stats::vcov(param_estimated)
  chol_decomp_matrix <- chol(vcov_param_estimated)
  results =  structure(list(
    param_estimated = param_estimated,
    summary_regression_results = summary_regression_results,
    variance_covariance = vcov_param_estimated,
    cholesky_decomp_matrix = chol_decomp_matrix
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
#' data_for_survival<-survival::aml
#' surv_estimated_aml<-use_km_survival("status", data_for_survival, "x", covariates_list=NA, "time")
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
    expression_recreated <- paste0("survival::survfit","(", surv_object, " ~ ", indep_var, " + ",
                                   covariates_list,", type =", "\"kaplan-meier\"",", "
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
#' data_for_survival<-survival::aml
#' surv_estimated_aml<-use_fh_survival("status", data_for_survival, "x", covariates_list=NA, "time")
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
    expression_recreated <- paste0("survival::survfit","(", surv_object, " ~ ", indep_var, " + ",
                                   covariates_list,", type =", "\"fleming-harrington\"",", ",
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
#' data_for_survival<-survival::aml
#' surv_estimated_aml<-use_fh2_survival("status", data_for_survival, "x", covariates_list=NA, "time")
#' @export
use_fh2_survival <- function(param_to_be_estimated, dataset,
                             indep_var,covariates_list, timevar_survival){
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
    expression_recreated <- paste0("survival::survfit","(", surv_object, " ~ ", indep_var,
                                   " + ", covariates_list,", type =", "\"fh2\"",", ","data = dataset) ", sep = "")
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
#' data_for_survival<-survival::aml
#' surv_estimated_aml <- use_coxph_survival("status", data_for_survival, "x",
#' covariates_list = NA, "time")
#' @export
use_coxph_survival <- function(param_to_be_estimated, dataset,indep_var,
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
    expression_recreated <- paste0("survival::coxph","(", surv_object, " ~ ", indep_var, " + ",
                                   covariates_list,", ", "data = dataset) ", sep = "")
  param_estimated <- eval(parse(text = expression_recreated))
  summary_regression_results = summary(param_estimated)
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
#' @param info_distribution distribution name  eg. for logistic regression -binomial
#' @param covariates_list list of covariates - calculations to be done before passing
#' @return the results of the regression analysis
#' @examples
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#' mydata$rank <- factor(mydata$rank)
#' results_logit <- use_logistic_rgression("admit", dataset=mydata,
#' indep_var = "gre", info_distribution ="binomial", covariates_list = NA)
#' @export
use_logistic_rgression <- function(param_to_be_estimated, dataset,indep_var,
                                   info_distribution,covariates_list){
  if (is.na(info_distribution))
    stop("Error - information on distribution is missing")
  else
    this_dist <- find_glm_distribution(info_distribution)
  if (is.na(covariates_list))
    expression_recreated <- paste0("glm","(", param_to_be_estimated, " ~ ", indep_var,
                                   ", family = ", this_dist, ", data = dataset) ", sep = "")
  else
    expression_recreated <- paste0("glm","(", param_to_be_estimated, " ~ ", indep_var, " + ",
                                   covariates_list, ", family = ", this_dist, ", data = dataset) ",
                                   sep = "")

  param_estimated <- eval(parse(text = expression_recreated))
  summary_regression_results = summary(param_estimated)
  vcov_param_estimated <- stats::vcov(param_estimated)
  chol_decomp_matrix <- chol(vcov_param_estimated)
  plot_result <- ggplot2::autoplot(param_estimated)
  results =  structure(list(
    param_estimated = param_estimated,
    variance_covariance = vcov_param_estimated,
    summary_regression_results = summary_regression_results,
    cholesky_decomp_matrix = chol_decomp_matrix,
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
#' results_logit <- use_linear_rgression("gre", dataset=mydata,
#' indep_var = "gpa", covariates = NA, interaction = FALSE)
#' @export
use_linear_rgression <- function(param_to_be_estimated, dataset,indep_var,covariates,interaction=FALSE){
  if (length(covariates) == 0 | is.na(covariates)) {
    # no need to check for interaction
    fmla <- stats::as.formula(paste(param_to_be_estimated, paste("~"),paste(indep_var,collapse = "+")))
    fit <- stats::lm(fmla,data = dataset)
    p_plt <- ggiraphExtra::ggPredict(fit,se = TRUE,interactive = TRUE)
  }else{
    expre = paste(covariates[1],sep = "")
    i = 2
    while (i <= length(covariates)) {
      this = paste(covariates[i],sep = "")
      expre = paste(expre,this,sep = "+")
      i = i + 1
    }
    if (interaction == FALSE) {
      fmla <- stats::as.formula(paste(param_to_be_estimated, paste("~"),paste(indep_var,"+",sep = ""),
                               paste(expre, collapse = "+")))
      fit <- stats::lm(fmla,data = dataset)
    }else{
      expre = paste(covariates[1],sep ="")
      i = 2
      while (i <= length(covariates)) {
        this = paste(covariates[i],sep = "")
        expre = paste(expre,this,sep = "*")
        i = i + 1
      }
      fmla <- stats::as.formula(paste(param_to_be_estimated, paste("~"),paste(indep_var,"*",sep = ""),
                               paste(expre, collapse = "*")))
      fit <- stats::lm(fmla,data = dataset)
    }
  }
  summary_regression_results = summary(fit)
  vcov_param_estimated <- stats::vcov(fit)
  chol_decomp_matrix <- chol(vcov_param_estimated)
  plot_result <- ggplot2::autoplot(fit)
  results =  structure(list(
    param_estimated = fit,
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
#' @param covariates, independent variables , NA by default
#' @return result regression result with plot if success and -1, if failure
#' @examples
#' mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
#  results_logit <- use_mixed_effect_model("gre", dataset=mydata,
#                                          indep_var = "gpa", covariates = NA)
#' @export
use_mixed_effect_model <- function(param_to_be_estimated, dataset, indep_var,covariates=NA){
  if (sum(is.na(covariates)) == 0) {
    expre = paste("1|",covariates[1],sep = "")
    i = 2
    while (i <= length(covariates)) {
      this = paste("1|", covariates[i], sep = "")
      expre = paste(expre,this,sep = "+")
      i = i + 1
    }
    fmla <- stats::as.formula(paste(param_to_be_estimated, paste("~"),paste(indep_var,"+"),
                             paste(expre, collapse = "+")))
    fit <- lme4::lmer(fmla,data = dataset)
  }else{
    fmla <- stats::as.formula(paste(param_to_be_estimated, paste("~"),paste(indep_var,collapse = "+")))
    fit <- stats::lm(fmla,data = dataset)
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
#' paramfile=system.file("extdata","LifeTable_USA_Mx_2015.csv",
#' package = "packDAMipd")
#' a <- get_mortality_from_file(paramfile, age = 10, gender = NULL)
#'@export
get_mortality_from_file <- function(paramfile, age, gender = NULL){
  if (is.null(paramfile))
    stop("Need to provide a parameter file to lookup")
  if (IPDFileCheck::test_file_exist_read(paramfile) != 0)
    stop("File doesnt exists or not able to access")
  dataset <- data.frame(read.csv(paramfile,header = TRUE,sep = ",", stringsAsFactors = FALSE))
  result <- IPDFileCheck::check_column_exists("age",dataset)
  if (result != 0)
    stop("Expecting the life tables to contain an age column")
  age_columnno = IPDFileCheck::get_columnno_fornames(dataset, "age")
  this.row = which(dataset[age_columnno] == age)
  if(this.row > 0 ){
    if (is.null(gender)) {
      total_columnno = IPDFileCheck::get_colno_pattern_colname("total", colnames(dataset))
      mortality <- dataset[[total_columnno]][this.row]
    }else{
      sex_columnno = IPDFileCheck::get_columnno_fornames(dataset, gender)
      mortality <- dataset[[sex_columnno]][this.row]
    }
  }else{
    stop("Error - Row corresponding to the specific age could not be found")
  }

  return(mortality)
}
######################################################################

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
#   method_rpack <- find_keyword_regression_method(method,info_get_method)
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
#         covariates_list = paste(covariates_list,paste(covariates[i], sep = ""))
#       else
#         covariates_list = paste(covariates_list,paste(covariates[i], " + ", sep = ""))
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
#             expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, " + ",covariates_list,
#                                          ", type =", "\"kaplan-meier\"",", ","data = dataset) ", sep = "")
#       }else{
#         if (caps_info_method == "FLEMING-HARRINGTON" | caps_info_method == "FH") {
#           if (is.na(covariates_list))
#                 expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var,", type =",
#                                              "\"fleming-harrington\"",", ","data = dataset) ", sep = "")
#           else
#                 expression_recreated <- paste0(method_rpack,"(", object, " ~ ", indep_var, " + ",covariates_list,
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
