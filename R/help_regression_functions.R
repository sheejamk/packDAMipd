#######################################################################
#' Do the diagnostic test for glm model assumption
#' @param method param describing the methods, expects glm
#' @param expression_recreated the expression recreated for calling lm
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param family family of distribution for generalised linear model
#' @param covariates list of covariates - calculations to be done before passing
#' @param interaction boolean value to indicate interaction in the case of linear regression,
#' false by default
#' @return the results of the regression analysis
#' @examples
#' @keywords internal
#' diagnostic tests for model assumption behind the glm,
#' note than ceresplot wasnt working when passing the dataset and yet to be implemented
#' correctly.(date May 3 2020)
do_diagnostic_glm <- function(method, fit,expression_recreated,param_to_be_estimated,
                                            dataset, indep_var,family, covariates, interaction){
  # Fit R2, AIC and BIC
  R2 <- 1 - (fit$deviance/fit$null.deviance)
  AIC <- stats::AIC(fit)
  BIC <- stats::BIC(fit)
  fit_diagnostics <- data.frame(cbind(R2, AIC, BIC))
  colnames(fit_diagnostics) <- c("R2", "AIC", "BIC")

  #  autocorrelation test
  autocorr_error_test <- lmtest::dwtest(fit)

  #  outlier test
  outlier_test = car::outlierTest(fit)

  # Anova table
  anova_table = stats::anova(fit) # anova table

  # influence fit
  influence_fit <- stats::influence(fit) # regression diagnostics

  # Evaluate homoscedasticity
  # non-constant error variance test
  # No need to check this in glm Ref:https://online.stat.psu.edu/stat504/node/216/)

  graphics::par(mfrow=c(2,2))

  name_file_plot <- paste0(method,"_Regression_diagnostics_plot_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)

  # plot studentized residuals vs. fitted values
  suppressWarnings(car::spreadLevelPlot(fit))

  # distribution of studentized residuals
  sresid <- MASS::studres(fit)
  graphics::hist(sresid, freq=FALSE,main="Distribution of Studentized Residuals")
  xfit <- seq(min(sresid),max(sresid),length=40)
  yfit <- stats::dnorm(xfit)
  graphics::lines(xfit, yfit)
  # Evaluate Nonlinearity component + residual plot
  if(interaction == FALSE)
    car::crPlots(fit)
  # Ceres plots
  # if(sum(is.na(covariates)) == 0 & interaction == FALSE & method == "glm"){
  #   expr1 = paste(param_to_be_estimated,expression_recreated$short_formula)
  #   fit_ceresPlots = stats::glm(expr1, data = dataset)
  #   car::ceresPlots(fit_ceresPlots)
  #   #car::ceresPlots(fit)
  # }
  car::influencePlot(fit, main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
  grDevices::dev.off()

  name_file_plot <- paste0(method, "_Residuals_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  plot_diagnostics <- ggplot2::autoplot(fit, toPdf = TRUE, file = name_file_plot)
  grDevices::dev.off()

  results = list(
    autocorr_error_test = autocorr_error_test,
    outlier_test = outlier_test,
    anova_table = anova_table,
    fit_diagnostics = fit_diagnostics
  )
  return(results)
}

#######################################################################
#' Do the diagnostic test for lm model assumption
#' @param method param describing the methods, expects lm
#' @param expression_recreated the expression recreated for calling lm
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates - calculations to be done before passing
#' @param interaction boolean value to indicate interaction in the case of linear regression,
#' false by default
#' @return the results of the regression analysis
#' @examples
#' @keywords internal
#' diagnostic tests for model assumption behind the glm,
#' note than ceresplot wasnt working when passing the dataset and yet to be implemented
#' correctly.(date May 3 2020)
do_diagnostic_linear_regression <- function(method, fit,expression_recreated,param_to_be_estimated,
                                            dataset, indep_var,covariates, interaction){
  # Fit R2, AIC and BIC
  Adj_R2 <- summary(fit)$adj.r.squared
  AIC <- stats::AIC(fit)
  BIC <- stats::BIC(fit)
  fit_diagnostics <- data.frame(cbind(Adj_R2, AIC, BIC))
  colnames(fit_diagnostics) <- c("Adj_R2", "AIC", "BIC")

  #  autocorrelation test
  autocorr_error_test <- lmtest::dwtest(fit)

  #  outlier test
  outlier_test = car::outlierTest(fit)

  # Anova table
  anova_table = stats::anova(fit) # anova table

  # influence fit
  influence_fit <- stats::influence(fit) # regression diagnostics

  # Evaluate homoscedasticity
  # non-constant error variance test
  non_constant_error_test <- car::ncvTest(fit)

  # Model fit assumption test
  model_fit_assumptions_test <- summary(gvlma::gvlma(fit))

  graphics::par(mfrow=c(2,2))

  name_file_plot <- paste0(method,"_Regression_diagnostics_plot_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)

  # plot studentized residuals vs. fitted values
  suppressWarnings(car::spreadLevelPlot(fit))

  # distribution of studentized residuals
  sresid <- MASS::studres(fit)
  graphics::hist(sresid, freq=FALSE,main="Distribution of Studentized Residuals")
  xfit <- seq(min(sresid),max(sresid),length=40)
  yfit <- stats::dnorm(xfit)
  graphics::lines(xfit, yfit)

  # Evaluate Nonlinearity component + residual plot
  if( interaction == FALSE)
    car::crPlots(fit)
  # Ceres plots --
  # if(sum(is.na(covariates)) == 0 & interaction == FALSE & method == "lm"){
  #   expr1 = paste(param_to_be_estimated,expression_recreated$short_formula)
  #   fit_ceresPlots = stats::lm(expr1, data = dataset)
  #   car::ceresPlots(fit_ceresPlots)
  # }
  car::influencePlot(fit, main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
  grDevices::dev.off()

  name_file_plot <- paste0(method, "_Residuals_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  plot_diagnostics <- ggplot2::autoplot(fit, toPdf = TRUE, file = name_file_plot)
  grDevices::dev.off()

  results = list(
    autocorr_error_test = autocorr_error_test,
    outlier_test = outlier_test,
    anova_table = anova_table,
    non_constant_error_test = non_constant_error_test,
    model_fit_assumptions_test = model_fit_assumptions_test,
    fit_diagnostics = fit_diagnostics
  )
  return(results)
}

#######################################################################
#' Do the prediction for regression
#' @param method param describing the methods, expects lm
#' @param expression_recreated the expression recreated for calling lm
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates - calculations to be done before passing
#' @param interaction boolean value to indicate interaction in the case of linear regression,
#' false by default
#' @return the results of the regression analysis
#' @examples
#' @keywords internal
#' Model prediction after regression using effects package.
#' works for glm and lm (tested o May 3, 2020)
prediction_regression <- function(method, fit, expression_recreated,
                                  param_to_be_estimated, indep_var,
                                  covariates, interaction){
  predictor_effect <- effects::predictorEffects(fit)
  leng = length(predictor_effect)
  prediction_all <- c()
  this_names= c()
  for(i in 1:leng){
    this = (predictor_effect[[i]]$model.matrix[,])
    prediction_all = append(prediction_all,list(this))
    if(i == leng){
      this_names = append(this_names, paste(indep_var, sep=""))
    }else{
      this_names = append(this_names, paste(covariates[i], sep=""))
    }
  }
  prediction_all = append(prediction_all,list(predictor_effect[[1]]$fit))
  names(prediction_all) <- c(this_names, param_to_be_estimated)

  name_file_plot <- paste0(method, "_Prediction_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  plot_prediction <- graphics::plot(predictor_effect, lines = list(multiline = TRUE))
  grDevices::dev.off()

  results = list(
    prediction_all = prediction_all
  )
}

