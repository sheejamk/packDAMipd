#######################################################################
#' Form expression to use with glm()
#' @param param_to_be_estimated  parameter of interest
#' @param indep_var the independent variable (column name in data file)
#' @param family distribution name  eg. for logistic regression -binomial
#' @param covariates list of covariates
#' @param interaction boolean value to indicate interaction in the case of
#' generalised linear models, false by default
#' @param naaction action to be taken with the missing values
#' @param link link function if not the default for each family
#' @return the formula for glm
#' @examples
#' formula <- form_expression_glm("admit",
#'   indep_var = "gre", family = "binomial",
#'   covariates = c("gpa", "rank"), interaction = FALSE, naaction = "na.omit",
#'   link = NA)
#' @export
#' @details
#' Form expression for the method glm
form_expression_glm <- function(param_to_be_estimated, indep_var, family,
                                covariates, interaction, naaction, link) {
  check_list = list(param_to_be_estimated,indep_var,interaction)
  checks = sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop( "Error - some of the required parameters are NULL or NA")

  if (is.na(family)) {
    stop("Error - information on distribution is missing")
  } else {
    this_dist <- find_glm_distribution(family)
  }
  if (!is.na(link)) {
    link <- check_link_glm(this_dist, link)
    family_def <- paste(this_dist, "(link = ", link, ")", sep = "")
  } else {
    family_def <- paste(this_dist, sep = "")
  }
  if (length(covariates) == 0 | sum(is.na(covariates)) == length(covariates)) {
    # no need to check for interaction
    fmla <- paste("glm(", param_to_be_estimated, " ~ ", indep_var, ",
                  family = ", family_def,
                  ", data = dataset, na.action =", naaction, ")",
                  sep = ""
    )
    short_fmla <- paste(" ~ ", indep_var, sep = "")
  } else {
    expre <- paste(covariates[1], sep = "")
    i <- 2
    while (i <= length(covariates)) {
      this <- paste(covariates[i], sep = "")
      if (interaction) {
        expre <- paste(expre, this, sep = " * ")
      } else {
        expre <- paste(expre, this, sep = " + ")
      }
      i <- i + 1
    }
    fmla <- paste("glm(", param_to_be_estimated, " ~ ", expre, " + ",
                  indep_var, ", family = ", family_def,
                  ", data = dataset, na.action =", naaction, ")",
                  sep = ""
    )
    short_fmla <- paste(" ~ ", expre, " + ", indep_var, sep = "")
  }
  expressions <- list(formula = fmla, short_formula = short_fmla)
  return(expressions)
}
#######################################################################
#' Function to find the keyword for family of distribution in glm
#' @param text distribution
#' @return the keyword - the name of distribution
#' @examples
#' find_glm_distribution("gamma")
#' @export
#' @details
#' Find the family for glm method
find_glm_distribution <- function(text) {
  # checking if test is NULL or NA
  if (is.null(text)) {
    stop("Error - text can not be null")
  }else{
    if (is.na(text))
      stop("Error - text can not be null or NA")
  }
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text == "BINOMIAL" | text == "BI NOMIAL") {
    keyword <- "binomial"
  }
  if (text == "GAUSSIAN" | text == "NORMAL") {
    keyword <- "gaussian"
  }
  if (text == "GAMMA") {
    keyword <- "gamma"
  }
  if (text == "INVERSE.GAUSSIAN" | text == "INVERSE GAUSSIAN" |
      text == "INVERSE_GAUSSIAN") {
    keyword <- "inverse.gaussian"
  }
  if (text == "POISSON") {
    keyword <- "poisson"
  }
  if (text == "QUASI") {
    keyword <- "quasi"
  }
  if (text == "QUASI BINOMIAL" | text == "QUASI_BINOMIAL") {
    keyword <- "quasibinomial"
  }
  if (text == "QUASI POISSON" | text == "QUASI_POISSON") {
    keyword <- "quasipoisson"
  }
  if (is.null(keyword)) {
    stop("Error - glm - family of distribution not found  ")
  }
  return(keyword)
}
#######################################################################
#' Function to find the keyword for family of distribution in glm
#' @param family family of distribution
#' @param link function to be used
#' @return the link if they can be accepted else error
#' @examples
#' check_link_glm("gaussian", "identity")
#' @export
#' @details
#' Check and get the link function for the method glm
check_link_glm <- function(family, link) {
  #testing for null or NA
  if (is.null(family)) {
    stop("Error - Family can not be null ")
  }
  if (is.na(family)) {
    stop("Error - Family can not be NA ")
  }
  if (is.null(link)) {
    stop("Error - link can not be null")
  }
  if (is.na(link)) {
    stop("Error - link can not be NA")
  }
  family <- tolower(trimws(toupper(family)))
  link <- tolower(trimws(toupper(link)))
  if (family == "gaussian") {
    link_accept <- c("identity", "log", "inverse")
  }
  if (family == "binomial") {
    link_accept <- c("logit", "probit", "cauchit", "log", "loglog")
  }
  if (family == "gamma") {
    link_accept <- c("inverse", "identity", "log")
  }
  if (family == "poisson") {
    link_accept <- c("sqrt", "identity", "log")
  }
  if (family == "inverse.gaussian") {
    link_accept <- c("1/mu^2", "identity", "log", "inverse")
  }
  if (family == "quasi") {
    link_accept <- c("logit", "probit", "cloglog", "identity", "inverse",
                     "log", "1/mu^2", "sqrt")
  }
  if (family == "quasibinomial") {
    link_accept <- c("logit", "probit", "cloglog", "identity", "inverse",
                     "log", "1/mu^2", "sqrt")
  }
  if (family == "quasipoisson") {
    link_accept <- c("logit", "probit", "cloglog", "identity", "inverse",
                     "log", "1/mu^2", "sqrt")
  }
  matching <- match(link, link_accept)
  #if not matching throw error or return the link
  if (is.na(matching)) {
    stop(paste("Error - link given- ", link, " can not be accepted by family-",
               family, sep = ""))
  } else {
    return(link)
  }
}
#######################################################################
#' Do the diagnostic test for glm model assumption
#' @param method param describing the methods, expects glm
#' @param expression_recreated the expression recreated for calling lm
#' @param param_to_be_estimated  parameter of interest
#' @param dataset data set to be provided
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates - calculations to be done before passing
#' @param interaction boolean value to indicate interaction in the case of linear regression,
#' false by default
#' @return the results of the regression analysis
#' @examples
#' \donttest{
#' datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
#' mydata <- read.csv(datafile)
#' results_logit <- use_generalised_linear_model("admit",dataset = mydata,
#' indep_var = "gre", family = "binomial", covariates = NA,
#' interaction = FALSE,naaction = "na.omit", link = NA)
#' do_diagnostic_glm("glm", results_logit$fit, results_logit$fit$call,
#' "admit",mydata, "gre", covariates = NA, interaction = FALSE)
#' }
#' @importFrom MASS studres
#' @importFrom lmtest dwtest
#' @export
#' @keywords internal
do_diagnostic_glm <- function(method = "glm", fit, expression_recreated,
                              param_to_be_estimated,
                              dataset, indep_var, covariates, interaction) {

  check_list = list(param_to_be_estimated,indep_var,interaction, expression_recreated)
  checks = sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop( "Error - some of the required parameters are NULL or NA")

  if (is.null(method)) {
    stop("Error - method should not be NULL")
  }else{
    if (method != "glm") stop("Error - method should be glm")
  }
  if (!("glm" %in% class(fit)))
    stop("Error- Fit object should be of type glm")

  # checking if dataset is is NULL
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  # Fit R2, AIC and BIC
  R2 <- 1 - (fit$deviance / fit$null.deviance)
  AIC <- stats::AIC(fit)
  BIC <- stats::BIC(fit)
  fit_diagnostics <- data.frame(cbind(R2, AIC, BIC))
  colnames(fit_diagnostics) <- c("R2", "AIC", "BIC")

  #  auto correlation test
  autocorr_error_test <- lmtest::dwtest(fit)

  #  outlier test
  outlier_test <- car::outlierTest(fit)

  # Anova table
  anova_table <- stats::anova(fit) # anova table

  # influence fit
  influence_fit <- stats::influence(fit) # regression diagnostics

  # Evaluate homoscedasticity
  # non-constant error variance test
  # No need to check this in glm Ref:https://online.stat.psu.edu/stat504/node/216/)

  name_file_plot <- paste0(method, "_Regression_diagnostics_plot_",
                           param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(2, 2))

  # plot studentized residuals vs. fitted values
  suppressWarnings(car::spreadLevelPlot(fit))

  # distribution of studentized residuals
  sresid <- MASS::studres(fit)
  graphics::hist(sresid, freq = FALSE, main = "Distribution of Studentized Residuals")
  xfit <- seq(min(sresid), max(sresid), length = 40)
  yfit <- stats::dnorm(xfit)
  graphics::lines(xfit, yfit)
  # Evaluate Nonlinearity component + residual plot
  if (!interaction) {
    car::crPlots(fit)
  }
  car::influencePlot(fit, main = "Influence Plot", sub = "Circle size is proportial to Cook's Distance")
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()


  name_file_plot <- paste0(method, "_Residuals_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  plot_diagnostics <- ggplot2::autoplot(fit, toPdf = TRUE, file = name_file_plot)
  grDevices::dev.off()

  results <- list(
    autocorr_error_test = autocorr_error_test,
    outlier_test = outlier_test,
    anova_table = anova_table,
    fit_diagnostics = fit_diagnostics
  )
  return(results)
}

#######################################################################
#' Function to find the keyword for survreg distribution
#' @param text distribution
#' @return the keyword - the name of distribution
#' @examples
#' find_survreg_distribution("weibull")
#' @export
#' @details
#' For surveg method, find the distribution
find_survreg_distribution <- function(text) {
  if (is.null(text)) {
    stop("Error - text can not be null")
  }else{
    if (is.na(text))
      stop("Error - text can not be null or NA")
  }
  text <- trimws(toupper(text))
  keyword <- NULL
  if (text == "EXPONENTIAL" | text == "EXPO") {
    keyword <- "exponential"
  }
  if (text == "WEIBULL") {
    keyword <- "weibull"
  }
  if (text == "GAUSSIAN") {
    keyword <- "gaussian"
  }
  if (text == "LOGGAUSSIAN" | text == "LOG GAUSSIAN") {
    keyword <- "loggaussian"
  }
  if (text == "RAYLEIGH") {
    keyword <- "rayleigh"
  }
  if (text == "LOGISTIC") {
    keyword <- "logistic"
  }
  if (text == "LOGNORMAL" | text == "LOG NORMAL") {
    keyword <- "lognormal"
  }
  if (text == "LOG LOGISTIC" | text == "LOGLOGISTIC") {
    keyword <- "loglogistic"
  }
  if (is.null(keyword)) {
    stop("Error - Survreg - family of distribution not matching  ")
  }
  return(keyword)
}
#######################################################################
#' Plotting and return the residuals after survival model
#' @param param_to_be_estimated  parameter to be estimated
#' @param indep_var independent variable
#' @param covariates  covariates
#' @param fit  fit object from survreg method
#' @return plot and the residuals
#' @examples
#' \donttest{
#' data_for_survival <- survival::lung
#' surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
#' info_distribution = "weibull",covariates = c("ph.ecog"), "time")
#' plot_return_residual_survival("status", "sex",
#' covariates = c("ph.ecog"),surv_estimated$fit)
#' }
#' @export
plot_return_residual_survival <- function(param_to_be_estimated, indep_var,
                                          covariates, fit) {
  if (!("survreg" %in% class(fit)))
    stop("Error- Fit object should be of type survreg")
  # checking if parameter to be estimated is NULL or NA
  check_list = list(param_to_be_estimated,indep_var)
  checks = sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop( "Error - some of the required parameters are NULL or NA")

  name_file_plot <- paste0("Survival_residuals_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = c(4, 4, 2, 2))
  residuals_response <- stats::residuals(fit, type = "response")
  residuals_deviance <- stats::residuals(fit, type = "deviance")
  residuals_working <- stats::residuals(fit, type = "working")
  residuals_ldshape <- stats::residuals(fit, type = "ldshape")
  residuals_ldresp <- stats::residuals(fit, type = "ldresp")
  residuals_ldcase <- stats::residuals(fit, type = "ldcase")

  plot(residuals_response, type = "p", main = "Response", ylab = "Residuals", lwd = 2)
  plot(residuals_deviance, type = "p", main = "Deviance", ylab = "Residuals", lwd = 2)
  nos <- ceiling(length(covariates) / 2)
  graphics::par(mfrow = c(2, nos))

  residuals_matrix <- stats::residuals(fit, type = "matrix")
  residuals_dfbeta <- stats::residuals(fit, type = "dfbeta")
  residuals_dfbetas <- stats::residuals(fit, type = "dfbetas")

  for (i in 1:length(covariates))
    plot(residuals_matrix[, i], type = "p", main = "Matrix", ylab = "Residuals", lwd = 2)
  for (i in 1:length(covariates))
    plot(residuals_dfbeta[, i], type = "p", main = "Dfbeta", ylab = "Residuals", lwd = 2)
  for (i in 1:length(covariates))
    plot(residuals_dfbetas[, i], type = "p", main = "Dfbetas", ylab = "Residuals", lwd = 2)
  on.exit(graphics::par(oldpar))
  grDevices::dev.off()

  results_residuals <- structure(list(
    Response_residual = residuals_response,
    Deviance_residual = residuals_deviance,
    Working_residual = residuals_working,
    Ldshape_residual = residuals_ldshape,
    Ldresp_residual = residuals_ldresp,
    Ldcase_residual = residuals_ldcase,
    Matrix_residual = residuals_matrix,
    Dfbeta_residual = residuals_dfbeta,
    Dfbetas_residual = residuals_dfbetas
  ))
}
#######################################################################
#' Plot the predicted survival curves for covariates keeping the others fixed
#' @param param_to_be_estimated   parameter to be estimated
#' @param indep_var  variable for which the levels have to be identified
#' @param covariates the  covariates
#' @param dataset  the dataset where these variables contain
#' @param fit the fit result survreg
#' @param timevar_survival  time variable from the dataset
#' @return plot
#' @examples
#' \donttest{
#' data_for_survival <- survival::lung
#' surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
#' info_distribution = "weibull",covariates = c("ph.ecog"),"time")
#' plot_prediction_parametric_survival("status", "sex",
#' covariates = c("ph.ecog"),data_for_survival, surv_estimated$fit, "time")
#' }
#' @export
plot_prediction_parametric_survival <- function(param_to_be_estimated, indep_var, covariates, dataset,
                                                fit, timevar_survival) {
  if (!("survreg" %in% class(fit)))
    stop("Error- Fit object should be of type survreg")

  check_list = list(param_to_be_estimated,indep_var,timevar_survival)
  checks = sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop( "Error - some of the required parameters are NULL or NA")

  # dataset should not be null
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  if (is.na(covariates)) {
    no_var <- 1
  }else{
    no_var <- 1 + length(covariates)
  }
  name_file_plot <- paste0("Survival_covariates_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  for (i in 1:no_var) {
    if (i == 1) {
      var <- indep_var
      other_fixed <- covariates
    }else{
      var <- covariates[i - 1]
      other_fixed <- c(indep_var, covariates[-(i - 1)])
    }
    categorical <- list()
    if (!is.na(other_fixed)) {
      for (j in 1:length(other_fixed)) {
        variable <- dataset[[other_fixed[j]]]
        result <- suppressWarnings(as.numeric(levels(factor(variable))))
        if (sum(is.na(result)) == 0) catego <- TRUE else catego <- FALSE
        categorical <- c(categorical, catego)
      }
    }
    categorical <- unlist(categorical)
    newdata <- as.data.frame(create_new_dataset(var, other_fixed, dataset, categorical))
    pct <- 1:98 / 100   # The 100th percentile of predicted survival is at +infinity
    prediction_value <- stats::predict(fit, newdata = newdata, type = "quantile",
                                       p = pct, se = TRUE)
    indep <- dataset[[var]]
    names <- var
    result <- suppressWarnings(as.numeric(levels(factor(indep))))
    if (sum(is.na(result)) == 0) {
      indep_lvl <- result
    } else {
      indep_lvl <- unique(as.numeric(as.factor(indep)))
    }
    for (m in 1:length(indep_lvl)) {
      graphics::matplot(cbind(prediction_value$fit[m, ], prediction_value$fit[m, ] +
                    2 * prediction_value$se.fit[m, ],
                    prediction_value$fit[m, ] - 2 * prediction_value$se.fit[m, ]) / 30.5,
                    1 - pct, xlab = timevar_survival, ylab = "Survival", type = "l",
                    lty = c(1, 2, 2), col = 3)
      graphics::legend("topright", legend = c(paste(names, "=", indep_lvl[m],
              sep = ""), "upper ci", "lower ci"), lty = c(1, 2, 2), lwd = 2, col = 3)
    }

  }
  grDevices::dev.off()
}
#######################################################################
#' create new dataset while keeping
#' cox regression results and returned coefficients
#' @param var   variable for which the levels have to be identified
#' usually indep variable
#' @param covar the other covariates
#' @param dataset  the dataset where these variables contain
#' @param categorical  are these variables categorical? True of false
#' @return new data frame
#' @examples
#' dataset <- survival::lung
#' new = create_new_dataset("status", c("age"), dataset, c(FALSE))
#' @export
create_new_dataset <- function(var, covar, dataset, categorical) {
  # checking if independent variable  is NULL or NA
  if (is.null(var)) {
    stop("Error -  variable can not be null")
  }else{
    if (is.na(var))
      stop("Error -   variable can not be NA")
  }
  #Error if data set is null
  if (is.null(dataset)) {
    stop("Error -  dataset can not be null")
  }
  #Error if categorical variable is null
  if (is.null(categorical)) {
    categorical <- c(FALSE)
  }
  check <- IPDFileCheck::check_column_exists(var, dataset)
  if (check != 0) {
    stop("Column of variable not in the dataset")
  }
  indep <- dataset[[var]]
  names <- var
  result <- suppressWarnings(as.numeric(levels(factor(indep))))
  if (sum(is.na(result)) == 0) {
    # means it is numerical
    indep_lvl <- result
  } else {
    indep_lvl <- levels(factor(indep))
  }
  #if the var is independent variable loop over the covariates else include
  # independent variable as fixed variable. If categorical take the minimum value,
  # else the mean for the fixed varaibles
  df <- as.data.frame(indep_lvl)
  if (sum(is.na(covar)) == 0) {
    for (i in 1:length(covar)) {
      this_var <- covar[i]
      check <- IPDFileCheck::check_column_exists(this_var, dataset)
      if (check != 0) {
        stop("Column of covariate not in the dataset")
      }
      fixed <- dataset[[covar[i]]]
      if (categorical[i]) {
        fixed_covar <- min(fixed[!is.na(fixed)])
      } else {
        fixed_covar <- mean(fixed, na.rm = TRUE)
      }
      df <- cbind(df, fixed_covar)
      names <- c(names, covar[i])
    }
  }
  colnames(df) <- names
  return(df)
}

#######################################################################
#' Form expression to use with lm()
#' @param param_to_be_estimated  parameter of interest
#' @param indep_var the independent variable (column name in data file)
#' @param covariates list of covariates
#' @param interaction boolean value to indicate interaction
#' in the case of linear regression, false by default
#' @return the formula for lm
#' @examples
#' formula <- form_expression_lm("gre", indep_var = "gpa", covariates = NA,
#' interaction = FALSE)
#' @export
#' @details
#' This function helps to create the expression for liner regression model
#' it takes care of covariates and interaction
form_expression_lm <- function(param_to_be_estimated, indep_var, covariates,
                               interaction) {
  check_list = list(param_to_be_estimated,indep_var)
  checks = sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop( "Error - some of the required parameters are NULL or NA")

  if (length(covariates) == 0 | sum(is.na(covariates)) == length(covariates)) {
    # no need to check for interaction
    fmla <- paste("lm(", param_to_be_estimated, " ~ ", indep_var, ",
                  data = dataset )", sep = "")
    short_fmla <- paste(" ~ ", indep_var, sep = "")
  } else {
    expre <- paste(covariates[1], sep = "")
    i <- 2
    while (i <= length(covariates)) {
      this <- paste(covariates[i], sep = "")
      if (interaction) {
        expre <- paste(expre, this, sep = " * ")
      } else {
        expre <- paste(expre, this, sep = " + ")
      }
      i <- i + 1
    }
    fmla <- paste("lm(", param_to_be_estimated, " ~ ", expre, " + ",
                  indep_var, ", data = dataset )", sep = "")
    short_fmla <- paste(" ~ ", expre, " + ", indep_var, sep = "")
  }
  expressions <- list(formula = fmla, short_formula = short_fmla)
  return(expressions)
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
#' @keywords internal
#' @examples
#'\donttest{
#' datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
#' mydata <- read.csv(datafile)
#' results_logit <- use_linear_regression("admit",dataset = mydata,
#' indep_var = "gre",covariates = NA, interaction = FALSE)
#' do_diagnostic_linear_regression("lm", results_logit$fit, results_logit$fit$call,
#' "admit", mydata, "gre", covariates = NA , interaction= FALSE)
#' }
#' @importFrom gvlma gvlma
#' @export
do_diagnostic_linear_regression <- function(method, fit, expression_recreated, param_to_be_estimated,
                                            dataset, indep_var, covariates, interaction) {
  if (is.null(method)) {
    stop("Error - method should not be NULL")
  }else{
    if (method != "lm") stop("Error - method should be lm")
  }
  if (!("lm" %in% class(fit)))
    stop("Error- Fit object should be of type lm")

  check_list = list(param_to_be_estimated, indep_var, expression_recreated,
                    interaction)
  checks = sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop( "Error - some of the required parameters are NULL or NA")


  # checking if dataset is is NULL
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  # Fit R2, AIC and BIC
  Adj_R2 <- summary(fit)$adj.r.squared
  AIC <- stats::AIC(fit)
  BIC <- stats::BIC(fit)
  fit_diagnostics <- data.frame(cbind(Adj_R2, AIC, BIC))
  colnames(fit_diagnostics) <- c("Adj_R2", "AIC", "BIC")

  #  autocorrelation test
  autocorr_error_test <- lmtest::dwtest(fit)

  #  outlier test
  outlier_test <- car::outlierTest(fit)

  # Anova table
  anova_table <- stats::anova(fit) # anova table

  # influence fit
  influence_fit <- stats::influence(fit) # regression diagnostics

  # Evaluate homoscedasticity
  # non-constant error variance test
  non_constant_error_test <- car::ncvTest(fit)

  # Model fit assumption test
  model_fit_assumptions_test <- summary(gvlma::gvlma(fit))


  name_file_plot <- paste0(method, "_Regression_diagnostics_plot_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  oldpar <- graphics::par(no.readonly = TRUE)
  graphics::par(mfrow = c(2, 2))

  # plot studentized residuals vs. fitted values
  suppressWarnings(car::spreadLevelPlot(fit))

  # distribution of studentized residuals
  sresid <- MASS::studres(fit)
  graphics::hist(sresid, freq = FALSE, main = "Distribution of Studentized Residuals")
  xfit <- seq(min(sresid), max(sresid), length = 40)
  yfit <- stats::dnorm(xfit)
  graphics::lines(xfit, yfit)

  # Evaluate Nonlinearity component + residual plot
  if (interaction == FALSE) {
    car::crPlots(fit)
  }
  car::influencePlot(fit, main = "Influence Plot", sub = "Circle size is proportial to Cook's Distance")
  oldpar <- graphics::par(no.readonly = TRUE)
   grDevices::dev.off()


  name_file_plot <- paste0(method, "_Residuals_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  plot_diagnostics <- ggplot2::autoplot(fit, toPdf = TRUE, file = name_file_plot)
  grDevices::dev.off()

  results <- list(
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
#' @keywords internal
#' @examples
#' \donttest{
#' datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
#' mydata <- read.csv(datafile)
#'  results_logit <- use_linear_regression("admit", dataset = mydata,
#'  indep_var = "gre",covariates = NA,interaction = FALSE)
#'  predict = prediction_regression("lm",results_logit$fit,
#'  results_logit$fit$call, "admit",covariates = NA,"gre", FALSE )
#'}
#' @importFrom effects predictorEffects
#' @export
prediction_regression <- function(method, fit, expression_recreated,
                                  param_to_be_estimated, indep_var,
                                  covariates, interaction) {

  if (is.null(method)) {
    stop("Error - method should not be NULL")
  }else{
    if (method != "lm" & method != "glm") stop("Error - method should be lm or glm")
  }
  if (!("lm" %in% class(fit))  & !("glm" %in% class(fit)))
    stop("Error- Fit object should be of type lm or glm")
  # checking if expression_created is NULL

  check_list = list(param_to_be_estimated, indep_var, expression_recreated,
                    interaction)
  checks = sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop( "Error - some of the required parameters are NULL or NA")

  predictor_effect <- effects::predictorEffects(fit)
  leng <- length(predictor_effect)
  prediction_all <- c()
  this_names <- c()
  for (i in 1:leng) {
    this <- (predictor_effect[[i]]$model.matrix[, ])
    prediction_all <- append(prediction_all, list(this))
    if (i == leng) {
      this_names <- append(this_names, paste(indep_var, sep = ""))
    } else {
      this_names <- append(this_names, paste(covariates[i], sep = ""))
    }
  }
  prediction_all <- append(prediction_all, list(predictor_effect[[1]]$fit))
  names(prediction_all) <- c(this_names, param_to_be_estimated)


  name_file_plot <- paste0(method, "_Prediction_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  plot_prediction <- graphics::plot(predictor_effect, lines = list(multiline = TRUE))
  grDevices::dev.off()

  results <- list(
    prediction_all = prediction_all
  )
}
#######################################################################
#' Form expression to use with mixed models
#' @param param_to_be_estimated column name of dependent variable
#' @param dataset a dataframe
#' @param fix_eff names of variables as fixed effect predictors
#' @param fix_eff_interact_vars, if interaction -true
#' @param random_intercept_vars, names of variables for random intercept
#' @param nested_intercept_vars_pairs, those of the random intercept variables
#' with nested effect
#' @param cross_intercept_vars, those of the random intercept variables
#'  with crossed effect
#' @param uncorrel_slope_intercept_pairs, variables with correlated intercepts
#' @param random_slope_intercept_pairs, random slopes intercept pairs -
#'  this is a list of paired variables
#' @param family, family of distribution for non gaussian distribution of
#' predicted variable
#' @param link, link function for the variance
#' @return result regression result with plot if success and -1, if failure
#' @examples
#' \donttest{
#' datafile <- system.file("extdata", "data_linear_mixed_model.csv",
#' package = "packDAMipd")
#' dt = utils::read.csv(datafile, header = TRUE)
#' formula <- form_expression_mixed_model("extro",
#'   dataset = dt,
#'   fix_eff = c("open", "agree", "social"),
#'   fix_eff_interact_vars = NULL,
#'   random_intercept_vars = c("school", "class"),
#'   nested_intercept_vars_pairs = list(c("school", "class")),
#'   cross_intercept_vars = NULL,
#'   uncorrel_slope_intercept_pairs = NULL,
#'   random_slope_intercept_pairs = NULL, family = "binomial", link = NA
#' )
#' }
#' @export
#' @details
#' Form the expression for mixed model
#'
form_expression_mixed_model <- function(param_to_be_estimated, dataset, fix_eff,
                                        fix_eff_interact_vars, random_intercept_vars,
                                        nested_intercept_vars_pairs, cross_intercept_vars,
                                        uncorrel_slope_intercept_pairs, random_slope_intercept_pairs,
                                        family, link) {

  # checking if parameter to be estimated is NULL or NA
  check_list = list(param_to_be_estimated, random_intercept_vars)
  checks = sapply(check_list, check_null_na)
  if (sum(checks) != 0)
    stop( "Error - some of the required parameters are NULL or NA")
  # checking if dataset is is NULL
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  if (!is.na(family)) {
      this_dist <- find_glm_distribution(family)
      if (!is.na(link)) {
        link <- check_link_glm(this_dist, link)
        family_def <- paste(this_dist, "(link = ", link, ")", sep = "")
      } else {
        family_def <- paste(this_dist, sep = "")
      }
  }
  if (is.null(nested_intercept_vars_pairs) & is.null(cross_intercept_vars)) {
    stop("Error - the random intercept variables have to be either nested
         or cross, both can not be null")
  }
  expression <- paste(param_to_be_estimated, "~")
  if (is.null(fix_eff)) {
    fix_eff <- 1
    expression <- paste(expression, fix_eff, "+")
  } else {
    no_interaction_var <- setdiff(fix_eff, fix_eff_interact_vars)
    i <- 1
    all_vars <- ""
    while (i <= length(no_interaction_var)) {
      this <- paste(no_interaction_var[i], "+ ")
      all_vars <- paste(all_vars, this)
      i <- i + 1
    }
    expression <- paste(expression, all_vars, sep = "")
    i <- 1
    all_interact_vars <- ""
    while (i <= length(fix_eff_interact_vars)) {
      if (i == length(fix_eff_interact_vars)) {
        this <- paste(fix_eff_interact_vars[i], "+ ")
      } else {
        this <- paste(fix_eff_interact_vars[i], "* ")
      }
      all_interact_vars <- paste(all_interact_vars, this)
      i <- i + 1
    }
    expression <- paste(expression, all_interact_vars, sep = "")
  }
  if (!is.null(random_intercept_vars)) {
    if (!is.null(cross_intercept_vars)) {
      char <- "+ "
      i <- 1
      all_interact_vars <- " "
      while (i <= length(cross_intercept_vars)) {
        if (!is.null(uncorrel_slope_intercept_pairs)) {
          index_uncorrel <- match(cross_intercept_vars[i],
                                  unlist(uncorrel_slope_intercept_pairs))
        } else {
          index_uncorrel <- NA
        }
        if (length(index_uncorrel) != 0 & sum(is.na(index_uncorrel)) > 0) {
          uncorrel_ind <- "1 + "
        } else {
          uncorrel_ind <- "0 + "
        }
        index <- match(cross_intercept_vars[i],
                       unlist(random_slope_intercept_pairs))
        if (length(index) == 0 | is.na(index) | index == 0) {
          slope <- 1
        } else {
          slope <- random_slope_intercept_pairs[index / 2][[1]][index - 1]
        }
        if (i == length(cross_intercept_vars)) {
          this <- paste("(", uncorrel_ind, slope, "|", cross_intercept_vars[i], ") ")
        } else {
          this <- paste("(", uncorrel_ind, slope, "|",
                        cross_intercept_vars[i], ")", char)
        }
        all_interact_vars <- paste(all_interact_vars, this)
        i <- i + 1
      }
      expression <- paste(expression, all_interact_vars, sep = "")
    }
    if (!is.null(nested_intercept_vars_pairs)) {
      char <- "+ "
      i <- 1
      if (is.null(cross_intercept_vars)) {
        all_interact_vars <- " "
      } else {
        all_interact_vars <- "+"
      }
      while (i <= length(nested_intercept_vars_pairs)) {
        if (!is.null(uncorrel_slope_intercept_pairs)) {
          index_uncorrel <- match(unlist(nested_intercept_vars_pairs[i]),
                                  unlist(uncorrel_slope_intercept_pairs))
        } else {
          index_uncorrel <- NA
        }
        if (length(index_uncorrel) != 0 & sum(is.na(index_uncorrel)) > 0) {
          uncorrel_ind <- "1 + "
        } else {
          uncorrel_ind <- "0 + "
        }
        index <- match(nested_intercept_vars_pairs[i], unlist(random_slope_intercept_pairs))
        if (length(index) == 0 | is.na(index) | index == 0) {
          slope <- 1
        } else {
          slope <- random_slope_intercept_pairs[index / 2][[1]][index - 1]
        }
        if (i == length(nested_intercept_vars_pairs)) {
          this <- paste("(", uncorrel_ind, slope, "|",
                        nested_intercept_vars_pairs[i][[1]][1], ":",
                        nested_intercept_vars_pairs[i][[1]][2], ")")
        } else {
          this <- paste("(", uncorrel_ind, slope, "|",
                        nested_intercept_vars_pairs[i][[1]][1], ":",
                        nested_intercept_vars_pairs[i][[1]][2], ")", char)
        }
        all_interact_vars <- paste(all_interact_vars, this)
        i <- i + 1
      }
      expression <- paste(expression, all_interact_vars, sep = "")
    }
  }
  if (!is.na(family)) {
    expression <- paste(expression, ", family = ", family_def, sep = "")
  }
  if (sys.nframe() > 1) {
    caller <- deparse(sys.calls()[[sys.nframe() - 1]])
    this_caller <- caller[1]
    index <- stringr::str_locate(this_caller, "\\(")[1]
    method_name <- substr(this_caller, 1, index[1] - 1)
    if (method_name == "use_linear_mixed_model") {
      expression <- paste("lme4::lmer(", expression, ", data = dataset)", sep = "")
    }
    if (method_name == "use_generalised_linear_mixed_model") {
      expression <- paste("lme4::glmer(", expression, ", data = dataset, control = lme4::glmerControl(optimizer = \"bobyqa\"), nAGQ = 10)", sep = "")
    }
  } else {
    expression <- paste("lme4::lmer(", expression, ", data = dataset)", sep = "")
  }
  return(expression)
}
################################################################
#' Plotting survival function for all covariates using survfit
#' @param param_to_be_estimated  parameter to be estimated
#' @param dataset param describing the methods
#' @param indep_var independent variable
#' @param covariates  covariates
#' @param timevar_survival  time variable for survival analysis
#' @return plot and the survival function values
#' @examples
#' \donttest{
#'  data_for_survival <- survival::lung
#'  plot_return_survival_curve(param_to_be_estimated = "status",
#'  dataset = data_for_survival,indep_var = "sex",covariates = c("ph.ecog"),
#'  timevar_survival = "time")
#'  }
#' @export
plot_return_survival_curve <- function(param_to_be_estimated, dataset, indep_var,
                                       covariates, timevar_survival) {

   # checking if parameter to be estimated is NULL or NA
  if (is.null(param_to_be_estimated)) {
    stop("Error - parameter to be estimated can not be null")
  }else{
    if (is.na(param_to_be_estimated))
      stop("Error - parameter to be estimated can not be NA")
  }
  # checking if independent variable  is NULL or NA
  if (is.null(indep_var)) {
    stop("Error - independent variable can not be null")
  }else{
    if (is.na(indep_var))
      stop("Error -  independent variable can not be NA")
  }
  # dataset need not be null
  if (is.null(dataset))
    stop("Error - dataset can not be null")


  # checking if time variable is NULL or NA
  if (is.null(timevar_survival)) {
    stop("Error - time variable can not be null")
  }else{
    if (is.na(timevar_survival)) {
      stop("For survival analysis, please provide the variable to use as time ")
    } else {
      surv_object <- paste("survival::Surv(", timevar_survival, ",", param_to_be_estimated, ")",
                           sep = ""
      )
    }
  }

  covariates_list <- make_string_covariates(covariates)

  if (sum(is.na(covariates_list)) != 0) {
    expression_recreated_forsurfit <- paste0("survival::survfit", "(", surv_object, " ~ ", indep_var, ", ",
                                             "data = dataset)",
                                             sep = ""
    )
  } else {
    expression_recreated_forsurfit <- paste0("survival::survfit", "(", surv_object, " ~ ", covariates_list, " + ",
                                             indep_var, ", ", "data = dataset)",
                                             sep = ""
    )
  }
  # estimate survival function using survival::survfit
  fit_survfit <- eval(parse(text = expression_recreated_forsurfit))


  name_file_plot <- paste0("Survival_function_default_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  graphics::plot(fit_survfit)
  grDevices::dev.off()

  summary <- summary(fit_survfit)
  fac <- levels(factor(summary$strata))
  total_no <- length(fac)
  survival_fn <- list()
  names_surv <- list()


  name_file_plot <- paste0("Survival_function_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  if (sum(is.na(covariates)) != 0) {
    subplots <- 1
    no_levels <- length(levels(factor(dataset[[indep_var]])))
  } else {
    no_levels <- 4
    subplots <- ceiling(total_no / no_levels)
  }
  no_overlay <- ceiling(total_no / subplots)
  for (i in 1:subplots) {
    colours <- legend_here <- list()
    for (j in 1:no_levels) {
      k <- i * j
      times <- summary$time[which(summary$strata == fac[k])]
      survs <- summary$surv[which(summary$strata == fac[k])]
      survs_lb <- summary$lower[which(summary$strata == fac[k])]
      survs_ub <- summary$upper[which(summary$strata == fac[k])]
      if (j == 1) {
        graphics::plot(times, survs,
                       type = "l", xlab = "Time", ylab = "Survival function", col = k,
                       lwd = 3, ylim = c(0, 1)
        )
      } else {
        graphics::lines(times, survs,
                        type = "l", xlab = "Time", ylab = "Survival function", col = k,
                        lwd = 3, ylim = c(0, 1)
        )
      }
      graphics::lines(times, survs_lb, type = "l", col = k, lwd = 1, lty = 2)
      graphics::lines(times, survs_ub, type = "l", col = k, lwd = 1, lty = 2)
      this <- data.frame(time = times, survfun = survs, upper_ci = survs_lb, lower_ci = survs_ub)
      colnames(this) <- c(
        paste("times for", fac[k]), paste("survival for", fac[k]),
        paste("upper ci for", fac[k]), paste("lower ci for", fac[k])
      )
      survival_fn <- append(survival_fn, this)
      legend_here <- append(legend_here, c(paste(fac[k], "Mean"), paste(fac[k], "CIs")))
      this_col <- c(k, k)
      colours <- append(colours, this_col)
      line_types <- rep(c(1, 2), no_levels)
    }
    graphics::legend("topright",
                     legend = legend_here, bty = "n", col = unlist(colours),
                     lty = line_types, cex = 0.5
    )
  }
  grDevices::dev.off()
  return(survival_fn)
}

#######################################################################
#' Plotting and return the residuals after cox proportional hazard model
#' @param param_to_be_estimated  parameter to be estimated
#' @param indep_var independent variable
#' @param covariates  covariates
#' @param fit  fit object from coxph method
#' @param dataset data used for cox ph model
#' @return plot and the residuals
#' @examples
#' \donttest{
#'   data_for_survival <- survival::lung
#'   surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
#'   covariates = c("ph.ecog"), "time")
#'   plot_return_residual_cox("status", "sex", covariates = c("ph.ecog"),
#'   surv_estimated$fit,data_for_survival )
#'   }
#' @export
#' @importFrom stats residuals
plot_return_residual_cox <- function(param_to_be_estimated, indep_var, covariates, fit, dataset) {
  if (!("coxph" %in% class(fit)))
    stop("Error- Fit object should be of type  coxph")

  # checking if parameter to be estimated is NULL or NA
  if (is.null(param_to_be_estimated)) {
    stop("Error - parameter to be estimated can not be null")
  }else{
    if (is.na(param_to_be_estimated))
      stop("Error - parameter to be estimated can not be NA")
  }
  # checking if independent variable  is NULL or NA
  if (is.null(indep_var)) {
    stop("Error - independent variable can not be null")
  }else{
    if (is.na(indep_var))
      stop("Error -  independent variable can not be NA")
  }


  name_file_plot <- paste0("Cox_residuals_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)

  residuals_martingale <- stats::residuals(fit, type = "martingale")
  cox_snell <- residuals_martingale - dataset[[param_to_be_estimated]]
  residuals_deviance <- stats::residuals(fit, type = "deviance")
  residuals_score <- stats::residuals(fit, type = "score")
  residuals_schoenfeld <- stats::residuals(fit, type = "schoenfeld")
  residuals_dfbeta <- stats::residuals(fit, type = "dfbeta")
  residuals_dfbetas <- stats::residuals(fit, type = "dfbetas")
  residuals_scaledsch <- stats::residuals(fit, type = "scaledsch")
  residuals_partial <- stats::residuals(fit, type = "partial")
  if (is.na(covariates)) {
    oldpar <- graphics::par(no.readonly = TRUE)
    graphics::par(mar = c(4, 4, 2, 2))

    plot(residuals_martingale, type = "p", main = "Martingale", ylab = "Residuals", lwd = 2)
    plot(residuals_deviance, type = "p", main = "Deviance", ylab = "Residuals", lwd = 2)
    plot(residuals_score, type = "p", main = paste("Score_", indep_var,
                                               sep = ""), ylab = "Residuals", lwd = 2)
    plot(residuals_schoenfeld, type = "p", main = paste("Schoenfeld_", indep_var,
                                                    sep = ""), ylab = "Residuals", lwd = 2)
    plot(residuals_dfbeta, type = "p", main = paste("Dfbeta_", indep_var,
                                                     sep = ""), ylab = "Residuals", lwd = 2)
    plot(residuals_dfbetas, type = "p", main = paste("Dfbetas_", indep_var,
                                                 sep = ""), ylab = "Residuals", lwd = 2)
    plot(residuals_scaledsch, type = "p", main = paste("Scaledsch_", indep_var,
                                                   sep = ""), ylab = "Residuals", lwd = 2)
    plot(residuals_partial, type = "p", main = paste("Partial_", indep_var,
                                                      sep = ""), ylab = "Residuals", lwd = 2)
    on.exit(graphics::par(oldpar))
    grDevices::dev.off()
  }else{
    total <- 1 + length(covariates)
    oldpar <- graphics::par(no.readonly = TRUE)
    graphics::par(mar = c(4, 4, 2, 2))
    nos <- ceiling(total / 2)
    graphics::par(mfrow = c(2, nos))
    plot(residuals_martingale, type = "p", main = "Martingale", ylab = "Residuals", lwd = 2)
    plot(residuals_deviance, type = "p", main = "Deviance", ylab = "Residuals", lwd = 2)
    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name  <-  covariates[i - 1]
      plot(residuals_score[, i], type = "p", main = paste("Score_", name,
                                                      sep = ""), ylab = "Residuals", lwd = 2)
    }
    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name <-  covariates[i - 1]
      plot(residuals_schoenfeld[, i], type = "p", main = paste("Schoenfeld_",
                              name, sep = ""), ylab = "Residuals", lwd = 2)
    }
    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name <- covariates[i - 1]
      plot(residuals_dfbeta[, i], type = "p", main = paste("Dfbeta_", name,
                            sep = ""), ylab = "Residuals", lwd = 2)
    }

    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name <-  covariates[i - 1]
      plot(residuals_dfbetas[, i], type = "p", main = paste("Dfbetas_", name,
                           sep = ""), ylab = "Residuals", lwd = 2)
    }
    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name <- covariates[i - 1]
      plot(residuals_scaledsch[, i], type = "p", main = paste("Scaledsch_", name,
                            sep = ""), ylab = "Residuals", lwd = 2)
    }

    for (i in 1:total) {
      if (i == 1) name <- indep_var else  name <-  covariates[i - 1]
      plot(residuals_partial[, i], type = "p", main = paste("Partial_", name,
                               sep = ""), ylab = "Residuals", lwd = 2)
    }
    on.exit(graphics::par(oldpar))
    grDevices::dev.off()
 }
  results_residuals <- structure(list(
    Coxsnell_residual = cox_snell,
    Martingale_residual = residuals_martingale,
    Deviance_residual = residuals_deviance,
    Score_residual = residuals_score,
    Schoenfeld_residual = residuals_schoenfeld,
    Dfbeta_residual = residuals_dfbeta,
    Dfbetas_residual = residuals_dfbetas,
    Scaledsch_residual = residuals_scaledsch,
    Partial_residual = residuals_partial
  ))
}
#######################################################################
#' Predict risk/hazard function for  cox ph regression
#' @param coxfit  cox regression fit result
#' @param dataset param describing the methods
#' @param param_to_be_estimated  parameter to be estimated
#' @param covariates  covariates
#' @param indep_var independent variable
#' @param timevar_survival time variable
#' @return plot and the survival function values
#' @examples
#' \donttest{
#' data_for_survival <- survival::lung
#' surv_estimated <- use_coxph_survival("status", data_for_survival,
#' "sex",covariates = c("ph.ecog"), "time")
#' predict_coxph(surv_estimated$fit,data_for_survival, "status","sex",
#' covariates = c("ph.ecog"), "time")
#' }
#' @export
#' @details
#' "risk" option for "type" returns the hazard ratio relative to mean e.g given below
#' For lung data with
#' data_for_survival <- survival::lung
#' fit <- use_coxph_survival("status", data_for_survival, "sex",
#' covariates = c("ph.ecog"), "time")
#' coeffit = fit$coefficients
#' r1234 <- exp(coeffit("sex")lung$sex+ coeffit("ph.ecog")lung$ph.ecog)
#' rMean <- exp(sum(coef(fit) * fit$means, na.rm=TRUE))
#' rr <- r1234/rMean
predict_coxph <- function(coxfit, dataset, param_to_be_estimated, covariates, indep_var, timevar_survival) {

  if (!("coxph" %in% class(coxfit)))
    stop("Error- Fit object should be of type  coxph")
  # dataset need not be null
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  # checking if parameter to be estimated is NULL or NA
  if (is.null(param_to_be_estimated)) {
    stop("Error - parameter to be estimated can not be null")
  }else{
    if (is.na(param_to_be_estimated))
      stop("Error - parameter to be estimated can not be NA")
  }
  # checking if independent variable  is NULL or NA
  if (is.null(indep_var)) {
    stop("Error - independent variable can not be null")
  }else{
    if (is.na(indep_var))
      stop("Error -  independent variable can not be NA")
  }

  # checking if time variable is NULL or NA
  if (is.null(timevar_survival)) {
    stop("Error - time variable can not be null")
  }else{
    if (is.na(timevar_survival))
      stop("Error -  time variable can not be NA")
  }
  baseline_hazard <- survival::basehaz(coxfit)


  name_file_plot <- paste0("Cox_Cumhazard_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  graphics::plot(baseline_hazard[, 2], baseline_hazard[, 1], main = "Cumulative hazard", xlab = "Time", ylab = "H0(t)")
  grDevices::dev.off()

  risk_relative_mean <- stats::predict(coxfit, type = "risk")
  times_used <- dataset[[timevar_survival]]
  times <- times_used
  relative_risk_mean <- risk_relative_mean

  predict_results <- structure(list(
    cumulative_hazard = baseline_hazard,
    times = times,
    relative_risk_to_mean = relative_risk_mean
  ))
  return(predict_results)
}
#######################################################################
#' Plotting survival function for all covariates  calculated from
#' cox regression results and returned coefficients
#' @param coxfit  cox regression fit result
#' @param dataset param describing the methods
#' @param param_to_be_estimated  parameter to be estimated
#' @param covariates  covariates
#' @param indep_var independent variable
#' @return plot and the survival function values
#' @examples
#' \donttest{
#'  data_for_survival <- survival::lung
#'  surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
#'  covariates = c("ph.ecog"), "time")
#'  plot_survival_cox_covariates(surv_estimated$fit,data_for_survival,
#'  "status", covariates = c("ph.ecog"), "sex")
#'  }
#' @export
plot_survival_cox_covariates <- function(coxfit, dataset, param_to_be_estimated, covariates, indep_var) {


  if (!("coxph" %in% class(coxfit)))
    stop("Error- Fit object should be of type  coxph")
  # dataset need not be null
  if (is.null(dataset))
    stop("Error - dataset can not be null")

  # checking if parameter to be estimated is NULL or NA
  if (is.null(param_to_be_estimated)) {
    stop("Error - parameter to be estimated can not be null")
  }else{
    if (is.na(param_to_be_estimated))
      stop("Error - parameter to be estimated can not be NA")
  }
  # checking if independent variable  is NULL or NA
  if (is.null(indep_var)) {
    stop("Error - independent variable can not be null")
  }else{
    if (is.na(indep_var))
      stop("Error -  independent variable can not be NA")
  }

  baseline_haz <- survival::basehaz(coxfit)


  name_file_plot <- paste0("Cox_Survivial function_", param_to_be_estimated, "_", indep_var, ".pdf", sep = "")
  grDevices::pdf(name_file_plot)
  if (sum(is.na(covariates)) == 0) {
    total <- length(covariates) + 1
  } else {
    total <- 1
  }
  for (j in 1:total) {
    if (total == 1) {
      var <- names(coxfit$coefficients)[1]
      var2 <- indep_var
      fixed <- NA
    } else {
      if (j == 1) {
        var <- names(coxfit$coefficients)[1]
        var2 <- indep_var
        fixed <- covariates
      } else {
        var <- covariates[j - 1]
        var2 <- var
        fixed <- c(names(coxfit$coefficients)[1], covariates[-(j - 1)])
      }
    }
    result <- suppressWarnings(as.numeric(levels(factor(dataset[[var2]]))))
    if (sum(is.na(result)) == 0) {
      lvls <- result
    } else {
      lvls <- unique(as.numeric(as.factor(dataset[[var2]])))
    }
    terms <- 0
    lbterms <- 0
    ubterms <- 0
    if (sum(is.na(fixed) == 0)) {
      for (m in 1:length(fixed)) {
        this_fixed <- fixed[m]
        value <- mean(dataset[[this_fixed]], na.rm = TRUE)
        this_coeff <- coxfit$coef[[this_fixed]]
        this_term <- this_coeff * value
        terms <- terms + this_term
        se_coeff <- summary(coxfit)$coefficients[this_fixed, 3]
        LB <- this_coeff - stats::qnorm(1 - 0.95 / 2) * se_coeff
        UB <- this_coeff + stats::qnorm(1 - 0.95 / 2) * se_coeff
        lbterms <- lbterms + LB * value
        ubterms <- ubterms + UB * value
      }
    }
    for (i in seq_len(length(lvls))) {
      this_coeff <- coxfit$coef[[var]]
      this_coeff_all <- terms + this_coeff * lvls[i]
      exp_coef <- exp(this_coeff_all)

      se_coeff <- summary(coxfit)$coefficients[var, 3]
      LB <- this_coeff - stats::qnorm(1 - 0.95 / 2) * se_coeff
      UB <- this_coeff + stats::qnorm(1 - 0.95 / 2) * se_coeff
      this_lb <- lbterms + LB * lvls[i]
      this_ub <- ubterms + UB * lvls[i]

      exp_coef_ci1 <- exp(this_lb)
      exp_coef_ci2 <- exp(this_ub)
      if (i == 1) {
        graphics::plot(baseline_haz[, 2], exp(-baseline_haz[, 1]) ^ (exp_coef),
                       col = i, main = paste("Survival curves", "-", var2), xlab = "Time", ylab = "S(t|X)"
        )
      } else {
        graphics::plot(baseline_haz[, 2], exp(-baseline_haz[, 1]) ^ (exp_coef), col = i, xaxt = "n", yaxt = "n", ann = FALSE)
      }
      graphics::lines(baseline_haz[, 2], exp(-baseline_haz[, 1]) ^ (exp_coef_ci1), col = i, lty = 2)
      graphics::lines(baseline_haz[, 2], exp(-baseline_haz[, 1]) ^ (exp_coef_ci2), col = i, lty = 2)
      graphics::par(new = TRUE)
    }
    graphics::legend("topright", legend = paste(var2, "-", lvls), lty = rep(length(lvls), 1), col = seq(1:length(lvls)))
  }
  grDevices::dev.off()
}
