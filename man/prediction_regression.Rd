% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/help_parameter_estimation_functions.R
\name{prediction_regression}
\alias{prediction_regression}
\title{Do the prediction for regression}
\usage{
prediction_regression(
  method,
  fit,
  expression_recreated,
  param_to_be_estimated,
  indep_var,
  covariates,
  interaction
)
}
\arguments{
\item{method, }{param describing the methods, expects lm}

\item{fit}{results of the regression fit}

\item{expression_recreated, }{the expression recreated for calling lm}

\item{param_to_be_estimated, }{parameter of interest}

\item{indep_var}{the independent variable (column name in data file)}

\item{covariates}{list of covariates - calculations to be done before
passing}

\item{interaction}{boolean value to indicate interaction in the case of
linear regression, false by default}
}
\value{
the results of the regression analysis
}
\description{
Do the prediction for regression
}
\examples{
\donttest{
datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
mydata <- read.csv(datafile)
results_logit <- use_linear_regression("admit", dataset = mydata,
indep_var = "gre",covariates = NA,interaction = FALSE)
predict = prediction_regression("lm",results_logit$fit,
results_logit$fit$call, "admit",covariates = NA,"gre", FALSE )
}
}
\keyword{internal}
