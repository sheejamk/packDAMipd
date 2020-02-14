# Regresssion outcome will contain
#
# 1. Results of model fit as is given by that method in R
# 2. summary of fit
# 3. Regression coefficients with CI
# 4. Variance covariance matrix of coefficents
# 5. Cholesky decomosition matrix  (from vcov matrix V = T'T and correlated x  = y + Tz where y is mean coeff, z and norm(0,1))
# 7.  Model fit assumptions test (contain 8 and 9)
# 8. Test for Autocorrelation of error terms
# 9.  Diagnosis of model fit using resiual plots
# 10. R2, AIC, BIC values
# 11. Model prediction
# 12. Extraction of distribution parameters
##############################################
mydata <- foreign::read.dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta")
debug(use_seemingly_unrelated_regression)
results_sureg <- use_seemingly_unrelated_regression("read", "math", dataset = mydata,
indep_var = "female", covariates1 = c("as.numeric(ses)", "socst"),
covariates2 = c("as.numeric(ses)", "science"),interaction1 = FALSE,  interaction2 = FALSE)

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
results_lm <- use_linear_regression("gre", dataset = mydata,
indep_var = "gpa", covariates = NA, interaction = FALSE)


y <- rnorm(2000)*4 - 4
dd = qqnorm(y)
cv = qqline(y, col = 2,lwd=2,lty=2)

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
results_logit <- use_mixed_effect_model("gre", dataset = mydata,  indep_var = "gpa", covariates = NA, random_effect = "rank")
