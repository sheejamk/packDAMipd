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

mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
debug(use_mixed_effect_model)
results_logit <- use_mixed_effect_model("gre", dataset = mydata,  indep_var = "gpa",
                                        covariates = NA, random_effect = NA)
