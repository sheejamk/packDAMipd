# # Regresssion outcome will contain
# #
# # 1. Results of model fit as is given by that method in R
# # 2. summary of fit
# # 3. Regression coefficients with CI
# # 4. Variance covariance matrix of coefficents
# # 5. Cholesky decomosition matrix  (from vcov matrix V = T'T and correlated x  = y + Tz where y is mean coeff, z and norm(0,1))
# # 7.  Model fit assumptions test (contain 8 and 9)
# # 8. Test for Autocorrelation of error terms
# # 9.  Diagnosis of model fit using resiual plots
# # 10. R2, AIC, BIC values
# # 11. Model prediction
# # 12. Extraction of distribution parameters
# ##############################################
# dataset <- read.table("http://bayes.acs.unt.edu:8083/BayesContent/class/Jon/R_SC/Module9/lmm.data.txt",
#                  header = TRUE, sep = ",", na.strings = "NA", dec = ".", strip.white = TRUE)
# result <- use_linear_mixed_model("extro", dataset = dataset, fix_eff = c("open" , "agree", "social"),
#                                        fix_eff_interact_vars = NULL,
#                                        random_intercept_vars = c("school", "class"),
#                                        nested_intercept_vars_pairs = list(c("school", "class")),
#                                        cross_intercept_vars = NULL,
#                                        uncorrel_slope_intercept_pairs = NULL,
#                                        random_slope_intercept_pairs = NULL)
#
# result$fit
# # Linear mixed model fit by REML ['lmerMod']
# # Formula: extro ~ open + agree + social + (1 + 1 | school:class)
# # Data: dataset
# # REML criterion at convergence: 3590.348
# # Random effects:
# #   Groups       Name        Std.Dev.
# # school:class (Intercept) 9.4766
# # Residual                 0.9841
# # Number of obs: 1200, groups:  school:class, 24
# # Fixed Effects:
# #   (Intercept)         open        agree       social
# # 60.2400412    0.0061011   -0.0077439    0.0005477
#
# lme4::lmer(extro ~ open +  agree +  social +   ( 1 | school : class ), data = dataset)
# # Linear mixed model fit by REML ['lmerMod']
# # Formula: extro ~ open + agree + social + (1 | school:class)
# # Data: dataset
# # REML criterion at convergence: 3590.348
# # Random effects:
# #   Groups       Name        Std.Dev.
# # school:class (Intercept) 9.4766
# # Residual                 0.9841
# # Number of obs: 1200, groups:  school:class, 24
# # Fixed Effects:
# #   (Intercept)         open        agree       social
# # 60.2400412    0.0061011   -0.0077439    0.0005477
#
# dataset = lme4::sleepstudy
# result <- use_linear_mixed_model("Reaction", dataset = dataset, fix_eff = c("Days"),
#                                        fix_eff_interact_vars = NULL,
#                                        random_intercept_vars = c("Subject"),
#                                        nested_intercept_vars_pairs = NULL,
#                                        cross_intercept_vars = c("Subject"),
#                                        uncorrel_slope_intercept_pairs = NULL,
#                                        random_slope_intercept_pairs = list(c("Days", "Subject")))
# result$fit
# # Formula: Reaction ~ Days + (1 + Days | Subject)
# # Data: dataset
# # REML criterion at convergence: 1743.628
# # Random effects:
# #   Groups   Name        Std.Dev. Corr
# # Subject  (Intercept) 24.737
# # Days         5.923   0.07
# # Residual             25.592
# # Number of obs: 180, groups:  Subject, 18
# # Fixed Effects:
# #   (Intercept)         Days
# # 251.41        10.47
# lme4::lmer(Reaction ~ Days +   ( 1 +  Days | Subject ), data = dataset)
# # Linear mixed model fit by REML ['lmerMod']
# # Formula: Reaction ~ Days + (1 + Days | Subject)
# # Data: dataset
# # REML criterion at convergence: 1743.628
# # Random effects:
# #   Groups   Name        Std.Dev. Corr
# # Subject  (Intercept) 24.737
# # Days         5.923   0.07
# # Residual             25.592
# # Number of obs: 180, groups:  Subject, 18
# # Fixed Effects:
# #   (Intercept)         Days
# # 251.41        10.47
# result <- use_linear_mixed_model("Reaction", dataset = dataset, fix_eff = c("Days"),
#                                  fix_eff_interact_vars = NULL,
#                                  random_intercept_vars = c("Subject"),
#                                  nested_intercept_vars_pairs = NULL,
#                                  cross_intercept_vars = c("Subject"),
#                                  uncorrel_slope_intercept_pairs = list(c("Days", "Subject")),
#                                  random_slope_intercept_pairs = list(c("Days", "Subject")))
#
# result$fit
# # Linear mixed model fit by REML ['lmerMod']
# # Formula: Reaction ~ Days + (0 + Days | Subject)
# # Data: dataset
# # REML criterion at convergence: 1766.525
# # Random effects:
# #   Groups   Name Std.Dev.
# # Subject  Days  7.26
# # Residual      29.02
# # Number of obs: 180, groups:  Subject, 18
# # Fixed Effects:
# #   (Intercept)         Days
# # 251.41        10.47
# lme4::lmer(Reaction ~ Days +   ( 0 +  Days | Subject ), data = dataset)
# #
# # Linear mixed model fit by REML ['lmerMod']
# # Formula: Reaction ~ Days + (0 + Days | Subject)
# # Data: dataset
# # REML criterion at convergence: 1766.525
# # Random effects:
# #   Groups   Name Std.Dev.
# # Subject  Days  7.26
# # Residual      29.02
# # Number of obs: 180, groups:  Subject, 18
# # Fixed Effects:
# #   (Intercept)         Days
# # 251.41        10.47
#
#
# # source data:https://stats.stackexchange.com/questions/174203/predict-function-for-lmer-mixed-effects-models
# require(gsheet)
# data <- read.csv(text =
#                    gsheet2text('https://docs.google.com/spreadsheets/d/1QgtDcGJebyfW7TJsB8n6rAmsyAnlz1xkT3RuPFICTdk/edit?usp=sharing',
#                                format ='csv'))
# head(data)
# result <- use_linear_mixed_model("Recall", dataset = data, fix_eff = c("Caffeine"),
#                                  fix_eff_interact_vars = NULL,
#                                  random_intercept_vars = c("Subject", "Time"),
#                                  nested_intercept_vars_pairs = list(c("Subject", "Time")),
#                                  cross_intercept_vars = c("Subject"),
#                                  uncorrel_slope_intercept_pairs =  NULL,
#                                  random_slope_intercept_pairs = NULL)
# result$fit
# # Formula: Recall ~ Caffeine + (1 + 1 | Subject) + (1 + 1 | Subject:Time)
# # Data: dataset
# # REML criterion at convergence: 444.5198
# # Random effects:
# #   Groups       Name        Std.Dev.
# # Subject:Time (Intercept) 23.64
# # Subject      (Intercept) 49.59
# # Residual                 25.98
# # Number of obs: 45, groups:  Subject:Time, 15; Subject, 5
# # Fixed Effects:
# #   (Intercept)     Caffeine
# # 61.9195       0.2116
# lme4::lmer(Recall ~ (1|Subject/Time) + Caffeine, data = data)
# # Formula: Recall ~ (1 | Subject/Time) + Caffeine
# # Data: data
# # REML criterion at convergence: 444.5198
# # Random effects:
# #   Groups       Name        Std.Dev.
# # Time:Subject (Intercept) 23.64
# # Subject      (Intercept) 49.59
# # Residual                 25.98
# # Number of obs: 45, groups:  Time:Subject, 15; Subject, 5
# # Fixed Effects:
# #   (Intercept)     Caffeine
# # 61.9195       0.2116
#
#
# hdp <- read.csv("https://stats.idre.ucla.edu/stat/data/hdp.csv")
# hdp <- within(hdp, {
#   Married <- factor(Married, levels = 0:1, labels = c("no", "yes"))
#   DID <- factor(DID)
#   HID <- factor(HID)
# })
# m <- lme4::glmer(remission ~ IL6 + CRP + CancerStage + LengthofStay + Experience +
#              (1 | DID), data = hdp, family = binomial, control = lme4::glmerControl(optimizer = "bobyqa"),
#            nAGQ = 10)
# m
# debug(use_generalised_linear_mixed_model)
# result <- use_generalised_linear_mixed_model("remission", dataset = hdp, fix_eff =
#                                                c("IL6" , "CRP", "CancerStage","LengthofStay","Experience"),
#                                              fix_eff_interact_vars = NULL,
#                                              random_intercept_vars = c("DID"),
#                                              nested_intercept_vars_pairs = NULL,
#                                              cross_intercept_vars = c("DID"),
#                                              uncorrel_slope_intercept_pairs = NULL,
#                                              random_slope_intercept_pairs = NULL, family = "binomial", link = NA)
# result$variance_covariance_coeff_random
#
# result <-
# use_generalised_linear_mixed_model("remission", dataset = hdp,
# fix_eff = c("IL6" , "CRP", "CancerStage","LengthofStay","Experience"),
# fix_eff_interact_vars = NULL, random_intercept_vars = c("DID"),
# nested_intercept_vars_pairs = NULL, cross_intercept_vars = c("DID"),
# uncorrel_slope_intercept_pairs = NULL, random_slope_intercept_pairs = NULL,
# family = "binomial", link = NA)
#
