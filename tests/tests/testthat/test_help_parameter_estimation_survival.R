###############################################################################
context("testing  form expression for glm")
test_that("testing form expression for glm", {
  formula <- form_expression_glm("admit",
    indep_var = "gre", family = "binomial",
    covariates = c("gpa", "rank"), interaction = FALSE, naaction = "na.omit",
    link = NA)
  # Error - parameter to be estimated should not be NULL or NA
  expect_error(form_expression_glm(NA, "gre", "binomial",covariates = NA,
                      interaction = FALSE, naaction = "na.omit", link = NA))
  # Error - independent variable should not be NULL or NA
  expect_error(form_expression_glm("admit", NA, "binomial",covariates = NA,
                                   interaction = FALSE, naaction = "na.omit", link = NA))
  # Error - distribution should not be NULL or NA
  expect_error(form_expression_glm("admit", "gre", NA,covariates = NA,
                                   interaction = FALSE, naaction = "na.omit", link = NA))
  # Error - interaction should not be NULL or NA
  expect_error(form_expression_glm("admit", "gre", "binomial",covariates = NA,
                                   interaction = NA, naaction = "na.omit", link = NA))
  # Error - interaction should not be NULL or NA
  expect_error(form_expression_glm("admit", "gre", "binomial",covariates = NA,
                                   interaction = NULL, naaction = "na.omit", link = NA))
})
###############################################################################
context("testing  getting family of distribution for glm")
test_that("testing  getting family of distribution for glm", {
  expect_equal(find_glm_distribution("normal"), "gaussian")
  expect_equal(find_glm_distribution("poisson"), "poisson")
  expect_error(find_glm_distribution("logical"))
  # Error the parameter can not be null
  expect_error(find_glm_distribution(NULL))
})
###############################################################################
context("testing  getting link function for the family of distribution for glm")
test_that("testing  getting link function for the family of distribution for glm", {
  expect_equal(check_link_glm("gaussian", "log"), "log")
  expect_equal(check_link_glm("poisson", "identity"), "identity")
  expect_error(check_link_glm("logical", NULL))
  expect_error(check_link_glm("poisson", "probit"))
  # Error the parameter can not be Na or NULL
  expect_error(check_link_glm(NULL, NULL))
  # Error the parameter can not be Na or NULL
  expect_error(check_link_glm("saussian", NULL))
})
###############################################################################
context("testing  diagnosis for glm fit")
test_that("testing  diagnosis for glm fit", {
  datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  results_logit <- use_generalised_linear_model("admit",dataset = mydata,
                              indep_var = "gre", family = "binomial", covariates = NA,
                              interaction = FALSE,naaction = "na.omit", link = NA)
  #Error method should be glm
  expect_error(do_diagnostic_glm(NULL, results_logit$fit, expression_recreated, param_to_be_estimated,
                                 mydata, "gre", covariates, interaction))
  #Error "fit" is not glm fit object
  expect_error(do_diagnostic_glm("glm", "fit", expression_recreated, param_to_be_estimated,
                                 mydata, "gre", covariates, interaction))
  # expression created can not be null
  expect_error(do_diagnostic_glm("glm", results_logit$fit, NULL, param_to_be_estimated,
                                 mydata, "gre", covariates, interaction))

  # param_to_be_estimated can not be null
  expect_error(do_diagnostic_glm("glm", results_logit$fit, results_logit$fit$call, NULL,
                                 mydata, "gre", covariates, interaction))

  # dataset can not be null
  expect_error(do_diagnostic_glm("glm", results_logit$fit, results_logit$fit$call, "gre",
                                 NULL, "gre", covariates, interaction))
  # indep_var can not be null
  expect_error(do_diagnostic_glm("glm", results_logit$fit, results_logit$fit$call, "gre",
                                 mydata, NULL, covariates, interaction))

  # interaction can not be null
  expect_error(do_diagnostic_glm("glm", results_logit$fit, results_logit$fit$call, "gre",
                                 mydata, "gre", covariates, NULL))
})
###############################################################################
context("testing find distribution for survreg")
test_that("testing  diagnosis for glm fit", {
  expect_equal(find_survreg_distribution("weibull"), "weibull")
  expect_equal(find_survreg_distribution("expo"), "exponential")
  # Error - text can not be NULL or NA
  expect_error(find_survreg_distribution(NULL))
  expect_error(find_survreg_distribution(NA))
  # Error - distribution not recognised
  expect_error(find_survreg_distribution("NULL"))
})
###############################################################################
context("testing find distribution for survreg")
test_that("testing  diagnosis for glm fit", {
  data_for_survival <- survival::lung
  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
           info_distribution = "weibull",covariates = c("ph.ecog"), "time"
  )
  plot_return_residual_survival("status", "sex",
                    covariates = c("ph.ecog"),surv_estimated$fit)
  #Error - parameter to be estimated can not be NA
  expect_error(plot_return_residual_survival(NA, "sex",
                                covariates = c("ph.ecog"),surv_estimated$fit))
  #Error - independent variable  can not be NULL
  expect_error(plot_return_residual_survival("status", NULL,
                                covariates = c("ph.ecog"),surv_estimated$fit))
  #Error - fit should be of survreg object
  expect_error(plot_return_residual_survival("status", "sex",
                            covariates = c("ph.ecog"),"fit"))
})
###############################################################################
context("testing find distribution for survreg")
test_that("testing  diagnosis for glm fit", {
  data_for_survival <- survival::lung
  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
                           info_distribution = "weibull",covariates = c("ph.ecog"),"time")
  plot_prediction_parametric_survival("status", "sex",
            covariates = c("ph.ecog"),data_for_survival, surv_estimated$fit, "time")
  #Error - parameter to be estimated can not be NA
  expect_error(plot_prediction_parametric_survival(NA, "sex",
          covariates = c("ph.ecog"),data_for_survival, surv_estimated$fit, "time"))
  #Error - independent variable  can not be NULL
  expect_error(plot_prediction_parametric_survival("status", NULL,
            covariates = c("ph.ecog"),data_for_survival, surv_estimated$fit, "time"))
  #Error - dataset should not be NULL
  expect_error(plot_prediction_parametric_survival("status", "sex",
              covariates = c("ph.ecog"),NULL, surv_estimated$fit, "time"))
  #Error - fit object should be of survreg type
  expect_error(plot_prediction_parametric_survival("status", "sex",
          covariates = c("ph.ecog"),data_for_survival, "fit", "time"))
  #Error - time variable can not be NULL
  expect_error(plot_prediction_parametric_survival("status", "sex",
        covariates = c("ph.ecog"),data_for_survival, surv_estimated$fit,NULL))
})
###############################################################################

context("testing creating a new dataset based on given one")
test_that("testing creating a new dataset based on given one", {
  dataset <- survival::lung
  new = create_new_dataset("status", c("age"), dataset, c(FALSE))
  #Error -var null or Na
  expect_error(create_new_dataset(NULL, c("age"), dataset, c(FALSE)))
  expect_error(create_new_dataset(NA, c("age"), dataset, c(FALSE)))
  # Error- dataset  null
  expect_error(create_new_dataset("status", c("age"),NULL, c(FALSE)))
  # Error- column not in dataset
  expect_error(create_new_dataset("status", c("age1"),dataset, c(FALSE)))
  expect_error(create_new_dataset("status1", c("age"),dataset, c(FALSE)))
  df <- data.frame(status = c(1,2),
                  age = c(62.44737, 62.44737))
  expect_equal(create_new_dataset("status", c("age"), dataset, c(FALSE)), df, tol = 1e-3)
})
###############################################################################
context("testing creating expression for linear regression ")
test_that("testing creating expression for linear regression", {
  formula <- form_expression_lm("gre", indep_var = "gpa", covariates = NA,
                                interaction = FALSE)
  #Error -parameter to be esimated is NULL
  expect_error(form_expression_lm(NULL, indep_var = "gpa", covariates = NA,
                                  interaction = FALSE))
  # Error- independent variable  null
  expect_error(form_expression_lm("gre", indep_var = NULL, covariates = NA,
                                  interaction = FALSE))
})
###############################################################################
context("testing  diagnosis for lm fit")
test_that("testing  diagnosis for lm fit", {
  datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  results_logit <- use_linear_regression("admit",dataset = mydata,
                            indep_var = "gre",covariates = NA, interaction = FALSE)
  #Error method should be glm
  expect_error(do_diagnostic_linear_regression(NULL, results_logit$fit, expression_recreated,
                         param_to_be_estimated, mydata, "gre", covariates, interaction))
  #Error "fit" is not lm fit object
  expect_error(do_diagnostic_linear_regression("lm", "fit", expression_recreated,
                  param_to_be_estimated, mydata, "gre", covariates, interaction))
  # expression created can not be null
  expect_error(do_diagnostic_linear_regression("lm", results_logit$fit, NULL,
                      param_to_be_estimated, mydata, "gre", covariates, interaction))

  # param_to_be_estimated can not be null
  expect_error(do_diagnostic_linear_regression("lm", results_logit$fit, results_logit$fit$call, NULL,
                                 mydata, "gre", covariates, interaction))

  # dataset can not be null
  expect_error(do_diagnostic_linear_regression("lm", results_logit$fit, results_logit$fit$call, "gre",
                                 NULL, "gre", covariates, interaction))
  # indep_var can not be null
  expect_error(do_diagnostic_linear_regression("lm", results_logit$fit, results_logit$fit$call, "gre",
                                 mydata, NULL, covariates, interaction))

  # interaction can not be null
  expect_error(do_diagnostic_linear_regression("lm", results_logit$fit, results_logit$fit$call, "gre",
                                 mydata, "gre", covariates, NULL))
})
###############################################################################
context("testing predicting regression")
test_that("testing predicting regression", {
  datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  results_logit <- use_linear_regression("admit",dataset = mydata,
                                         indep_var = "gre",covariates = NA,
                                         interaction = FALSE)
  predict = prediction_regression("lm",results_logit$fit, results_logit$fit$call, "admit",
                                  covariates = NA,"gre", FALSE )
  #Error method should be glm or lm
  expect_error(prediction_regression(NULL,results_logit$fit, results_logit$fit$call, "admit",
                                      covariates = NA,"gre", FALSE ))
  expect_error(prediction_regression("fit",results_logit$fit, results_logit$fit$call, "admit",
                                     covariates = NA,"gre", FALSE ))

  #Error "fit" is not lm fit object
  expect_error(prediction_regression("lm","fit", results_logit$fit$call, "admit",
                                     covariates = NA,"gre", FALSE ))
  # expression created can not be null
  expect_error(prediction_regression("lm",results_logit$fit, NULL, "admit",
                                     covariates = NA,"gre", FALSE ))
  # param_to_be_estimated can not be null
  expect_error(prediction_regression("lm",results_logit$fit, results_logit$fit$call, NULL,
                                     covariates = NA,"gre", FALSE ))
  # indep_var can not be null
  expect_error(prediction_regression("lm",results_logit$fit, results_logit$fit$call, "admit",
                                     covariates = NA,NULL, FALSE ))

  # interaction can not be null
  expect_error(prediction_regression("lm",results_logit$fit, results_logit$fit$call, "admit",
                                     covariates = NA,"gre", NULL ))

})
###############################################################################
context("testing forming expression for mixed model")
test_that("testing forming expression for mixed model", {
  datafile = system.file("extdata", "binary.csv", package = "packDAMipd")
  mydata <- read.csv(datafile)
  formula <- form_expression_mixed_model("extro",
    dataset = mydata,
    fix_eff = c("open", "agree", "social"),
    fix_eff_interact_vars = NULL,
    random_intercept_vars = c("school", "class"),
    nested_intercept_vars_pairs = list(c("school", "class")),
    cross_intercept_vars = NULL,
    uncorrel_slope_intercept_pairs = NULL,
    random_slope_intercept_pairs = NULL, family = "binomial", link = NA
  )

  #Error - parameter to be estimated can not be null
  expect_error(form_expression_mixed_model(NULL,dataset = mydata,
            fix_eff = c("open", "agree", "social"),fix_eff_interact_vars = NULL,
            random_intercept_vars = c("school", "class"),
            nested_intercept_vars_pairs = list(c("school", "class")),
            cross_intercept_vars = NULL, uncorrel_slope_intercept_pairs = NULL,
          random_slope_intercept_pairs = NULL, family = "binomial", link = NA
  ))
  #Error - dataset can not be null
  expect_error(form_expression_mixed_model("extro",dataset = NULL,
                                           fix_eff = c("open", "agree", "social"),fix_eff_interact_vars = NULL,
                                           random_intercept_vars = c("school", "class"),
                                           nested_intercept_vars_pairs = list(c("school", "class")),
                                           cross_intercept_vars = NULL, uncorrel_slope_intercept_pairs = NULL,
                                           random_slope_intercept_pairs = NULL, family = "binomial", link = NA
  ))

  # Fixed eff variables can not be null
  expect_error(form_expression_mixed_model("extro",dataset = mydata,
                                           fix_eff = NULL,fix_eff_interact_vars = NULL,
                                           random_intercept_vars = c("school", "class"),
                                           nested_intercept_vars_pairs = list(c("school", "class")),
                                           cross_intercept_vars = NULL, uncorrel_slope_intercept_pairs = NULL,
                                           random_slope_intercept_pairs = NULL, family = "binomial", link = NA
  ))
  # random intercept variables can not be null
  expect_error(form_expression_mixed_model("extro",dataset = mydata,
                                           fix_eff = c("open", "agree", "social"),fix_eff_interact_vars = NULL,
                                           random_intercept_vars = NULL,
                                           nested_intercept_vars_pairs = list(c("school", "class")),
                                           cross_intercept_vars = NULL, uncorrel_slope_intercept_pairs = NULL,
                                           random_slope_intercept_pairs = NULL, family = "binomial", link = NA
  ))
  # Error - family can not be NULL
  expect_error(form_expression_mixed_model("extro",dataset = mydata,
                              fix_eff = c("open", "agree", "social"),fix_eff_interact_vars = NULL,
                              random_intercept_vars = c("school", "class"),
                              nested_intercept_vars_pairs = list(c("school", "class")),
                              cross_intercept_vars = NULL, uncorrel_slope_intercept_pairs = NULL,
                              random_slope_intercept_pairs = NULL, family = NULL, link = NA
  ))
  # Error - nested intercept vars and cross intercept vars both can not be NULL
  expect_error(form_expression_mixed_model("extro",dataset = mydata,
                                           fix_eff = c("open", "agree", "social"),fix_eff_interact_vars = NULL,
                                           random_intercept_vars = c("school", "class"),
                                           nested_intercept_vars_pairs = NULL,
                                           cross_intercept_vars = NULL, uncorrel_slope_intercept_pairs = NULL,
                                           random_slope_intercept_pairs = NULL, family = "binomial", link = NA
  ))
})
###############################################################################

context("testing plotting survival function")
test_that("testingplotting survival function", {
  data_for_survival <- survival::lung
  data_for_survival <- na.omit(data_for_survival)
  plot_return_survival_curve(param_to_be_estimated = "status", dataset = data_for_survival,
                             indep_var = "sex",covariates = c("ph.ecog"), timevar_survival = "time")
  #Error - parameter to be estimated can not be NA or NULL
  expect_error(plot_return_survival_curve(param_to_be_estimated = NULL, dataset = data_for_survival,
                          indep_var = "sex",covariates = c("ph.ecog"), timevar_survival = "time"))
  expect_error(plot_return_survival_curve(param_to_be_estimated = NA, dataset = data_for_survival,
                          indep_var = "sex",covariates = c("ph.ecog"), timevar_survival = "time"))

  #Error -data set can not be NULL
  expect_error(plot_return_survival_curve(param_to_be_estimated =  "status", dataset = NULL,
                        indep_var = "sex",covariates = c("ph.ecog"), timevar_survival = "time"))

  #Error -independent variable can not be NULL or NA
  expect_error(plot_return_survival_curve(param_to_be_estimated = "status", dataset = data_for_survival,
                        indep_var = NULL,covariates = c("ph.ecog"), timevar_survival = "time"))

  expect_error(plot_return_survival_curve(param_to_be_estimated = "status", dataset = data_for_survival,
                      indep_var = NA,covariates = c("ph.ecog"), timevar_survival = "time"))

  #Error -time variable can not be NULL or NA
  expect_error(plot_return_survival_curve(param_to_be_estimated = "status", dataset = data_for_survival,
                    indep_var = "sex",covariates = c("ph.ecog"), timevar_survival = NULL))
  expect_error(plot_return_survival_curve(param_to_be_estimated = "status", dataset = data_for_survival,
                    indep_var = "sex",covariates = c("ph.ecog"), timevar_survival = NA))

})
###############################################################################
context("testing plotting residual for cox ph models")
test_that("testing plotting residual for cox ph models", {

  data_for_survival <- survival::lung
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
    covariates = c("ph.ecog"), "time")
  plot_return_residual_cox("status", "sex", covariates = c("ph.ecog"),
                           surv_estimated$fit,  data_for_survival)
  #Error - param to be estimated can not be null  or NA
  expect_error(plot_return_residual_cox(NULL, "sex", covariates = c("ph.ecog"),
                                        surv_estimated$fit, data_for_survival))
  expect_error(plot_return_residual_cox(NA, "sex", covariates = c("ph.ecog"),
                                        surv_estimated$fit, data_for_survival))

  #Error - independent variable  can not be null or na
  expect_error(plot_return_residual_cox("status", NULL, covariates = c("ph.ecog"),
                                        surv_estimated$fit, data_for_survival))
  expect_error(plot_return_residual_cox("status", NA, covariates = c("ph.ecog"),
                                        surv_estimated$fit, data_for_survival))

  # Error - fit object is not of type coxph
  expect_error(plot_return_residual_cox("status", "sex", covariates = c("ph.ecog"),
                                        "fit", data_for_survival))
})
###############################################################################
context("testing plotting residual for cox ph models")
test_that("testing plotting residual for cox ph models", {

  data_for_survival <- survival::lung
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
                                       covariates = c("ph.ecog"), "time")
  predict_coxph(surv_estimated$fit,data_for_survival, "status","sex",
                covariates = c("ph.ecog"), "time")
  #Error - fit object is not of coxph type
  expect_error(predict_coxph("fit",data_for_survival, "status","sex",
                covariates = c("ph.ecog"), "time"))
  #Error - dataset should not be NULL
  expect_error(predict_coxph(surv_estimated$fit,NULL, "status","sex",
                             covariates = c("ph.ecog"), "time"))

  #Error - param to be estimated should not be NULL or NA
  expect_error(predict_coxph(surv_estimated$fit,data_for_survival, NULL,"sex",
                             covariates = c("ph.ecog"), "time"))
  expect_error(predict_coxph(surv_estimated$fit,data_for_survival, NA,"sex",
                             covariates = c("ph.ecog"), "time"))

  #Error - independent varaible should not be NULL or NA
  expect_error(predict_coxph(surv_estimated$fit,data_for_survival, "status",NULL,
                             covariates = c("ph.ecog"), "time"))
  expect_error(predict_coxph(surv_estimated$fit,data_for_survival, "status",NA,
                             covariates = c("ph.ecog"), "time"))

  #Error -time variable should not be NULL or NA
  expect_error(predict_coxph(surv_estimated$fit,data_for_survival, "status","sex",
                             covariates = c("ph.ecog"), NULL))
  expect_error(predict_coxph(surv_estimated$fit,data_for_survival, "status","sex",
                             covariates = c("ph.ecog"), NA))

})
###############################################################################
context("testing plotting residual for cox ph models")
test_that("testing plotting residual for cox ph models", {

  data_for_survival <- survival::lung
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
                                       covariates = c("ph.ecog"), "time")
  plot_survival_cox_covariates(surv_estimated$fit,data_for_survival, "status",
                covariates = c("ph.ecog"), "sex")
  #Error - fit object is not of coxph type
  expect_error(plot_survival_cox_covariates("fit",data_for_survival, "status",
                             covariates = c("ph.ecog"), "sex"))
  #Error - dataset should not be NULL
  expect_error(plot_survival_cox_covariates(surv_estimated$fit,NULL, "status",
                                            covariates = c("ph.ecog"), "sex"))

  #Error - param to be estimated should not be NULL or NA
  expect_error(plot_survival_cox_covariates(surv_estimated$fit,data_for_survival, NULL,
                             covariates = c("ph.ecog"), "sex"))
  expect_error(predict_coxph(surv_estimated$fit,data_for_survival, NA,
                             covariates = c("ph.ecog"), "sex", NA))

  #Error - independent varaible should not be NULL or NA
  expect_error(plot_survival_cox_covariates(surv_estimated$fit,data_for_survival, "status",
                             covariates = c("ph.ecog"), NULL))
  expect_error(plot_survival_cox_covariates(surv_estimated$fit,data_for_survival, "status",
                             covariates = c("ph.ecog"), NA))


})
