
###############################################################################
test_that("get parameter using survival analysis", {
  data_for_survival <- survival::aml
  data_for_survival <- na.omit(data_for_survival)
  # Error - parameter to be estimated is not found
  expect_error(use_survival_analysis(NULL, data_for_survival,
                                     "x",
                                     info_get_method = "parametric", info_distribution
                                     = "weibull", covariates = NA, "time"
  ))
  # Error - dataset is not found
  expect_error(use_survival_analysis("status", NULL,"x",
                                     info_get_method = "parametric", info_distribution
                                     = "weibull", covariates = NA, "time"))
  # Error - independent variable is not found
  expect_error(use_survival_analysis("status", data_for_survival,
                                     NA,
                                     info_get_method = "parametric", info_distribution
                                     = "weibull", covariates = NA, "time"
  ))
  # Error - method is missing
  expect_error(use_survival_analysis("status", data_for_survival, "x", NA,
                                     info_distribution = "weibull", covariates = NA,
                                     "time"
  ))
})
###############################################################################
test_that("get parameter using parametric regression survival analysis", {
  data_for_survival <- survival::lung
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated <- use_parametric_survival("status", data_for_survival, "sex",
                                            info_distribution = "weibull",
                                            covariates = c("ph.ecog"), "time")

  # error - param to be estimated not valid
  expect_error(use_parametric_survival(NA, data_for_survival, "sex",
                              info_distribution = "weibull", covariates = NA,
                               "time"
  ))
  #error - no dataset
  expect_error(use_parametric_survival("status", NULL, "sex",
                              info_distribution = "weibull", covariates = NA,
                              "time"
  ))
  # error - no independent variable
  expect_error(use_parametric_survival("status", data_for_survival, NA,
                              info_distribution = "weibull", covariates = NA,
                              "time"
  ))
  # Error - no information on distribution
  expect_error(use_parametric_survival("status", data_for_survival, "x",
                                       NA,
                                       covariates = NA, "time"
  ))
  # Error - no information on the time variable
  expect_error(use_parametric_survival("status", data_for_survival, "x",
                info_distribution = "weibull", covariates = NA, NA
  ))
})
###############################################################################

test_that("get parameter using kaplan meier survival analysis", {
  data_for_survival <- survival::aml
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated_aml <- use_km_survival("status", data_for_survival, "x", covariates = NA, "time")
  # Error - parameter to be estimated is not found
  expect_error(use_km_survival(NA, data_for_survival, "x", covariates = NA, "time"))
  # Error - dataset not provided
  expect_error(use_km_survival("status", NULL, "x", covariates = NA, "time"))
  # Error - independent variable not given
  expect_error(use_km_survival("status", data_for_survival, NA, covariates = NA, "time"))
  # Error - time variable not given
  expect_error(use_km_survival("status", data_for_survival, "x", covariates = NA, NA))
})
###############################################################################

test_that("get parameter using FH survival analysis", {
  data_for_survival <- survival::aml
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated_aml <- use_fh_survival("status", data_for_survival, "x", covariates = NA, "time")
  # Error - parameter to be estimated is not found
  expect_error(use_fh_survival(NA, data_for_survival, "x", covariates = NA, "time"))
  # Error - dataset not provided
  expect_error(use_fh_survival("status", NULL, "x", covariates = NA, "time"))
  # Error - independent variable not given
  expect_error(use_fh_survival("status", data_for_survival, NA, covariates = NA, "time"))
  # Error - time variable not given
  expect_error(use_fh_survival("status", data_for_survival, "x", covariates = NA, NA))
})
###############################################################################

test_that("get parameter using FH2 survival analysis", {
  data_for_survival <- survival::aml
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated_aml <- use_fh2_survival("status", data_for_survival, "x", covariates = NA, "time")
  # Error - parameter to be estimated is not found
  expect_error(use_fh2_survival(NA, data_for_survival, "x", covariates = NA, "time"))
  # Error - dataset not provided
  expect_error(use_fh2_survival("status", NULL, "x", covariates = NA, "time"))
  # Error - independent variable not given
  expect_error(use_fh2_survival("status", data_for_survival, NA, covariates = NA, "time"))
  # Error - time variable not given
  expect_error(use_fh2_survival("status", data_for_survival, "x", covariates = NA, NA))
})
###############################################################################

test_that("get parameter using cox ph survival analysis", {
  data_for_survival <- survival::lung
  data_for_survival <- na.omit(data_for_survival)
  surv_estimated <- use_coxph_survival("status", data_for_survival, "sex",
    covariates = c("ph.ecog"), "time"
  )
  # Error - parameter to be estimated is not found
  expect_error(use_coxph_survival(NA, data_for_survival, "sex", covariates = NA, "time"))
  # Error - dataset not provided
  expect_error(use_coxph_survival("status", NULL, "sex", covariates = NA, "time"))
  # Error - independent variable not given
  expect_error(use_coxph_survival("status", data_for_survival, NA, covariates = NA, "time"))
  # Error - time variable not given
  expect_error(use_coxph_survival("status", data_for_survival, "sex", covariates = NA, NA))
})
