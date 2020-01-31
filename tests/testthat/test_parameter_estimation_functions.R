###############################################################################
context("testing assigning parameter directly")
test_that("testing add_probabilities",  {
  expect_equal(get_parameter_direct("a",0.2), 0.2)
  expect_error(get_parameter_direct("a",NULL))
})
test_that("testing reading parameter from file",  {
  file = system.file("extdata","table_param.csv", package = "packDAMipd")
  nofile = system.file("extdata","blank.csv", package = "packDAMipd")
  expect_equal(get_parameter_read("cost_IT",file),2000)
  expect_error(get_parameter_read("cost_IT",nofile))
  expect_error(get_parameter_read("a",file))
})

test_that("testing parameter using distribution read from file",  {
  file = system.file("extdata","table_param.csv", package = "packDAMipd")
  nofile = system.file("extdata","blank.csv", package = "packDAMipd")
  expect_identical(get_parameter_def_distribution("rr",file),"rr = rexp(1,rate=0.3)")
  expect_error(get_parameter_def_distribution("rr", NULL))
  expect_error(get_parameter_def_distribution("rr2", NULL))
})

test_that("get parameter from estimated regression",  {
  mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
  results_logit <- get_parameter_estimated_regression("admit", mydata,
    "logistic regression", "gre", NA,"binomial", c("gpa", "factor(rank)"))
  results_logit2 <- get_parameter_estimated_regression("admit", mydata,
    "logistic regression", "gre", NA,"binomial")
  expect_error(get_parameter_estimated_regression(NA, mydata,
               "logistic regression", "gre", NA,"binomial", c("gpa", "factor(rank)")))
  expect_error(get_parameter_estimated_regression("admit", NA,
        "logistic regression", "gre", NA,"binomial", c("gpa", "factor(rank)")))
  expect_error(get_parameter_estimated_regression("admit", mydata,
         NA, "gre", NA,"binomial", c("gpa", "factor(rank)")))
  expect_error(get_parameter_estimated_regression("admit", mydata,
      "logistic regression", NA, NA,"binomial", c("gpa", "factor(rank)")))
  expect_error(get_parameter_estimated_regression("admit", mydata,
      "logistic regression", "gre", NA,NA, c("gpa", "factor(rank)")))

})
test_that("get parameter using survival analysis",  {
  data_for_survival<-survival::aml
  surv_estimated_aml <- use_survival_analysis("status", data_for_survival,
                          "x", info_get_method="parametric", info_distribution = "weibull",
                          covariates_list=NA,"time")
  expect_error(use_survival_analysis("status", NA,
                                     "x", info_get_method="parametric", info_distribution = "weibull",
                                     covariates_list=NA,"time"))
  expect_error(use_survival_analysis("status", data_for_survival,
                                     NA, info_get_method="parametric", info_distribution = "weibull",
                                     covariates_list=NA,"time"))
  expect_error(use_survival_analysis("status", data_for_survival,
                                     "x", NA, info_distribution = "weibull",
                                     covariates_list=NA,"time"))
  expect_error(use_survival_analysis("status", data_for_survival,
                                     "x", info_get_method="parametric", NA,
                                     covariates_list=NA,"time"))
  expect_error(use_survival_analysis("status", data_for_survival,
                                     "x", info_get_method="parametric",  "weibull",
                                     covariates_list=NA,NA))
})
test_that("get parameter using parametric regression survival analysis",  {
  data_for_survival<-survival::aml
  surv_estimated_aml <- use_parametric_regression("status", data_for_survival,"x",
                        info_distribution="weibull", covariates_list=NA,"time")
  expect_error(use_parametric_regression(NA, data_for_survival,"x",
                                         info_distribution="weibull", covariates_list=NA,"time"))
  expect_error(use_parametric_regression("status", NA,"x",
                                         info_distribution="weibull", covariates_list=NA,"time"))
  expect_error(use_parametric_regression("status", data_for_survival,NA,
                                         info_distribution="weibull", covariates_list=NA,"time"))
  expect_error(use_parametric_regression("status", data_for_survival,"x",
                                         NA, covariates_list=NA,"time"))
  expect_error(use_parametric_regression("status", data_for_survival,"x",
                                         info_distribution="weibull", covariates_list=NA,NA))
})
test_that("get parameter using parametric regression survival analysis",  {
  data_for_survival<-survival::aml
  surv_estimated_aml <- use_km_survival("status", data_for_survival, "x", covariates_list=NA, "time")
  expect_error(use_km_survival(NA, data_for_survival, "x", covariates_list=NA, "time"))
  expect_error(use_km_survival("status", NA, "x", covariates_list=NA, "time"))
  expect_error(use_km_survival("status", data_for_survival, NA, covariates_list=NA, "time"))
  expect_error(use_km_survival("status", data_for_survival, "x", covariates_list=NA, NA))
})
test_that("get parameter using parametric regression survival analysis",  {
  data_for_survival<-survival::aml
  surv_estimated_aml <- use_fh_survival("status", data_for_survival, "x", covariates_list=NA, "time")
  expect_error(use_fh_survival(NA, data_for_survival, "x", covariates_list=NA, "time"))
  expect_error(use_fh_survival("status", NA, "x", covariates_list=NA, "time"))
  expect_error(use_fh_survival("status", data_for_survival, NA, covariates_list=NA, "time"))
  expect_error(use_fh_survival("status", data_for_survival, "x", covariates_list=NA, NA))
})
test_that("get parameter using parametric regression survival analysis",  {
  data_for_survival<-survival::aml
  surv_estimated_aml <- use_fh2_survival("status", data_for_survival, "x", covariates_list=NA, "time")
  expect_error(use_fh2_survival(NA, data_for_survival, "x", covariates_list=NA, "time"))
  expect_error(use_fh2_survival("status", NA, "x", covariates_list=NA, "time"))
  expect_error(use_fh2_survival("status", data_for_survival, NA, covariates_list=NA, "time"))
  expect_error(use_fh2_survival("status", data_for_survival, "x", covariates_list=NA, NA))
})
test_that("get parameter using parametric regression survival analysis",  {
  data_for_survival<-survival::aml
  surv_estimated_aml <- use_coxph_survival("status", data_for_survival, "x", covariates_list=NA, "time")
  expect_error(use_coxph_survival(NA, data_for_survival, "x", covariates_list=NA, "time"))
  expect_error(use_coxph_survival("status", NA, "x", covariates_list=NA, "time"))
  expect_error(use_coxph_survival("status", data_for_survival, NA, covariates_list=NA, "time"))
  expect_error(use_coxph_survival("status", data_for_survival, "x", covariates_list=NA, NA))
})
test_that("get parameter using parametric regression survival analysis",  {
  mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
  results_logit <- use_logistic_rgression("admit", dataset = mydata,
                      indep_var = "gre", info_distribution ="binomial", covariates_list = NA)
  expect_error(use_logistic_rgression("admit", dataset = NA,
                      indep_var = "gre", info_distribution ="binomial", covariates_list = NA))
  expect_error(use_logistic_rgression("admit", dataset = mydata,
                      indep_var = NA, info_distribution ="binomial", covariates_list = NA))
  expect_error(use_logistic_rgression("admit", dataset = mydata,
                      indep_var = "gre", info_distribution = NA, covariates_list = NA))
  expect_error(use_logistic_rgression(NA, dataset = mydata,
                      indep_var = "gre", info_distribution ="binomial", covariates_list = NA))
})
test_that("get parameter using parametric regression survival analysis",  {
  mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
  results_logit <- use_linear_rgression("gre", dataset=mydata,
                                    indep_var = "gpa", covariates = NA, interaction = FALSE)
  expect_error(use_linear_rgression(NA, dataset=mydata,
                                    indep_var = "gpa", covariates = NA, interaction = FALSE) )
  expect_error(use_linear_rgression("gre", dataset=NA,
                                    indep_var =NA, covariates = NA, interaction = FALSE) )
  expect_error(use_linear_rgression("gre", dataset=NA,indep_var = "gpa" ))
 })
test_that("get parameter using parametric regression survival analysis",  {
  mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
  results_logit <- use_mixed_effect_model("gre", dataset=mydata, indep_var = "gpa", covariates = NA)
  expect_error(use_mixed_effect_model(NA, dataset=mydata, indep_var = "gpa", covariates = NA))
  expect_error(use_mixed_effect_model("gre", dataset=NA, indep_var = "gpa", covariates = NA))
  expect_error(use_mixed_effect_model("gre", dataset=mydata, indep_var = NA, covariates = NA))
})

test_that("get parameter using parametric regression survival analysis",  {
  paramfile=system.file("extdata","LifeTable_USA_Mx_2015.csv",package = "packDAMipd")
  nofile = system.file("extdata","blank.csv", package = "packDAMipd")
  mortality <- get_mortality_from_file(paramfile, age = 10, gender = NULL)
  expect_equal(get_mortality_from_file(paramfile, age = 10, gender = NULL),0.000107)
  expect_error(get_mortality_from_file(nofile, age = 10, gender = NULL))
  expect_error(get_mortality_from_file(paramfile, age = 120, gender = NULL))
  expect_error(get_mortality_from_file(paramfile, age = NULL))
})
