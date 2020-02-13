mydata <- foreign::read.dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta")
results_sureg <- use_seemingly_unrelated_regression("read", "math", dataset = mydata,
indep_var = "female", covariates1 = c("as.numeric(ses)", "socst"),
covariates2 = c("as.numeric(ses)", "science"),interaction1 = FALSE,  interaction2 = FALSE)
