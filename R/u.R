mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
mydata$rank <- factor(mydata$rank)

results_logit <- use_logistic_rgression("admit", dataset= mydata,
indep_var = "gre", family ="binomial", covariates_list = NA, naaction = "stats::na.omit", link = NA)
results_logit$param_estimated



results_logit$plot_prediction
