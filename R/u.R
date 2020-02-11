# mydata <- read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
# mydata$rank <- factor(mydata$rank)
#
# debug(use_logistic_rgression)
# results_logit <- use_logistic_rgression("admit", dataset= mydata,
# indep_var = "gre", family ="binomial", covariates_list = " gpa +  rank", naaction = "stats::na.omit", link = NA)
#
# glm1<- glm(admit ~ gpa* rank + gre, data =mydata, family="binomial")
# re <- predictorEffect("rank", glm1 )
# plot(re, lines=list(multiline=TRUE))
# results_logit$plot_prediction
