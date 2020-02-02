
library(gmodels)
library(lmtest)
library(survival)
library(eha)
library(nlme)
library(coda)
library(lattice)
library(R2WinBUGS)
library(MASS)
library(foreign)
library(plyr)
library(packDAMipd)
lreg.or <- function(x)
{
  lreg.coeffs <- coef(summary(x))
  lci <- exp(lreg.coeffs[ , 1] - (qnorm(0.975,  0,  1)) * lreg.coeffs[ , 2])
  or <- exp(lreg.coeffs[ , 1])
  uci <- exp(lreg.coeffs[ , 1] + (qnorm(0.975,  0,  1)) * lreg.coeffs[ , 2])
  lreg.or <- cbind(lci,  or,  uci)
  lreg.or
}

patient_m <- read.table('/Users/SheejaMK/Desktop/repos/rita3/patient_m1.txt',  header  =  TRUE,  sep  =  "\t",  quote  =  "\"",  dec  =  ".",  fill  =  TRUE,  na.strings  =  c(""), as.is  =  1:149)
#Get the risk score from the riskscore file
riskscore <- read.table('/Users/SheejaMK/Desktop/repos/rita3/riskscore.txt',  header  =  TRUE,  sep  =  "\t",  quote = "\"",  dec  = ",",  fill  =  TRUE,  na.strings  =  c(""), as.is = 1:4)
names(riskscore)[4] <- "riskgroup"
patient_m <- merge(patient_m, riskscore[, c("studyno", "risk", "riskgroup")], by = "studyno", all.x  = T, all.y  =  F)



patient_m$agegrp0 <-  factor(patient_m$agegrp0)
patient_m$diabmell <- factor(patient_m$diabmell)
patient_m$prevmi <- factor(patient_m$prevmi)
patient_m$smoker <- factor(patient_m$smoker)

patient_m$stdepres <- factor(patient_m$stdepres)
patient_m$agrade <- factor(patient_m$agrade)
patient_m$sex <- factor(patient_m$sex)
patient_m$leftbund <- factor(patient_m$leftbund)
patient_m$treat <- factor(patient_m$treat)
patient_m$riskgroup <- factor(patient_m$riskgroup)

profile <- patient_m[, c(which(colnames(patient_m)  ==  "studyno" ),
                         which(colnames(patient_m)  ==  "sex"):which(colnames(patient_m)  ==  "agegrp0"))]

rm(riskscore)
# added by Sheeja after Tom send the correct codes
riskccfinal_logit <- glm(ccindex ~ as.numeric(as.character(agegrp0)) + agrade + treat, binomial, patient_m)  ##### TECNICAL REPORT - TABLE 2 #####
#riskccfinal_logit <- glm(ccindex ~ treat + as.numeric(as.character(agegrp0)) + agrade , binomial, patient_m)
summary(riskccfinal_logit)      ##### TECNICAL REPORT - TABLE 2 #####
lreg.or(riskccfinal_logit)      ##### TECNICAL REPORT - TABLE 2 #####
VCM_riskccfinal_logit <- vcov(riskccfinal_logit)
Chol_riskccfinal_logit <- chol(VCM_riskccfinal_logit)
Chol_riskccfinal_logit

results_eq1<- packDAMipd::get_parameter_estimated_regression("ccindex", patient_m, method = "logistic",
                                               indep_var = "as.numeric(as.character(agegrp0))",info_get_method = NA,
                                               info_distribution = "binomial",
                                               covariates= c("agrade", "treat"),
                                               strategycol= NA, strategyname= NA,
                                               timevar_survival= NA, interaction= NA)
results_eq1$param_estimated[1]


