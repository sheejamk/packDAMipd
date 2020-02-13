#
# mydata <- foreign::read.dta("https://stats.idre.ucla.edu/stat/stata/notes/hsb2.dta")
# results_sureg <- use_seemingly_unrelated_regression("read", "math", dataset = mydata,
#                                                     indep_var = "female", covariates1 = c("as.numeric(ses)", "socst"),
#                                                     covariates2 = c("as.numeric(ses)", "science"),interaction1 = FALSE,  interaction2 = FALSE)
#
#
# # #######################################################################
# # library(packDAMipd)
# patient_m <- load_trial_data(file = "data-raw/patient_m1.txt")
# riskscore <- load_trial_data(file = "data-raw/riskscore.txt")
# qol_long <- load_trial_data(file = "data-raw/r3form2.dta")
# names(riskscore)[4] <- "riskgroup"
# patient_m <- merge(patient_m, riskscore[, c("studyno", "risk", "riskgroup")], by = "studyno",
#                    all.x  = T, all.y  =  F)
#
# # patient_m$agegrp0 <-  factor(patient_m$agegrp0)
# # patient_m$diabmell <- factor(patient_m$diabmell)
# # patient_m$prevmi <- factor(patient_m$prevmi)
# # patient_m$smoker <- factor(patient_m$smoker)
# # patient_m$stdepres <- factor(patient_m$stdepres)
# # patient_m$agrade <- factor(patient_m$agrade)
# # patient_m$sex <- factor(patient_m$sex)
# # patient_m$leftbund <- factor(patient_m$leftbund)
# # patient_m$treat <- factor(patient_m$treat)
# # patient_m$riskgroup <- factor(patient_m$riskgroup)
#
# rm(riskscore)
#
# results_eq1 <- get_parameter_estimated_regression("ccindex", patient_m, method = "logistic",
#                                                               indep_var = "treat",info_get_method = NA,
#                                                               info_distribution = "binomial",
#                                                               covariates = c("agegrp0", "agrade"),
#                                                               strategycol = NA, strategyname = NA,
#                                                               timevar_survival = NA, interaction = FALSE, naaction = "na.omit")
#
# riskccfinal_logit <- glm(ccindex ~ as.numeric(as.character(agegrp0)) + agrade + treat, binomial, patient_m, na.action = na.omit)  ##### TECNICAL REPORT - TABLE 2 #####
#
# results_r <- glm(ccindex ~  as.numeric(as.character(agegrp0)) + agrade + treat,  binomial, patient_m, na.action = na.omit)
# results_r
# results_eq1$fit
#
# results_eq1$cholesky_decomp_matrix
# # #######################################################################
# # # in Table 2
# # # Covariate	Odds ratio*	95 % CI
# # # Treat	1.520	0.864 to 2.675
# # # Age	1.731	1.262 to 2.374
# # # Angina	1.893	1.086 to 3.299
# # # Constant**	0.010	0.005 to 0.019
# #
# # # Cholesky decomposition matrix is reported in Table A1 as
# # # 	Treat	Age	Angina	Constant
# # # Treat	0.2882	0	0	0
# # # Age	0.0009	0.1612	0	0
# # # Angina	0.0007	-0.0114	0.2833	0
# # # Constant	-0.1724	-0.2023	-0.1452	0.1416
# #
# # #######################################################################
# # patient_m$cvdmi <- "0"
# # patient_m$cvdmi[which(patient_m$mi == 1 | patient_m$cvdeath == 1)] <- "1"
# # #estimating the risk of combined endpoint during the remainder of the trial.
# # #events during index hospitalisation should be then omitted because starting time
# # #set at the time of discharge from hospital
# # patient_m1 <- patient_m[which(!(patient_m$ccindex == 1)), ]
# # dim(patient_m1)
# #
# # #all remaining patients have a date of discharge except 1
# # #we assume discharged at 80 days
# # patient_m1$datedisc <- as.Date(patient_m1$datedisc,  format = "%d%b%Y")
# # patient_m1$last2 <- as.Date(patient_m1$last2,  format = "%d%b%Y")
# # patient_m1$datedisc[is.na(patient_m1$datedisc)] <- patient_m1$last2[is.na(patient_m1$datedisc)] - 1
# # patient_m1$datmi <- as.Date(patient_m1$datmi,  format = "%d%b%Y")
# #
# # patient_m1$last3 <- as.Date(NA,  format = "%Y-%m-%d")
# #
# # patient_m1$last3[patient_m1$mi == 0] <- patient_m1$last2[patient_m1$mi == 0]
# # patient_m1$last3[patient_m1$mi == 1] <- patient_m1$datmi[patient_m1$mi == 1]
# #
# # patient_m1$follow <- (patient_m1$last3 - patient_m1$datedisc)/365.25
# # patient_m1$follow[patient_m1$follow  == 0] <- 1/365.25
# #
# # patient_m1$follow <- as.numeric(patient_m1$follow)
# # patient_m1$cvdmi <- as.numeric(patient_m1$cvdmi)
# #
# # results_eq2 <- packDAMipd::get_parameter_estimated_regression("cvdmi", patient_m1, method = "survival",
# #                                                               indep_var = "as.numeric(as.character(agegrp0))",
# #                                                               info_get_method = "parametric",
# #                                                               info_distribution = "weibull",
# #                                                               covariates = c("diabmell", "prevmi","smoker", "pulse",
# #                                                                              "stdepres", "agrade", "sex", "leftbund", "treat", "survival::cluster(studyno)"),
# #                                                               strategycol = NA, strategyname = NA,
# #                                                               timevar_survival = "follow", interaction = NA)
# # #######################################################################
# # # From report Table 4
# # # Covariate	Hazard ratio*	95 % CI
# # # Age	1.777	1.499 to 2.108
# # # Diabetes	1.905	1.359 to 2.672
# # # Previous MI	1.471	1.087 to 1.990
# # # Smoker	1.651	1.207 to 2.258
# # # Pulse	1.062	1.012 to 1.114
# # # ST depression	1.423	1.067 to 1.913
# # # Angina	1.323	0.988 to 1.771
# # # Male	1.372	1.007 to 1.869
# # # Left BBB	1.977	1.169 to 3.344
# # # Treat	0.621	0.464 to 0.830
# # # Constant**	0.008	0.005 to 0.015
# # # Ancillary or shape parameter	0.579	0.505 to 0.664
# #
# # # cholesky decompostion matrix Table A3
# #
# # # 	Age	Diabetes	Previous MI	Smoker	Pulse	ST-
# # # depression	Angina	Male	Left BBB	Treat	Constant	ln ??
# # # Age	0.087	0	0	0	0	0	0	0	0	0	0	0
# # # Diabetes	-0.004	0.173	0	0	0	0	0	0	0	0	0	0
# # # Previous MI	-0.012	-0.014	0.153	0	0	0	0	0	0	0	0	0
# # # Smoker	0.051	0.008	0.007	0.151	0	0	0	0	0	0	0	0
# # # Pulse	0.001	-0.005	-0.000	0.000	0.024	0	0	0	0	0	0	0
# # # ST depression	-0.021	-0.006	0.019	-0.002	-0.021	0.144	0	0	0	0	0	0
# # # Angina	0.003	-0.002	-0.023	-0.003	0.006	-0.003	0.147	0	0	0	0	0
# # # Male	0.004	-0.005	-0.021	-0.009	0.017	0.000	0.021	0.154	0	0	0	0
# # # Left BBB	-0.033	-0.004	-0.028	0.010	-0.039	0.006	-0.008	0.001	0.261	0	0	0
# # # Treat	-0.000	-0.005	-0.005	0.008	0.003	0.001	-0.001	-0.001	0.006	0.148	0	0
# # # Constant	-0.122	0.007	-0.041	-0.049	-0.197	-0.069	-0.077	-0.104	-0.026	-0.056	0.092	0
# # # ln ??	0.003	0.002	0.001	0.000	0.000	0.002	0.001	0.001	0.002	-0.002	-0.043	0.054
# # #
# #
# #
# # #######################################################################
# # #generate index dummy
# # patient_m$indexdummy <- 0
# # patient_m$datdth <- as.Date(patient_m$datdth, format = "%d%b%Y")
# # patient_m$datedisc <- as.Date(patient_m$datedisc, format = "%d%b%Y")
# # patient_m$datmi <- as.Date(patient_m$datmi, format = "%d%b%Y")
# #
# # patient_m$indexdummy[which(patient_m$datdth <= patient_m$datedisc | patient_m$datmi <= patient_m$datedisc)] <- 1
# # patient_m$indexdummy[which(is.na(patient_m$datedisc))] <- 1 #added by sheeja -why?
# #
# # # We want to use data relating to first and second events in patients (cvdmi is the composite endpoint of mi or cv death)
# # patient_m$nfmi1 <- patient_m$nfmi
# # patient_m$cvdmi1 <- patient_m$cvdmi
# #
# # patient_m$cvdmi2 <- NA
# # patient_m$cvdmi2[which(patient_m$nfmi == 1)] <- 0 #added by sheeja -why?
# # patient_m$cvdmi2[which(patient_m$nfmi == 1 & (patient_m$mi2 == 1 | patient_m$cvdeath == 1))] <- 1
# #
# # patient_m$datmi2 <- as.Date(patient_m$datmi2, format = "%d%b%Y")
# #
# # patient_m$nfmi2 <- 0
# # patient_m$nfmi2[which(patient_m$mi2 == 1 & patient_m$cvdmi2 == 1 & (patient_m$datdth - patient_m$datmi2 > 30))] <- 1
# #
# # variables <- c(1,154,157) #cvdm1 and nfm1
# #
# # patient_m2a <- patient_m[,variables]
# #
# # patient_m2a <- reshape(patient_m2a, idvar = "studyno",
# #                        timevar = "EventNumber",
# #                        varying = list(names(patient_m2a[,c(2:3)])),
# #                        v.names = "nfmi", direction = "long")
# #
# # variables <- c(1,155,156) #cvdm2 and nfm2
# #
# # patient_m2b <- patient_m[,variables]
# #
# # patient_m2b <- reshape(patient_m2b, idvar = "studyno",
# #                        timevar = "EventNumber",
# #                        varying = list(names(patient_m2b[,c(2:3)])),
# #                        v.names = "cvdmi", direction = "long")
# #
# #
# # patient_m2 <- merge(patient_m2a,patient_m2b,by = c("studyno","EventNumber"))
# #
# # variables <- c(1,55,62,153) #prevmi, agegrp0,indexdummy
# # patient_m2c <- patient_m[,variables]
# #
# # patient_m2 <- merge(patient_m2,patient_m2c,by = c("studyno"))
# # patient_m2 <- subset(patient_m2, cvdmi == 1)
# #
# # rm(patient_m2a,patient_m2b,patient_m2c)
# #
# #
# # results_eq4 <- packDAMipd::get_parameter_estimated_regression("nfmi", patient_m2, method = "logistic",
# #                                                               indep_var = "indexdummy",
# #                                                               info_get_method = "parametric",
# #                                                               info_distribution = "binomial",
# #                                                               covariates = c("as.numeric(as.character(agegrp0))", "as.factor(prevmi)"),
# #                                                               strategycol = NA, strategyname = NA,
# #                                                               timevar_survival = "follow", interaction = NA)
# # results_eq4$param_estimated
# # rm(patient_m2)
# # #######################################################################
# #
# # ##### Table 6 in report
# # # Covariate	Odds ratio*	95 % CI
# # # Index dummy	3.040	1.614 to 5.726
# # # Age	0.699	0.520 to 0.941
# # # Previous MI	0.492	0.286 to 0.847
# # # Constant**	1.189	0.720 to 1.964
# #
# # # Cholesky decomposition matrix Table A5
# # # Index dummy	Age	Previous MI	Constant
# # # Index dummy	0.3230	0	0	0
# # # Age	-0.0108	0.1511	0	0
# # # Previous MI	-0.0063	0.0026	0.2771	0
# # # Constant	-0.0526	-0.1909	-0.0951	0.1316
# #
# #
# #
# # #######################################################################
# # qol_long <- merge(qol_long, patient_m[, c("studyno", "dm", "mindex", "dindex", "mi1yr", "mi", "datmi",
# #                                           "mi2", "datmi2", "daterand", "sex", "prevmi", "diabmell",
# #                                           "stdepres", "agegrp0", "smoker", "pulse", "leftbund", "agrade",
# #                                           "treat", "datdth", "daterand")], by = "studyno", all.x = T, all.y = T)
# #
# # eq5d_details <- get_eq5d_details(qol_long)
# # col1 = eq5d_details$name[1]
# # col2 = eq5d_details$name[2]
# # col3 = eq5d_details$name[3]
# # col4 = eq5d_details$name[4]
# # col5 = eq5d_details$name[5]
# # qol_long_naremov <- qol_long[!is.na(qol_long[col1]) & !is.na(qol_long[col2]) & !is.na(qol_long[col3]) &
# #                                !is.na(qol_long[col4]) & !is.na(qol_long[col5]),]
# #
# # eq5d_details <- get_eq5d_details(qol_long_naremov)
# #
# # qol_long_naremov_eq5d <- valueEQ5D::value3L(qol_long_naremov,eq5d_details$name[1],eq5d_details$name[2], eq5d_details$name[3],
# #                                             eq5d_details$name[4],eq5d_details$name[5],"UK","TTO",groupby = NA, agelimit = NA)
# #
# # qol_long_naremov_eq5d <- qol_long_naremov_eq5d$modifiedData[qol_long_naremov_eq5d$modifiedData$formno == 2,]
# # colnames(qol_long_naremov_eq5d)[36] <- "eq5d3l"
# # re_eq5d_lm <- packDAMipd::get_parameter_estimated_regression("eq5d3l", dataset = qol_long_naremov_eq5d,
# #                                                              method = "linear regression", indep_var = "diabmell",
# #                                                              info_get_method = NA, info_distribution = NA, covariates =
# #                                                                c("prevmi","stdepres", "agrade", "sex"), interaction =  FALSE)
# #
# # # Table 10 report
# # # Covariate	Coefficient	95 % CI
# # # Diabetes	-0.0506	-0.0915 to -0.0096
# # # Previous MI	-0.0443	-0.0761 to -0.0125
# # # ST depression	-0.0660	-0.0950 to -0.0369
# # # Angina	-0.0738	-0.1033 to -0.0443
# # # Male	0.0727	0.0436 to 0.1017
# # # Constant	0.6924	0.6636 to 0.7212
# # #
# # #
# # # # Cholesky decomposition matrix Table A5
# # # 	Diabetes	Previous MI	ST depression	Angina	Male	Constant
# # # Diabetes	0.0209	0	0	0	0	0
# # # Previous MI	-0.0012	0.0162	0	0	0	0
# # # ST dep.	-0.0006	0.0013	0.0147	0	0	0
# # # Angina	-0.0005	-0.0023	-0.0003	0.0149	0	0
# # # Male	0.0002	-0.0017	0.0001	0.0015	0.0146	0
# # # Constant	-0.0022	-0.0031	-0.0054	-0.0063	-0.0091	0.0071
# #
# #
# # #######################################################################
# # fill_index <- valueEQ5D::value3L(qol_long_naremov,eq5d_details$name[1],eq5d_details$name[2], eq5d_details$name[3],
# #                                  eq5d_details$name[4],eq5d_details$name[5],"UK","TTO",groupby = NA, agelimit = NA)
# #
# # qol_long_naremov_eq5d <- fill_index$modifiedData
# # colnames(qol_long_naremov_eq5d)
# # colnames(qol_long_naremov_eq5d)[36] <- "eq5d3l"
# # qol_long<-qol_long_naremov_eq5d
# # qol_long_a <- subset(qol_long, treat == "Conservative")
# # qol_long_a$treat <- 0
# # qol_long_b <- subset(qol_long,treat == "Intervention")
# # qol_long_b$treat <- 1
# #
# # qol_long <- rbind(qol_long_a, qol_long_b)
# # rm(qol_long_a,qol_long_b)
# #
# # qol_long$formno[qol_long$formno == 2] <- 0
# # qol_long$formno[qol_long$formno == 8] <- 4
# # qol_long$formno[qol_long$formno == 16] <- 24
# # qol_long$formno[qol_long$formno == 18] <- 36
# # qol_long$formno[qol_long$formno == 20] <- 48
# # qol_long$formno[qol_long$formno == 22] <- 60
# #
# #
# # ## added by sheeja
# # # not sure the reason..
# # qol_long$fxt <- as.numeric(as.character(qol_long$formno)) * as.numeric(as.character(qol_long$treat))
# # qol_long$fsq <- as.numeric(as.character(qol_long$formno)) * as.numeric(as.character(qol_long$formno))
# # qol_long$fsqxt <- as.numeric(as.character(qol_long$fsq)) * as.numeric(as.character(qol_long$treat))
# # ## added by sheeja
# #
# # ##added by sheeja --binary indicator for month 4 or 12
# #
# # qol_long$d4 <- 0
# # qol_long$d4[qol_long$formno == 4] <- 1
# # qol_long$d12 <- 0
# # qol_long$d12[qol_long$formno >= 12] <- 1
# #
# # qol_long$d4t <- as.numeric(qol_long$d4) * as.numeric(qol_long$treat)
# # qol_long$d12t <- as.numeric(qol_long$d12) * as.numeric(qol_long$treat)
# #
# # #calculate change from baseline
# #
# # qol_baseline <- qol_long[,c("studyno","formno","eq5d3l")]
# # qol_baseline <- qol_baseline[qol_baseline$formno == 0,]
# # qol_baseline <- qol_baseline[,c(1,3)]
# # qol_baseline <- plyr::rename(qol_baseline, c("eq5d3l" = "baseline"))
# #
# #
# # qol_long <- merge(qol_long,qol_baseline,by = "studyno",all.x = T, all.y = T)
# # qol_long$eqch <- as.numeric(qol_long$eq5d3l) - as.numeric(qol_long$baseline)
# # qol_long$eqch <- as.numeric(qol_long$eqch)
# #
# # #time to first mi in months #added by sheeja -time to MI is difference betwwen datmi-daterand
# # qol_long$ttmi <- NA
# #
# # qol_long$daterand <- as.Date(qol_long$daterand, format = "%d%b%Y")
# # qol_long$ttmi[qol_long$mi == 1] <- (qol_long$datmi[qol_long$mi == 1] - qol_long$daterand[qol_long$mi == 1]) * 12 / 365
# #
# # #time to 2nd mi in months
# # qol_long$ttmi2 <- NA
# #
# # qol_long$ttmi2[qol_long$mi2 == 1] <- (qol_long$datmi2[qol_long$mi2 == 1] - qol_long$daterand[qol_long$mi2 == 1]) * 12 / 365
# #
# #
# # #priormi =  1 if had mi either before the trial or during the trial before the time point of interest
# # qol_long$priormi[qol_long$prevmi == "No"] <- 0
# # qol_long$priormi[qol_long$prevmi == "Yes"] <- 1
# # cond_priormi <- (qol_long$mi == 1) & (qol_long$formno >= qol_long$ttmi)
# # qol_long$priormi[cond_priormi] <- 1
# #
# # #patient has a recent mi only if in the last 12 months
# # qol_long$currentmi <- qol_long$mi
# # cond_currentmi1 <- (qol_long$mi == 1) & ((qol_long$formno - 12 > qol_long$ttmi) | qol_long$ttmi >= qol_long$formno)
# # qol_long$currentmi[cond_currentmi1] <- 0
# # cond_currentmi2 <- (qol_long$mi2 == 1) & ((qol_long$formno - 12 <= qol_long$ttmi2) & (qol_long$ttmi2 <= qol_long$formno))
# # qol_long$currentmi[cond_currentmi2] <- 1
# #
# # length(unique(qol_long$studyno))
# #
# # #HRQoL in patients who have had a recent MI (within past 12 months)
# # #aim is to isolate the decrement associated with recent MIs and any previous MI
# #
# #
# # #decrement associated with a recent mi
# # qol_panel <- qol_long[which((qol_long$formno == 4) | (qol_long$formno == 12) | (qol_long$formno == 24) | (qol_long$formno == 36) |
# #                               (qol_long$formno == 48) | (qol_long$formno == 60) | (is.na(qol_long$formno))),]
# #
# # qol_panel <- qol_panel[which(qol_panel$eqch != "NA"),]
# #
# # re_eq5d_lm <- packDAMipd::get_parameter_estimated_regression("eqch", dataset = qol_panel, method = "mixed effect",
# #                                                              indep_var = "currentmi",
# #                                                              info_get_method = NA,
# #                                                              info_distribution = NA,
# #                                                              covariates = c("d4t",
# #                                                                             "d12", "d12t", "priormi"),
# #                                                              interaction =  FALSE, random_effect = "studyno")
# #
# #
# #
# # # Table 11 from report
# # # Covariate	Coefficient	Standard error	95 % CI
# # # D4i*	0.0384	0.0168	0.0054 to 0.0714
# # # D12	0.0383	0.0076	0.0234 to 0.0533
# # # D12i*	0.0177	0.0154	-0.0126 to 0.0480
# # # Previous MI	-0.0097	0.0156	-0.0404 to 0.0209
# # # Current MI	-0.0353	0.0220	-0.0784 to 0.0078
# # # Constant	0.0442	0.0126	 0.0195 to 0.0689
# # # Between patient standard error (??u)		0.295
# # # Within patient standard error (??e)		0.183
# # # Fraction of variance due to ui (??)		0.722
# #
# # # choesky decomposition matrix Table A9
# # # D4i	D12	D12i	Previous MI	Current MI	Constant
# # # D4i	0.0168	0	0	0	0	0
# # # D12	0.0024	0.0072	0	0	0	0
# # # D12i	0.0120	-0.0064	0.0072	0	0	0
# # # Previous MI	-0.0005	-0.0004	-0.0005	0.0156	0	0
# # # Current MI	0.0003	0.0008	0.0008	-0.0041	0.0216	0
# # # Constant	-0.0082	-0.0027	-0.0023	-0.0046	-0.0005	0.0075
# # #
# #
# #
# #
# # debug(packDAMipd::get_parameter_estimated_regression)
# # eqch_lm5 <- lme(eqch ~ d4t + d12 + d12t + priormi + currentmi, data = qol_panel, random= ~ 1 | studyno)    ##### TECNICAL REPORT - TABLE 11 #####
# # summary(eqch_lm5)                                                       ##### TECNICAL REPORT - TABLE 11 #####
# # eqch_lm5$coefficients$fixed
# #
# # VCM_eqch_lm5 <- vcov(eqch_lm5)                                            ##### TECNICAL REPORT - TABLE 11 #####
# # Chol_eqch_lm5 <- chol(VCM_eqch_lm5)
# # Chol_eqch_lm5


[]
