## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = ">",
 fig.width = 6, 
 fig.height = 4
)

## ----setup--------------------------------------------------------------------
library(packDAMipd)

## -----------------------------------------------------------------------------
H <- health_state("H", cost = "c_H ", utility = "u_H")
S1 <- health_state("S1", cost = "c_S1",utility = "u_S1")
S2 <- health_state("S2", cost = "c_S2",utility = "u_S2")
D <- health_state("D", cost = "c_D ",utility = "u_D")

## -----------------------------------------------------------------------------
tmat <- rbind(c(1, 2, NA, 3), c(4, 5, 6, 7),c(NA, NA, 8, 9), c(NA, NA, NA, 10))
colnames(tmat) <- rownames(tmat) <- c("H","S1" ,"S2","D")

## -----------------------------------------------------------------------------
tmat_cost <- rbind(c(NA, 1, NA, 2), c(NA, NA, NA, 3),c(NA, NA, NA, 4), 
                   c(NA, NA, NA, NA))
colnames(tmat_cost) <- rownames(tmat_cost) <- c("H", "S1", "S2", "D")
tmat_util <- rbind(c(NA, 1, NA, NA), c(NA, NA, NA, NA),c(NA, NA, NA, NA), 
                   c(NA, NA, NA, NA))
colnames(tmat_util) <- rownames(tmat_util) <- c("H", "S1", "S2", "D")

## -----------------------------------------------------------------------------
tm <- populate_transition_matrix(4, tmat, c("p_HH","p_HS1","p_HDage","p_S1H",
                                   "p_S1S1", "p_S1S2", "p_S1Dage",
                                   "p_S2S2","p_S2Dage","p_DD" ), 
                                 colnames(tmat))
tm_cost <- transition_cost_util(4, tmat_cost, c("ic_HS1","ic_D","ic_D","ic_D"), colnames(tmat_cost))
tm_util <- transition_cost_util(4, tmat_util, c("du_HS1" ), colnames(tmat_util))

## -----------------------------------------------------------------------------
mortality_file = system.file("extdata", "LifeTable_USA_Mx_2015.csv",  package = "packDAMipd")
param_list <- define_parameters(age_init = 25,
                      mortality_age = "get_mortality_from_file(mortality_file, age_init - 1 + markov_cycle, \"total\")",
                      p_HDage = "1-exp(-mortality_age)", p_HS1  = 0.15, 
                      p_S1H = 0.5,
                      p_S1S2  = 0.105, hr_S1   = 3,hr_S2   = 10,
                      p_S1Dage  =  "1-exp(-mortality_age *hr_S1)",
                      p_S2Dage  = "1-exp(-mortality_age *hr_S2)",
                      p_HH = "1 - (p_HS1 + p_HDage)",
                      p_S1S1 = "1 - (p_S1H + p_S1S2+ p_S1Dage)",
                      p_S2S2 = "1 - ( p_S2Dage)",
                      p_DD = 1,
                      c_H   = 2000,c_S1  = 4000,c_S2  = 15000,
                      c_D   = 0, c_Trt = 12000,u_H   = 1,
                      u_S1  = 0.75, u_S2  = 0.5,u_D   = 0,
                      u_Trt = 0.95, du_HS1 = -0.01, ic_HS1 = 1000, ic_D = 2000)

## -----------------------------------------------------------------------------
health_states <- combine_state(H,S1,S2,D)
uc_strategy <- strategy(tm, health_states, "Usual care",tm_cost,tm_util)

uc_markov <- markov_model(current_strategy = uc_strategy, cycles = 85, 
                          initial_state = c(1, 0,0,0), discount = c(0.03,0.03), parameter_values = param_list, TRUE, FALSE, FALSE,
                          method = "half cycle correction")
png("states.png")
plot_model(uc_markov)
dev.off()

## -----------------------------------------------------------------------------
H <- health_state("H", cost = "c_H ", utility = "u_H")
S1 <- health_state("S1", cost = "c_S1 + c_Trt ",utility = "u_Trt")
S2 <- health_state("S2", cost = "c_S2 + c_Trt",utility = "u_S2")
D <- health_state("D", cost = "c_D ",utility = "u_D")
health_states <- combine_state(H,S1,S2,D)
trt_strategy <- strategy(tm, health_states, "New treatment",tm_cost,tm_util)
trt_markov <- markov_model(current_strategy = trt_strategy, cycles = 85, 
                           initial_state = c(1, 0,0,0), discount = c(0.03,0.03),parameter_values = param_list,TRUE, FALSE, FALSE ,
                           method = "half cycle correction")

png("trt.png")
plot_model(trt_markov)
write.csv(trt_markov$utility_matrix, "util_trt.csv")
write.csv(uc_markov$utility_matrix, "util_uc.csv")

write.csv(trt_markov$cost_matrix, "cost_trt.csv")
write.csv(uc_markov$cost_matrix, "cost_uc.csv")

## -----------------------------------------------------------------------------
list_markov <- combine_markov(uc_markov, trt_markov)
cal <- calculate_icer_nmb(list_markov, threshold = 65000, 
                          comparator = "Usual care" )
write.csv(cal, "icer_nmb.csv" )

## -----------------------------------------------------------------------------
plot_nmb_lambda(list_markov,threshold_values = c(20000, 40000, 60000, 80000), 
          comparator = "Usual care" , currency = "USD")
plot_efficiency_frontier(cal, 20000)


## -----------------------------------------------------------------------------
sample_list <- define_parameters(u_Trt = "normal(mean = 0.95, sd = 0.02)",
                                 c_Trt = "normal(mean = 12000, sd = 1000)")
param_table_mono <- define_parameters_psa(param_list, sample_list)
param_table_combo <- define_parameters_psa(param_list, sample_list)
result_psa_mono = do_psa(uc_markov, param_table_mono, 50)
result_psa_comb = do_psa(trt_markov, param_table_combo, 50)

list_result_psa_mono <- list_paramwise_psa_result(result_psa_mono, NULL, NULL, 
                                                  NULL)
list_result_psa_comb <- list_paramwise_psa_result(result_psa_comb, NULL, NULL, 
                                                  NULL)
list_all <- list_paramwise_psa_result(result_psa_mono,result_psa_comb, 65000, 
                                      "Usual care")

summary_plot_psa(result_psa_mono)
summary_plot_psa(result_psa_comb)
summary_plot_psa(result_psa_mono,result_psa_comb, 65000, "Usual care")

ceac_res <- plot_ceac_psa(uc_markov, trt_markov, param_table_mono, param_table_combo, seq(100000, 200000, 10000), 500, "Usual care")


