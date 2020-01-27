library(DecisionAnalysisModel)
H <- health_state("H", cost="c_H ",utility="u_H")
S1 <- health_state("S1", cost="c_S1",utility="u_S1")
S2 <- health_state("S2", cost="c_S2",utility="u_S2")
D <- health_state("D", cost="c_D ",utility="u_D")

tmat <- rbind(c(1, 2,NA,3), c(4, 5,6,7),c(NA, NA, 8,9), c(NA,NA,NA,10))
colnames(tmat) <- rownames(tmat) <- c("H","S1" ,"S2","D")


tm <- transition_matrix(4, tmat, c("p_HH","p_HS1","p_HD","p_S1H",
                                   "p_S1S1", "p_S1S2", "p_S1D",
                                   "p_S2S2","p_S2D","p_DD" ), colnames(tmat) )

a<-define_parameters(p_HD = 0.002, p_HS1  = 0.15, p_S1H = 0.5,
                     p_S1S2  = 0.105,hr_S1   = 3,hr_S2   = 10,
                     p_S1D  = "1 - exp(log(1 - p_HD) * hr_S1)",
                     p_S2D  =" 1 - exp(log(1 - p_HD) * hr_S2)",
                     p_HH = "1 - (p_HS1 + p_HD)",
                     p_S1S1= "1 - (p_S1H + p_S1S2+ p_S1D)",
                     p_S2S2= "1 - ( p_S2D)",
                     p_DD= 1,
                     c_H   = 2000,c_S1  = 4000,c_S2  = 15000,
                     c_D   = 0, c_Trt = 12000,u_H   = 1,
                     u_S1  = 0.75,u_S2  = 0.5,u_D   = 0,
                     u_Trt = 0.95, du_HS1 = 0.01,ic_HS1 = 1000,ic_D   = 2000 )
health_states <- combine_state(H,S1,S2,D)
uc_strategy <- strategy(tm, health_states, "Usual care")
undebug(markov_model)
uc_markov <-markov_model(uc_strategy, 85, c(1, 0,0,0),c(0,0,0,0),discount=c(0.03,0.03),a)
uc_markov$trace_matrix
a_A[, , 3]

plot_model(uc_markov)
