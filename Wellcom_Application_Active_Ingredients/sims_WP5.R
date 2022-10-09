# library(tidyverse)
# library(faux)
# library(simr)
# library(lme4)
# library(lmerTest)
# library(stringr)
# 



n_sims <- 500
#p_vals <- c()
#n <- 150
#sample_size <- c(10,50,100,300, 400, 500)


power_func <- function( n){
  p_vals <- c()
for (i in 1:n_sims){
  
  
  datA <- rnorm_multi(n = n, # create two correlated variables
                      mu = c(50, 54.5 ),
                      sd = c(15, 15),
                      r = c(0.5), 
                      varnames = c("T_0_Cond_A", "T_1_Cond_A"),
                      empirical = FALSE)
  
  datB <- rnorm_multi(n = n,  # create two correlated variables
                      mu = c(50, 50.5 ),
                      sd = c(15, 15),
                      r = c(0.5), 
                      varnames = c("T_0_Cond_B", "T_1_Cond_B"),
                      empirical = FALSE)
  
  subject <- rep((1:n), times = 4)
  time <- rep(c("time_0", "time_1"), each = n*2)
  cond <- rep(c("A", "B"), each = n, times = 2)
  
  df_for_sim <-  data.frame(subject = subject, time = time, cond = cond, mood = c(datA$T_0_Cond_A, datB$T_0_Cond_B, datA$T_1_Cond_A, datB$T_1_Cond_B))
  
  lmer_int <- lmer(mood ~ 1 + cond + time + cond:time + (1|subject), data = df_for_sim, REML = FALSE,) # fit the model with the interaction (score~time*cond + time*surprise , data = df_for_sim )
  lmer_null <- lmer(mood ~ 1 + cond + time  + (1|subject), data = df_for_sim, REML = FALSE,) # fit the model without the interaction  lm_null <- lm(mood ~ 1 + cond + time, data = df_for_sim)
  p_vals[i] <- anova(lmer_int, lmer_null)$`Pr(>Chisq)`[2] # put the p-values in a list
  
}
sum(p_vals<0.05)/length(p_vals)
}
prop_abo_thresh_p_value <- lapply(seq(100, 600, by = 50),power_func)

power_data_WP5 <- data.frame(cbind(x = seq(100, 600, by = 50), y = prop_abo_thresh_p_value))
#plot(seq(50, 250, by = 10),prop_abo_thresh_p_value)

power_WP5 <- power_data_WP5 %>% 
ggplot(aes(x= as.numeric(x), y = as.numeric(y))) + 
  geom_point() +
  geom_hline(yintercept = 0.9, colour = "red")

power_WP5 + 
labs(x= "Sample Size", y="Power") +
  annotate("text",x= 400,y=0.6,label="simulations = 500, \n alpha = 0.05, \n d = 0.3")+ #+ coord_cartesian(ylim=c(-0,26),clip="off")+
ggtitle("Power Simulation for WP5")


#length(p_vals)