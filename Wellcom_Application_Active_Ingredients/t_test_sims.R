
n_sims <- 500
#p_vals <- c()
#n <- 150
#sample_size <- c(10,50,100,300, 400, 500)

#n = 100
avg_1 = 50
avg_2 = 52
std = 15

power_func_t_test <- function( n){
  p_vals <- c()
  for (i in 1:n_sims){
    
    
   df_t_test_sim <- data.frame(var_1 = rnorm(n, avg_1, std), var_2 = rnorm(n, avg_2, std))
   p_vals_sims[i] <- t.test(df_t_test_sim$var_1,df_t_test_sim$var_2, alternative = "two.sided")$p.value
    

  }
  sum(p_vals<0.05)/length(p_vals)
}

prop_abo_thresh_p_value <- lapply(seq(100, 600, by = 50),power_func_t_test)

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
