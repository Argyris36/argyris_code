## get per individual correlations 

library(faux)
library(tidyverse)

within_indiv_cor <- function(sample, correlation, avg_1, avg_2, sd_1, sd_2, n_trials, coef_criterion)
{
  vars <- 2 # the number of variables
  df_list <- list()
  
  for (i in 1:sample){
    df_list[[i]] <-  rnorm_multi(n = n_trials, # the faux function for mvnorms; alter to add two variables with different means etc
                                 mu = c(avg_1, avg_2),
                                 sd = c(sd_1, sd_2),
                                 r = c(correlation), 
                                 varnames = paste0("T_", seq(1:vars)),
                                 empirical = FALSE)
    
    
  }
  
  # then simply get correlation for each row
  
  var_with_correlations <- 0 #an empty vector to put the correlations in
  var_with_p <- 0  # an empty vector to put the p-values of each correlation in. We are not using these, but you can 
  # check them. They are the per-individual p-value and are not affected by sample size (but are by trial number)
  
  # run a loop for each element of the list to extract correlations and p-values (remember p-values are optional here)
  for (i in 1: length(df_list)){ 
    
    var_with_correlations[i] <- cor.test(df_list[[i]]$T_1,df_list[[i]]$T_2)$estimate # this is the correlation estimate
    var_with_p[i] <- cor.test(df_list[[i]]$T_1,df_list[[i]]$T_2)$p.value # this is the p-value estimate (optional)
    
  }
  df_cors_ps <- data.frame(corrs=var_with_correlations, ps = var_with_p) 
  
  
  
  
  # here comes the one sided test adn the p-value that we are going to use
  p_from_t_test <- t.test(var_with_correlations, mu = coef_criterion,  alternative = "greater") # whether the average across all subjects is above .35 
  # here you extract the p-value
  p_from_t_test <- p_from_t_test$p.value # here you extract the p-value from the one-sided test.
  # what you get out is the p-value from that one-sided t-test
  return( p_from_t_test )  
}



## here you get for each sample size the proportion of p-values abvoe criterion (fine to use 0.05)
p_values_sample <- list()
perc_p_values_above_p_criterion <- 0

sample_sizes <- seq(10, 100, by = 10 )
n_sims <- 500
correlation_level = 0.3
mean_level_1 = 7
mean_level_2 = 300
sd_level_1 = 3
sd_level_2 = 100
trial_number = 91
coefficient_criterion = 0.25

p_criterion <- 0.05 # no reason to correct


for (i in 1: length(sample_sizes)){
  
  p_values_sample [[i]] <-   replicate(n_sims,within_indiv_cor(sample_sizes [i], correlation_level, 
                                                               mean_level_1, mean_level_2, 
                                                               sd_level_1, sd_level_2,
                                                               trial_number, coefficient_criterion))
  perc_p_values_above_p_criterion [i]<- sum( p_values_sample [[i]] < p_criterion)/length(p_values_sample [[i]])
  
}

perc_p_values_above_p_criterion

## now plot

power_level <- .9 # for 90% power

# create the necessary dataframe
df_for_plot <- data.frame(perc_above_criterion = perc_p_values_above_p_criterion, sample_size = sample_sizes)

# prepared the title
title_1 <- paste0("Power Simulation for RT ~ Rank Distance; ", " power = ", power_level, 
                  ", alpha = ",p_criterion)

title_2 <- paste0("r = ", correlation_level,
  ", n_trials = ", trial_number,
  ", n_sims = ", n_sims)

# now create the actual plot
df_for_plot %>% 
  ggplot(aes(x = sample_size, y = perc_above_criterion))+
  geom_point()+
  geom_hline(yintercept = 0.9, colour = "red", linetype = "dashed") +
  ggtitle(paste0(title_1, "\n", title_2))



