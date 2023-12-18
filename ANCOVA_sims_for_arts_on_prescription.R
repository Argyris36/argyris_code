library(tidyverse) # for data wrangling
library(faux)      # for simulation
library(broom)     # for tidy analysis results

set.seed(1974)

# I will simulate the following
# two groups, treatment and WL control. 
# they start off with distributions with the same mean and sd (due to randomisation)
# the WL control stays the same, the treatment group changes to arrive at a d = 0.4, which i
# what we stipulated in advance
# the shape of the change should not matter that much for this linear model calculation
# what should matter are 
# a) sds which I will keep the same as in baseline
# b) number of measurements, stipulated to be n = 4
# c) number of people, sample size, which I will vary for the simulation



# here comes the actual function
ancova_sim_func <- function(df_means, sd_across, n_subjects, number_sims, correlations_across){
  
  arts_on_pres_sim <- sim_design(
    n = n_subjects, # this is the per arm n 
    between = list(group = c("active", "wl")), 
    within = list(time = c("T1", "T2", "T3", "T4")), 
    mu= df_means,
    sd = sd_across,
    dv = "score",
    r = correlations_across,
    long = TRUE, 
    plot = FALSE,
    rep = number_sims
    
  )  


  temp_results <- lst()
  temp_results_disp <- lst()
  temp_p_values <- 0
  
  for(i in 1:n_sims){
    
    temp_results[[i]] <-  aov(score~ time + group + time*group , data = arts_on_pres_sim$data[[i]])
    
    temp_results_disp[[i]] <-broom::tidy(temp_results[[i]])
    
    temp_p_values[i] <-  temp_results_disp[[i]]$p.value[3]
    
    
  }
  return(temp_p_values)
}


# here come the parameters that I will pass to this function
# from https://hqlo.biomedcentral.com/articles/10.1186/1477-7525-10-156
mean_T1 <- 43.5 # the biggest trial in that list, the PEIP
sd_T1 <- 10.4 # from that trial
d <- -0.5 # stipulated effect size of difference
# to derive the mean needed for the final effect size
# cohen_d = mean_T1-mean_T4/sd_T1 # using sd_T_1 as I stipulated this to be the same throughout
# which converts to 
mean_T4 <- mean_T1 + d*sd_T1

# to create equally spaced value, my idealised decline of values
the_treatment_means <- seq(mean_T1, mean_T4, length.out = 4)

# the control means will be the same throughout
the_control_means <- rep(mean_T1, 4)

# we assume a correlation between values from each time point r = 0.6
within_corr <- 0.5

# now we can simulate using faux.
n_sims <- 500




# the way I have created the function above, I need to create this dataframe as an obejct for 
# the faux function
df_means <-data.frame(T1 = c(the_treatment_means[1], the_control_means[1]),
                      T2 = c(the_treatment_means[2], the_control_means[2]), 
                      T3 = c(the_treatment_means[3], the_control_means[3]),
                      T4 = c(the_treatment_means[4], the_control_means[4]),
                      row.names = c("active", "wl"))




# Now I will loop this function over a number of different sample sizes

# this is the list of sample sizes
n_per_arm <- seq(20,300, by = 20)

# this is the p-values to extract
p_vals <- lst()
p_above_criterion <- 0
for(i in 1:length(n_per_arm)){
  
p_vals[[i]] <- ancova_sim_func (df_means = df_means, sd_across = sd_T1, 
                 n_subjects = n_per_arm[i] , number_sims = n_sims, correlations_across
                 = within_corr)

p_above_criterion[i] <- sum(p_vals[[i]]<0.05)/length(p_vals[[i]])

}

p_above_criterion


# now I can plot the sample sizes
df_to_plot <- data.frame(p_above_criterion = p_above_criterion, n_per_arm = n_per_arm)

df_to_plot %>% 
  ggplot(aes(y = p_above_criterion, x = n_per_arm))+
  geom_point()+
  geom_hline(yintercept = 0.8, colour = "red", linetype = "dashed") +
  scale_x_continuous("sample size", breaks = n_per_arm)
  

#### I have added here the simulations done using an R library. 
#### It largely agrees with the above under certain conditions
library(Superpower)

power_arts_prescr = ANCOVA_analytic(
  design = "2b*2b", # a 2x2
  mu = c(43.5, 38.3, # means as above
         43.5, 43.5),
  n_cov = 3, # some number of covariates--hadn't done this above
  sd = 10.4, # as above
  r2 = .4, # the overall R-squared of the model
  alpha_level = .05, # alpha level
  beta_level = .2, # 1-beta for power
  round_up = TRUE
)

# Print main results
power_arts_prescr 


# # from https://hqlo.biomedcentral.com/articles/10.1186/1477-7525-10-156
# mean_T1 <- 43.5 # the biggest trial in that list, the PEIP
# sd_T1 <- 10.4 # from that trial
# d <- -0.5 # stipulated effect size of difference
# # to derive the mean needed for the final effect size
# # cohen_d = mean_T1-mean_T4/sd_T1 # using sd_T_1 as I stipulated this to be the same throughout
# # which converts to 
# mean_T4 <- mean_T1 + d*sd_T1
# 
# # to create equally spaced value, my idealised decline of values
# the_treatment_means <- seq(mean_T1, mean_T4, length.out = 4)
# 
# # the control means will be the same throughout
# the_control_means <- rep(mean_T1, 4)
# 
# # we assume a correlation between values from each time point r = 0.6
# within_corr <- 0.6
# 
# # now we can simulate using faux.
# n_sims <- 50
# 
# n_subj_per_arm <- 100
# 
# 
#####this is an example 
# arts_on_pres_sim <- sim_design(
#   n = 10, # this is the per arm n
#   between = list(group = c("active", "wl")),
#   within = list(time = c("T1", "T2", "T3", "T4")),
#   mu= data.frame(T1 = c(the_treatment_means[1], the_control_means[1]),
#                  T2 = c(the_treatment_means[2], the_control_means[2]),
#                  T3 = c(the_treatment_means[3], the_control_means[3]),
#                  T4 = c(the_treatment_means[4], the_control_means[4]),
#    # sd= sd_T1,
#   row.names = c("active", "wl")),
#   sd = sd_T1,
#   dv = "score",
#   r = within_corr,
#   long = TRUE,
#   rep = 1
# 
# )
#   
# 
# #dim(arts_on_pres_sim$data[[1]])
# 
# 
# # test <- aov(response~factor(drug)+Error(factor(patient)), data = df)
# # 
# # 
# # test <-aov(score~ time + group + time*group , data = arts_on_pres_sim$data[[1]])
# # test <-broom::tidy(test)
# # test$p.value[3]
# 
# temp_results <- lst()
# temp_results_disp <- lst()
# temp_p_values <- 0
# 
# for(i in 1:n_sims){
#   
# temp_results[[i]] <-  aov(score~ time + group + time*group , data = arts_on_pres_sim$data[[i]])
# 
# temp_results_disp[[i]] <-broom::tidy(temp_results[[i]])
# 
# temp_p_values[i] <-  temp_results_disp[[i]]$p.value[3]
#   
# 
# }
# 
# sum(temp_p_values<0.05)/length(temp_p_values)


# create a function for all the above -------------------------------------

# I will simulate the following
# two groups, treatment and WL control. 
# they start off with distributions with the same mean and sd (due to randomisation)
# the WL control stays the same, the treatment group changes to arrive at a d = 0.4, which i
# what we stipulated in advance
# the shape of the change should not matter that much for this linear model calculation
# what should matter are 
# a) sds which I will keep the same as in baseline
# b) number of measurements, stipulated to be n = 4
# c) number of people, sample size, which I will vary for the simulation

