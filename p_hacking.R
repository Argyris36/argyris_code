

lm_variability_function <- function (n_subjects){

pred_vars <- c("gender",
"ethnicity",
"age_dad",
"age_mum",
"polygenic_score",
"iq_dad",
"iq_mum",
"job_dad",
"job_mum",
"num_siblings",
"height",
"weight",
"prematurity",
"ivf",
"age_death_granny",
"age_death_gdad",
"hyperchol_granny",
"hyperchol_gdad",
"illness_sibling")



distribs_preds <- lapply(1:length(pred_vars), function(x)  rnorm(n_subjects, 0, 1))

df_sim_lm <- matrix(NA, nrow= n_subjects, ncol =length(distribs_preds) )

for (i in 1: length(distribs_preds)){
  
df_sim_lm[, i]   <- distribs_preds[[i]]
  
}

df_sim_lm <- data.frame(df_sim_lm)
colnames(df_sim_lm) <- pred_vars


#now create two correlated variables: 

df_temp <- rnorm_multi(
  n = n_subjects,
  mu = c(magic_var = 1, outcome_var  = 2),
  sd = c(0.5, 1),
  r = c(0.3)
)


#check
cor(df_temp$outcome_var, df_temp$magic_var)


df_sim_lm <-cbind(df_sim_lm, df_temp)

# note that for illustration purposes I omit the intercept here
formula <- as.formula(paste0("outcome_var ~ ", paste0("0 +", names(df_sim_lm)[-ncol(df_sim_lm)], collapse = " + ")))

all_models <- broom::tidy(lm(formula, df_sim_lm))
#n_p_value_significant <- sum(broom::tidy(lm(formula, df_sim_lm))$p.value<0.05)

#return(list(n_p_value_significant, all_models))
return(all_models)

}



n_sims <- 100

#simulated_p_values <- replicate(n_sims, lm_variability_function(50))



simulated_models <- lapply (1:n_sims, function(x) lm_variability_function(30))

how_many_significant_p_values <- 0

for(i in 1: length(simulated_models)){

how_many_significant_p_values[i] <- sum(simulated_models[[i]]$p.value<0.05)

}
how_many_significant_p_values 
  
# extract the first model 
model_with_one_sig <- simulated_models[[1]]
# extract the model with most significant findings
model_with_many_sig <- simulated_models[[ which(how_many_significant_p_values == max(how_many_significant_p_values )) ]]

# # save models in excel commented out for the moment
# library(openxlsx)
# ## Create a new workbook and add a worksheet
# wb <- createWorkbook("Creator of workbook")
# addWorksheet(wb, sheetName = "model_with_one_sig")
# writeData(wb, sheet = 1, x = model_with_one_sig)
# addWorksheet(wb, sheetName = "model_with_many_sig")
# writeData(wb, sheet = 2, x = model_with_many_sig)
# ## Save workbook to working directory
# saveWorkbook(wb, file = "p_hacking.xlsx", overwrite = TRUE)


#
sum(how_many_significant_p_values>1)/length(how_many_significant_p_values)
table(how_many_significant_p_values)

df_sig_values_percentages <- data.frame(table(how_many_significant_p_values))

df_sig_values_percentages %>% 
  ggplot(aes(how_many_significant_p_values, Freq, fill = how_many_significant_p_values))+
  geom_bar(stat = "identity")+
  ggtitle("p-hacking", subtitle = "capitalising on chance")+
  xlab("number of variables significant")+
  ylab("Percentage")+
  theme_minimal()


# simulated_p_values
# sum(simulated_p_values>1)/length(simulated_p_values)
# sum(simulated_p_values<1)/length(simulated_p_values)
# 
# all_models[which(simulated_p_values==6)[1]]
# 
# 




