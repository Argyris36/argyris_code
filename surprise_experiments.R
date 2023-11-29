library(tidyverse)


df_surprises <-  read.csv("~/Downloads/all_pilots_wide_8var.csv")


dim(df_surprises)

unique(df_surprises$pilot_nr)
# 
# df_surprises %>% 
# group_by(pilot_nr) %>% 
# summarise(length(unique(Random_ID)))

# check n per subject
df_surprises   |> summarise(length(unique(Random_ID)), .by = pilot_nr)



# get averaged correlations per subject for each pilot
cors_by_pilot <- df_surprises %>%
  # filter(pilot_nr ==  "Pilot 10") %>% 
  group_by(pilot_nr, Random_ID) %>%
  summarize(cor=cor(SubjPE, Mood))


correlations_df <- cors_by_pilot  %>% 
  group_by(pilot_nr) %>% 
  summarise(mean_cor = mean(cor))

correlations_df <- correlations_df %>% arrange(match(pilot_nr, pilots))
correlations_df
  
# now plot across all pilots the relationship between subjective PE and Mood
pdf("mood_subj_PE_plts6_to_10.pdf")

pilots <- unique(df_surprises$pilot_nr)

pe_mood_plots <- list()

my_splits <- (split(df_surprises, df_surprises$pilot_nr))
my_splits <- my_splits[pilots] # make sure to re-arrange the order to be from 6-10 

for(i in 1:length(my_splits)){
  
plot <- my_splits[[i]] %>% 
  ggplot(aes(x = SubjPE, y = Mood)) +
  geom_smooth(method = "lm", colour = "red") +
  geom_point(alpha = 0.2) +
  facet_wrap(~Random_ID)+
  ggtitle(paste(pilots[i], "overall r = ", round(correlations_df[i,2], 2)))

# Store the plot in the list
pe_mood_plots[[i]] <- plot
}

# Print  plots
for (i in 1: length(my_splits)) {
  print(pe_mood_plots[[i]])
}
dev.off()




library(lme4)
library(parameters)

# test the ICC, i.e. variance explained by random effects.
# first for IDs
test_icc_id <- list()
icc_results_id <- list()
for(i in 1:length(my_splits)){
 test_icc_id[[i]] <- lmer(Mood ~ SubjPE + (1| Random_ID), data = 
                         df_surprises[df_surprises$pilot_nr==pilots[i],], 
                       REML = FALSE, 
                       control = lmerControl(optimizer = "bobyqa")) 
 icc_results_id[[i]] <- performance::icc(test_icc_id[[i]])
}
icc_results_id


# first for IDs and nesting by pilot (i.e. number of experiment)
test_icc_pilot <- list()
icc_results_pilot <- list()
for(i in 1:length(my_splits)){
  test_icc_pilot[[i]] <- lmer(Mood ~ SubjPE + (1| Random_ID/ pilot_nr), data = 
                             df_surprises[df_surprises$pilot_nr==pilots[i],], 
                           REML = FALSE, 
                           control = lmerControl(optimizer = "bobyqa")) 
  icc_results_pilot[[i]] <- performance::icc(test_icc_pilot[[i]])
}

# now check the differences in adjusted iccs
differences_icc <- 0
for(i in 1: length(my_splits)){
differences_icc[i] <- icc_results_id[[i]][2] - icc_results_pilot[[i]][2]
}
 
format(differences_icc, scientific = F) #  adding the nesting makes no difference

# now run lme models for random interecept only and rint + random slope and choose between them

# first lmes with random intercept models
rint_models <- list()
for(i in 1:length(my_splits)){
  rint_models[[i]] <- lmer(Mood ~ SubjPE + (1| Random_ID), data = 
                               df_surprises[df_surprises$pilot_nr==pilots[i],], 
                             REML = FALSE, 
                             control = lmerControl(optimizer = "bobyqa"))
}
rint_models

# now  lmes with random slopes
rslope_models <- list()
for(i in 1:length(my_splits)){
  rslope_models[[i]] <- lmer(Mood ~ SubjPE + (SubjPE| Random_ID), data = 
                   df_surprises[df_surprises$pilot_nr==pilots[i],], 
            REML = FALSE, 
            control = lmerControl(optimizer = "bobyqa"))
}
rslope_models

# now compare between them
p_vals <- 0
for(i in 1: length(my_splits)){
p_vals[i] <-  (anova(rint_models[[i]], rslope_models[[i]]))$`Pr(>Chisq)`[2]
}

format(p_vals, scientific = F) # the p-values show that there is always a significant difference



mix_models_per_pilot <- list() # the lme objects for each pilot
mix_models_coefficients <- list() # the coefficients for each pilot
std_param_mix_models_per_pilot <- list() # the standardised coefficients for each lme object
dfs_RE_raw_pe_mood <- list() # the dataframes that contain raw values and coefficeints (may not need this)
for(i in 1: length(my_splits)){

  mix_models_per_pilot[[i]] <-  lmer(Mood ~ SubjPE + (SubjPE| Random_ID), data = 
       df_surprises[df_surprises$pilot_nr==pilots[i],], 
     REML = FALSE, 
     control = lmerControl(optimizer = "bobyqa"))
  std_param_mix_models_per_pilot[[i]] <- parameters:: standardise_parameters( mix_models_per_pilot[[i]])
 
  mix_models_coefficients[[i]] <-  coef(mix_models_per_pilot[[i]])
 mix_models_coefficients[[i]] <- data.frame(mix_models_coefficients[[i]]$Random_ID)
 mix_models_coefficients[[i]]$Random_ID <- rownames(mix_models_coefficients[[i]])
 colnames(mix_models_coefficients[[i]]) <-c( "intercept", "slope", "Random_ID")
 
 #now merge these datasets with the raw values 
 dfs_RE_raw_pe_mood[[i]] <- left_join(my_splits[[i]], mix_models_coefficients[[i]], by = "Random_ID" )
 
  }
names(std_param_mix_models_per_pilot) <- pilots
std_param_mix_models_per_pilot$`Pilot 7`$Std_Coefficient[1] # to get intercept for example


