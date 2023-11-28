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

  
# now plot across all pilots the relationship between subjective PE and Mood
pdf("mood_subj_PE_plts6_to_10.pdf")

pilots <- unique(df_surprises$pilot_nr)

pe_mood_plots <- list()

my_splits <- (split(df_surprises, df_surprises$pilot_nr))

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
library(parameter)
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

test_icc_pilot <- list()
icc_results_pilot <- list()
for(i in 1:length(my_splits)){
  test_icc_pilot[[i]] <- lmer(Mood ~ SubjPE + (1| Random_ID/ pilot_nr), data = 
                             df_surprises[df_surprises$pilot_nr==pilots[i],], 
                           REML = FALSE, 
                           control = lmerControl(optimizer = "bobyqa")) 
  icc_results_pilot[[i]] <- performance::icc(test_icc_pilot[[i]])
}


for
icc_results_id[[1]][1] - icc_results_pilot[[1]][1]


performance::icc(model[[i]])
test_mod <- lmer(Mood ~ SubjPE + (1| Random_ID), data = 
                   df_surprises[df_surprises$pilot_nr=="Pilot 7",], 
            REML = FALSE, 
            control = lmerControl(optimizer = "bobyqa"))
summary(test_mod)
standardize_parameters(test_mod)
