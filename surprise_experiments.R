library(tidyverse)



df_surprises <- all_pilots_wide_8var

dim(df_surprises)

unique(df_surprises$pilot_nr)
# 
# df_surprises %>% 
# group_by(pilot_nr) %>% 
# summarise(length(unique(Random_ID)))

# n per subject
df_surprises   |> summarise(length(unique(Random_ID)), .by = pilot_nr)

df_surprises %>% 
  filter(pilot_nr == "Pilot 10") %>% 
  ggplot(aes(x = SubjPE, y = Mood))+
  geom_smooth(method = "lm", colour = "red")+
  geom_point(alpha = 0.2)+
  facet_wrap(~Random_ID)


cors_by_pilot <- df_surprises %>%
  # filter(pilot_nr ==  "Pilot 10") %>% 
  group_by(pilot_nr, Random_ID) %>%
  summarize(cor=cor(SubjPE, Mood))


cors_by_pilot  %>% 
  group_by(pilot_nr) %>% 
  summarise(mean_cor = mean(cor))

library(lme4)
library(parameter)
test_mod <- lmer(Mood ~ SubjPE + (1| Random_ID), data = 
                   df_surprises[df_surprises$pilot_nr=="Pilot 7",], 
            REML = FALSE, 
            control = lmerControl(optimizer = "bobyqa"))
summary(test_mod)
standardize_parameters(test_mod)
