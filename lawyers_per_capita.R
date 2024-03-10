library(tidyverse)
library(readxl)

# rule of law data from here: https://worldjusticeproject.org/rule-of-law-index/global/2023 Had to extract "by hand"
# lawyers per country data from here: https://worldpopulationreview.com/country-rankings/lawyers-per-capita-by-country
rule_of_law_short <- read_excel("~/Downloads/rule_of_law_short.xlsx")
lawyers_per_capita <- read.csv("~/Downloads/lawyers-per-capita-by-country-2024.csv")
lawyers_rule_of_law <- right_join(rule_of_law_short,lawyers_per_capita, "country" )

# add lawyers per capita from extra search

# Denmark https://rm.coe.int/country-profile-denmark-en/1680a9671e#:~:text=The%20number%20of%20lawyers%20(117,and%20among%20court%20presidents%2041%25.
# Sweden https://www.osce.org/files/f/documents/a/b/36309.pdf 
# Germany https://www.brak.de/presse/zahlen-und-statistiken/statistiken/#:~:text=Insgesamt%20waren%20zum%201.1.2023,(Vorjahr%3A%20165.587)%20zugelassen.
# France https://www.justice.gouv.fr/sites/default/files/migrations/portail/art_pix/1_1_pejc_com_stat_prof_avocat_2016.pdf 

df_to_add <- data.frame(country = c("Denmark", "Sweden", "Germany", "France"), LawyersPerCapitaLawyersPer100kInhabitants2016 = c(
  116, 49, 199, 94.4))

# create new dataset
lawyers_per_capita <- rbind(lawyers_per_capita, df_to_add)
# re-join
lawyers_rule_of_law <- right_join(rule_of_law_short,lawyers_per_capita, "country" )


lawyers_rule_of_law[lawyers_rule_of_law$country == "Greece",]

lawyers_rule_of_law <-
  lawyers_rule_of_law %>% 
rename(lawyers_per_capita = LawyersPerCapitaLawyersPer100kInhabitants2016)

plot(lawyers_rule_of_law$rol_index, lawyers_rule_of_law$lawyers_per_capita)
range(lawyers_rule_of_law$income_group, na.rm = T)


lawyers_rule_of_law %>% 
  ggplot(aes(lawyers_per_capita, rol_index, colour = income_group))+
  geom_point()
  



plot_high_rol_lawyers <- lawyers_rule_of_law %>% 
  filter(income_group == "High") %>% 
  ggplot(aes(lawyers_per_capita, rol_index))+
  geom_point(colour = "red") +                                   
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") 

lawyers_rule_of_law_high <-lawyers_rule_of_law %>% 
  filter(income_group == "High") 

model_simple <- lm(rol_index ~ lawyers_per_capita, data = lawyers_rule_of_law_high)

model_simple_params <- summary(model_simple)
model_simple_params$adj.r.squared

lab_for_r_sq = paste("Adjusted R^2 =", round(model_simple_params$adj.r.squared, 1))

plot_high_rol_lawyers <- plot_high_rol_lawyers + geom_text(data =lawyers_rule_of_law_high, aes(label = country))
plot_high_rol_lawyers +
  ggtitle("Relationship betwen Rule of Law and Lawyer Density in High Income Countries", 
          subtitle = lab_for_r_sq )+
  ylab("Rule of Law Index")+
  xlab("Lawyers per 100K Inhabitants")
  
# check spearman due to distribution of lawyers 
cor.test(lawyers_rule_of_law_high$lawyers_per_capita,lawyers_rule_of_law_high$rol_index, method =  "spearman")


  