murder_rate_greece_time_trends <- read_excel("~/Downloads/murder_rate_greece_time_trends.xlsx") # created by hand from Greek Police data found online at their site


murder_rate_greece_time_trends<- murder_rate_greece_time_trends %>% 
  mutate(which_half = case_when(year<=median(year)~0, year>median(year)~1)) 

df_sum_murders <- murder_rate_greece_time_trends %>% 
group_by(which_half) %>% 
  summarise(avg_murder = mean(murder_rate))

n_years <- length(murder_rate_greece_time_trends$year)
mdn_year <- median(murder_rate_greece_time_trends$year)

murder_rate_greece_time_trends %>% 
  ggplot(aes(x= year, y = murder_rate))+
  geom_point()+
  geom_line() +
  labs(x = "year", y = "number of murders")+
  ggtitle("Number of Murders in Greece 1991 to 2010", subtitle = "data from astynomia.gr") +
  scale_x_continuous(n.breaks= n_years) +
  theme(axis.text.x = element_text(angle = 45)) +
  geom_vline(xintercept = mdn_year, colour = "red", linetype = "dashed") +
  annotate("text", x = 2001, y = 200, label= paste0("mean\nuntil 2006 = ", formatC(df_sum_murders$avg_murder[1], format = "g" , digits = 3)))+
  annotate("text", x = 2011, y = 200, label= paste0("mean\nsince 2007 = ", formatC(df_sum_murders$avg_murder[2], format = "g" , digits = 3)))
  
bodily_harm_by_age <- read_excel("~/Downloads/bodily_harm_by_age.xlsx") # created by hand from Greek Police data found online at their site
View(bodily_harm_by_age)




bodily_harm_by_age <- bodily_harm_by_age %>% 
  mutate(new_age_group = case_when(age == "seven_to_twelve"~ "A. 7-12 years", 
                                   age == "thirteen_to_seventeen" ~ "B. 13-17 years", 
                                   age == "eighteen_to_twenty" ~ "C. 18-24 years",
                                   age == "twenty_one_to_twenty_four" ~ "D. 21-24 years",
                                   age == "twenty_five_to_twenty_nine" ~ "E. 25-29 years", 
                                   age == "thirty_to_thirty_four" ~ "F. 30-34 years", 
                                   age == "thirty_five_to_fourty_four" ~ "G. 35-44 years"))


bodily_harm_by_age %>% 
  ggplot(aes(x = new_age_group, y = bodily_harm, fill = new_age_group))+
  geom_bar(stat  ="identity")+
  facet_wrap(~year)+
  theme(axis.text.x = element_text(angle = 45)) +
  xlab(NULL)+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  ylab("Number of Bodily Harm Incidents") +
  ggtitle("Bodily Harm by Year and Age Group in Greece 2020 -2022", subtitle = "data from astynomia.gr" )


sum_stats_bodily_harm <- bodily_harm_by_age %>% 
  group_by(year) %>% 
  summarise(crim_per_year = sum(bodily_harm)) 

sum_stats_adolescents <- bodily_harm_by_age%>% 
 filter(new_age_group == "B. 13-17 years") %>% 
  mutate(perc_adol_crime = 100*(bodily_harm/(sum(sum_stats_bodily_harm$crim_per_year))))

sum_stats_adolescents %>% 
  ggplot(aes(x = year, y = perc_adol_crime)) +
  geom_bar(stat  ="identity") +
  ylab("% bodily harm")+
  ggtitle("Bodily harm incidents perpetrated by adolescents", subtitle = "data from astynomia.gr" )+
  geom_text(aes(label = bodily_harm), vjust = 2, colour = "white")
  
library(lme4)
summary(lmer(bodily_harm ~ year*new_age_group + (1|new_age_group), data = bodily_harm_by_age)  )
summary(aov(bodily_harm ~year*new_age_group, data = bodily_harm_by_age))

           