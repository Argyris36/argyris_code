
# student mental health referrals -----------------------------------------
library(tidyverse)
library(lubridate)
library(ggrepel)
df_student_mh_refs_by_ethnicity <- read_excel("df_student_mh_refs_by_ethnicity.xlsx")


long_df_student_mh_refs_by_ethnicity <- df_student_mh_refs_by_ethnicity %>% 
pivot_longer(cols =c(  "White" ,  "Mixed"  , "Asian" ,  "Black"  , "Chinese", "Other" ,  "Missing"), names_to = "ethnicity", 
             values_to = "numbers")

View(long_df_student_mh_refs_by_ethnicity)

df_dates_totals <- long_df_student_mh_refs_by_ethnicity %>% 
  group_by(Year, Month) %>% 
  mutate(total_n_year_month = colSums(across(is.numeric)), 
         date = ymd(paste(Year, Month, "01", sep = "-")))


verticals <- as.Date(paste0(c(2018:2022),"-01-01"))

dates_labels <-  as.Date(paste0(c(2017:2022),"-06-01"))
labels <- c(2017:2022)
df_labels <- data.frame(
                        dates_labels = dates_labels,
                        labels = labels)
p <- df_dates_totals %>% 
  ggplot(aes(x = date, y = total_n_year_month ))+ 
  geom_line()+
  geom_vline(xintercept = verticals, colour = "red", 
             linetype= "dashed")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b") +

  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1, size = 12)) 

p +
  annotate("text", x = df_labels$dates_labels, 
           y = 200, label = df_labels$labels, size = 6 )+
  ylab("Total Stuent Referrals Per Year")+
  xlab("Months of the Year") +
  ggtitle("Student Referrals to IAPT-Services by Year and Month")
  

df_dates_totals_missing <- df_dates_totals %>% 
  filter(!ethnicity == "Missing")

p_ethn <- df_dates_totals_missing %>% 
  ggplot(aes(x = date, y = numbers, , color = ethnicity))+ 
  geom_line()+
  geom_vline(xintercept = verticals, colour = "red", 
             linetype= "dashed")+
  scale_x_date(date_breaks = "2 month", date_labels =  "%B") +
  
  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1, size = 12))  +
  annotate("text", x = df_labels$dates_labels, 
           y = 100, label = df_labels$labels, size = 6 )+
  ylab("Total Stuent Referrals Per Year")+
  ggtitle("Student Referrals to IAPT-Services by Year and Month", 
          subtitle = "by self-reported ethnicity")+
  theme(plot.title = element_text(size = 20, face = "bold"), 
         plot.subtitle = element_text(size = 18, face = "bold")) +
  geom_text(data = filter(test, date == "2022-12-01"),
            aes(label = ethnicity),
            hjust = 0,  
            position=position_jitter(width=1.5,height=3) ) +
  coord_cartesian(clip = 'off') +
  # geom_dl(aes(label = ethnicity), method = list(dl.combine( "last.points")), cex = 0.8) +
   theme(legend.position = 'none',
         plot.margin = margin(0.5, 2.6, 0.1, 0.1, "cm")) +
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size=15) )
p_ethn 


  

# get the cumulative sums for one year
df_cum_sums <- df_dates_totals %>% 
  filter(Year == "2022") %>% 
  summarise(totals = sum(numbers), .groups = "drop") %>% 
  # mutate(date = make_date(Year, Month)) %>% 
  mutate(date = ymd(paste(Year, Month, "01", sep = "-"))) %>% 
  arrange(date) %>% 
  mutate(cumulative_sum = cumsum(totals)) %>% 
  mutate(perc_cum_sum = cumulative_sum/sum(totals))


plot(df_cum_sums$date, df_cum_sums$perc_cum_sum) # doesn't look informative

  

