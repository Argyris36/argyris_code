
# student mental health referrals -----------------------------------------
library(tidyverse)
library(lubridate)
library(ggrepel)

# data I exctracted from Rob's excel sent on 16th Dec 2023
df_student_mh_refs_by_ethnicity <- read_excel("df_student_mh_refs_by_ethnicity.xlsx")

# turn to long
long_df_student_mh_refs_by_ethnicity <- df_student_mh_refs_by_ethnicity %>% 
pivot_longer(cols =c(  "White" ,  "Mixed"  , "Asian" ,  "Black"  , "Chinese", "Other" ,  "Missing"), names_to = "ethnicity", 
             values_to = "numbers")


df_dates_totals <- long_df_student_mh_refs_by_ethnicity %>% 
  group_by(Year, Month) %>% 
  mutate(total_n_year_month = colSums(across(is.numeric)), 
         date = ymd(paste(Year, Month, "01", sep = "-"))) 

colnames(df_dates_totals)
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
  scale_x_date(date_breaks = "2 month", date_labels =  "%b") +

  theme_classic() +
  theme(axis.text.x=element_text(angle=60, hjust=1, size = 12)) 

p +
  annotate("text", x = df_labels$dates_labels, 
           y = 200, label = df_labels$labels, size = 6 )+
  ylab("Total Stuent Referrals Per Year")+
  xlab("Months of the Year") +
  ggtitle("Student Referrals to IAPT-Services by Year and Month")+
  theme(plot.title = element_text(size = 20, face = "bold"), 
        plot.subtitle = element_text(size = 18, face = "bold"))+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size=15) )
  

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
  geom_text(data = filter( df_dates_totals_missing , date == "2022-12-01"),
            aes(label = ethnicity),
            hjust = 0,  
            position=position_jitter(width=1.5,height=4) ) +
  coord_cartesian(clip = 'off') +
  # geom_dl(aes(label = ethnicity), method = list(dl.combine( "last.points")), cex = 0.8) +
   theme(legend.position = 'none',
         plot.margin = margin(0.5, 2.6, 0.1, 0.1, "cm")) +
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size=15) )
p_ethn 





# get the cumulative sums for one year
years <- unique (df_dates_totals$Year)
df_cum_sums <- list()
for (i in 1: length(years)){

df_cum_sums[[i]] <- df_dates_totals %>%
  filter(Year ==  years[i]) %>%
  summarise(totals = sum(numbers), .groups = "drop") %>%
  # mutate(date = make_date(Year, Month)) %>%
  mutate(date = ymd(paste(Year, Month, "01", sep = "-"))) %>%
  arrange(date) %>%
  mutate(cumulative_sum = cumsum(totals)) %>%
  mutate(perc_cum_sum = cumulative_sum/sum(totals))
}

# tried it for one of them, doesn't look informative

test<- df_cum_sums[[1]] %>% 
  ggplot(aes(x = date, y = perc_cum_sum))+
  geom_line(colour = "red")



