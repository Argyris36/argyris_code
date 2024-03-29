library(tidyverse)
library(dplyr)
library(tidyr)
maria_grades <- read.csv("~/Downloads/Maria_term_marks.xlsx - Sheet1 (3).csv")



maria_grades_long <-
  
  maria_grades %>%
  
  #  dplyr::select(Term, art_p, art_r) %>%
  
  pivot_longer(grep("_", colnames(maria_grades)),
               
               names_to = c("subject", "source"),
               
               names_pattern = "(.+)_(.)",
               
               values_to = "grade")



dim(maria_grades_long)
tail(maria_grades_long)




maria_grades_long <- as.data.frame(c(lapply(maria_grades_long[-length(maria_grades_long)]
                                          , as.factor), maria_grades_long[length(maria_grades_long)]))



levels(maria_grades_long$source) <-  c("predicted", "actual")

#maria_grades_long$grade <- as.factor(maria_grades_long$grade)

maria_grades_long$grade <-
case_match(
  maria_grades_long$grade,
       2 ~ 5, 
       3 ~ 4,
       4 ~ 3, 
       5 ~ 2)

# levels(maria_grades_long$grade) <-  c(
#                                       "satisfactory", "improvament_needed",
#                                       "good",
#                                       "very good")



# present overall

maria_grades_long %>%
  
  filter(source == "actual" & subject != "compsci") %>%
  
  ggplot(aes(x = subject, y = grade, group = Term, color = Term)) +
  
  geom_line(position = position_dodge(width = 0.2), size = 0.5) +
  
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.2), size = 3) +
  

  
  ggtitle("Maria's Grade Progression 2023-2024")+
  
  theme(axis.title.x=element_blank(), 
        axis.text = element_text(angle = 45, size = 12), 
        plot.background = element_blank(),
        plot.title = element_text(size = 14))+
  scale_y_continuous(breaks = c(2,3,4,5),labels = function(x) ifelse(x == 2, "improvement needed", ifelse(x == 3, "satisfactory", 
                                        ifelse(x == 4, "good", ifelse(x == 5, "very good", as.character(x))))) )

  
  
# present as facet plot

maria_grades_long %>%
  
  filter(source == "actual" & subject != "compsci") %>%
  
  ggplot(aes(y = grade,x = Term)) +
  
  
  geom_point(aes(y = grade,x = Term, group = subject)) +
  
  geom_line(aes(y = grade,x = Term, group = subject))+
  
  
  ggtitle("Maria's Grade Progression 2023-2024")+
  
  theme(axis.title.x=element_blank(), 
        axis.text.x =  element_text(angle = 90, size = 12), 
        plot.background = element_blank(),
        plot.title = element_text(size = 14))+
  scale_y_continuous(breaks = c(2,3,4,5),labels = function(x) ifelse(x == 2, "improvement needed", ifelse(x == 3, "satisfactory", 
                                                                                                          ifelse(x == 4, "good", ifelse(x == 5, "very good", as.character(x))))) ) +
 xlim("Michaelmas_2023", 
                                           "Michaelmas_2024", 
                                           "Lent_2024")+
  facet_wrap(~subject)







# with predicted

maria_grades_long %>%
  
  filter( subject != "compsci", subject != "thrive") %>%
  
  ggplot(aes(y = grade,x = Term)) +
  
  
  geom_point(aes(y = grade,x = Term, group = source, colour = source),position = position_dodge(width = 0.2)) +
  
  geom_line(aes(y = grade,x = Term, group = source, colour = source),position = position_dodge(width = 0.2))+
  
  
  ggtitle("Maria's Grade Progression 2023-2024")+
  
  theme(axis.title.x=element_blank(), 
        axis.text.x =  element_text(angle = 90, size = 12), 
        plot.background = element_blank(),
        plot.title = element_text(size = 14))+
  scale_y_continuous(breaks = c(2,3,4,5),labels = function(x) ifelse(x == 2, "improvement needed", ifelse(x == 3, "satisfactory", 
                                                                                                          ifelse(x == 4, "good", ifelse(x == 5, "very good", as.character(x))))) ) +
  xlim("Michaelmas_2023", 
       "Michaelmas_2024", 
       "Lent_2024")+
  facet_wrap(~subject)

# create variable denoting change

maria_grades_long$change <- 
     ifelse(maria_grades_long[maria_grades_long$Term == "Michaelmas_2024",]$grade > 
         maria_grades_long[maria_grades_long$Term == "Lent_2024",]$grade, "worse", ifelse
       (maria_grades_long[maria_grades_long$Term == "Michaelmas_2024",]$grade < 
        maria_grades_long[maria_grades_long$Term == "Lent_2024",]$grade, "better", "same"))


maria_grades_long %>%
  
  filter(source == "actual" & subject != "compsci") %>%
  
  ggplot(aes(y = grade,x = Term, colour = change)) +
  
  
  geom_point(aes(y = grade,x = Term, group = subject)) +
  
  geom_line(aes(y = grade,x = Term, group = subject))+
  
  
  ggtitle("Maria's Grade Progression 2023-2024")+
  
  theme(axis.title.x=element_blank(), 
        axis.text.x =  element_text(angle = 90, size = 12), 
        plot.background = element_blank(),
        plot.title = element_text(size = 14))+
  scale_y_continuous(breaks = c(2,3,4,5),labels = function(x) ifelse(x == 2, "improvement needed", ifelse(x == 3, "satisfactory", 
                                                                                                          ifelse(x == 4, "good", ifelse(x == 5, "very good", as.character(x))))) ) +
  xlim("Michaelmas_2023", 
       "Michaelmas_2024", 
       "Lent_2024")+
  facet_wrap(~subject)


