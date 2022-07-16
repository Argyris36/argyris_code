# gant chart
library(reshape2)
library(ggplot2)



milestones <- c( 
  
  "Recruit YP Advisory Group",  
  "Co-design WP2/3", 
  "Co-deliver WP2/3",  
  "YP project on surprises in therapy",
  
  
  "Adapt platforms/recruit", 
  "Longitudinal Data collection ", 
  "Predictive Modeling", 
  
  
  "Tool optimisation",  
  "Physiological Data collection",  
  "Computational modeling") 


df_gantt <- data.frame(
  name = factor(milestones, levels = milestones),
  start.date = as.Date(c(
    "2023-01-01", "2023-06-01", "2023-12-31", "2024-01-01","2023-01-01" , "2024-01-01", "2027-01-01", 
    "2023-01-01", "2024-01-01", "2027-03-01" 
  )),
  end.date    = as.Date(c("2023-06-01",  "2023-12-31", "2028-01-01", "2027-01-01", "2024-01-01",
                          "2027-03-01", "2028-01-01",  "2024-01-01", "2027-05-01", "2028-01-01"
  )),
  phases = c(rep("WP 1: Learning from Lived Experience", 4), rep("WP 2: Treatment Prediction", 3), rep("WP 3: Mechanism Manipulation", 3))
)

df_gantt <- melt(df_gantt, measure.vars = c("start.date", "end.date"))

colnames(df_gantt) <- c("name", "Work_Packages", "beginning_end", "value") # change name



gantt_chart <- ggplot(df_gantt, aes(value, forcats::fct_rev(name), colour = Work_Packages)) + 
  geom_line(size = 6) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("Surprise Signals as a Mechanism of Change in Psychological Therapy") +
  theme(axis.title.y = element_text((size = 1))) +labs(colour= "Work Packages")








gant_chart <- gantt_chart + scale_x_date(date_breaks = "4 month", date_labels =  "%b %Y") + theme(axis.text.x=element_text(angle=45, hjust=1)) + theme(axis.text.y=element_text(angle=45, hjust=1)) + theme(legend.position=c(0.8, 0.895))
gant_chart

gant_chart <- gant_chart + scale_fill_discrete(name = "New Legend Title")
gant_chart #+                                          # Add panel border to ggplot2 plot
  #theme(panel.border = element_rect(color = "#1b98e0",
   #                                 fill = NA,
    #                                size = 10))
#annotation <- data.frame(
# x = c(2023-12-05,4.5),
# y = c(20,25),
#  label = c("label 1", "label 2")
#)

