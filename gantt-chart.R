# gant chart
library(reshape2)
library(ggplot2)



milestones <- c( 

"co-select materials",  
"focus groups", 
"thematic analysis",  


"create existing item list", 
"examine standard items", 
"examine new items",  
"content analysis", 


"prototyping",  
"collect/model pilot data",  
"collect final data",  
"model data",  
"test hypotheses") 


df_gantt <- data.frame(
         name = factor(milestones, levels = milestones),
  start.date = as.Date(c(
     "2022-11-01", "2022-12-02","2023-05-01","2022-12-01" , "2023-01-05", "2023-05-05", 
    "2023-10-05", "2022-11-01", "2023-03-15", "2023-12-10", "2024-04-01", "2024-6-01" 
  )),
  end.date    = as.Date(c("2022-12-01",  "2023-05-01", "2023-07-15", "2023-01-05",
                          "2023-05-01", "2023-10-05",  "2023-12-05", "2023-03-15", "2023-12-15","2024-04-01",
                           "2024-08-01","2024-10-01"
                          )),
  phases = c(rep("Work Package 1: Build Conceptual Basis", 3), rep("Work Package 2: Scrunitise Items", 4), rep("Work Package 3: Build Preference-based Tool", 5))
)

df_gantt <- melt(df_gantt, measure.vars = c("start.date", "end.date"))

colnames(df_gantt) <- c("name", "Work_Package", "beginning_end", "value") # change name



gantt_chart <- ggplot(df_gantt, aes(value, forcats::fct_rev(name), colour = Work_Package)) + 
  geom_line(size = 6) +
  xlab(NULL) + 
  ylab(NULL) +
  ggtitle("RADIANCE: Gantt Chart of Research Co-Production") +
  theme(axis.title.y = element_text((size = 1)))
  



gant_chart <- gantt_chart + scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y") + theme(axis.text.x=element_text(angle=60, hjust=1)) + theme(legend.position=c(0.7, 0.85))
gant_chart

#annotation <- data.frame(
 # x = c(2023-12-05,4.5),
 # y = c(20,25),
#  label = c("label 1", "label 2")
#)

# Add text
annotation <- data.frame(
  x = c(as.Date("2023-12-01"),as.Date("2023-12-25")),
  y = c(6.8,4.78),
  label = c("ADORE \n Stakeholder \n Summit", "ADEPT \n Stakeholder \n Summit")
)

gant_chart <- gant_chart + geom_text(data = annotation, aes( x=x, y=y, label=label),                 , 
                       color="orange", 
                       size=3 , angle=0, fontface="bold" )

gant_chart   + theme(legend.title = element_blank())
