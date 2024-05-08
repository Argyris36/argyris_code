
library(tidyverse)

# Data from https://www.unodc.org/documents/data-and-analysis/gsh/2023/Global_study_on_homicide_2023_web.pdf
# https://en.wikipedia.org/wiki/Crime_drop#cite_note-unodc-data-10

Overall_crime_rate <- c(21.3, 1.1, 8.8, 3)
Female_Rate <- 0.07*Overall_crime_rate
Male_Rate <- Overall_crime_rate - Female_Rate

data <- data.frame(
  Country = c("Brazil now", "Greece now", "Europe in the 90ies", "Europe now"),
  Male_Rate = Male_Rate
)

# Females are about 7% of the total (see pages 150 and 151 of UNODC report)
data$Female_Rate <- Female_Rate



# make long
data_long <- tidyr::pivot_longer(data, cols = c(Male_Rate, Female_Rate),
                                 names_to = "Gender", values_to = "Rate")

# plot
ggplot(data_long, aes(x = Country, y = Rate, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("pink", "blue")) +
  labs(title = "Ανθρωποκτονίες ανά 100,000 κατοίκους",
       subtitle = "ανά γεωγραφική περιοχή και φύλο",
       y = "Ποσοστό επί 100,000 κατοίκων",
       x = "Γεωγραφική Περιοχή ") +
  theme_minimal()
