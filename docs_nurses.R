library(tidyverse)

#import data
doc_nurses_per_cntry <-  read.csv("~/Downloads/doctors-per-capita-by-country-2024.csv")

# rename the two main columns
doc_nurses_per_cntry <-
  doc_nurses_per_cntry %>% 
  rename(med_docs = DoctorsPerCapitaDensityOfMedicalDoctors, nurse_midwif = 
           DoctorsPerCapitaDensityOfNursingAndMidwiferyPersonel)


# restrict analyses to western countries
western_countries <- c(
  "United Kingdom", "France", "Germany", "Italy", "Spain", "Greece",
  "Poland", "Ukraine", "Turkey", "Romania", "Netherlands", "Belgium",
  "Czech Republic", "Greece", "Portugal", "Sweden", "Hungary", "Belarus",
  "Austria", "Switzerland", "Bulgaria", "Denmark", "Finland", "Slovakia",
  "Norway", "Ireland", "Croatia", "Bosnia and Herzegovina", "Albania",
  "Slovenia", "Montenegro", "North Macedonia", "Kosovo", "Serbia",
  "Australia", "New Zealand"
)


doc_nurses_per_cntry_western <- 
  doc_nurses_per_cntry[doc_nurses_per_cntry$country %in% western_countries,]

# relabel Bosnia
doc_nurses_per_cntry_western$country <- as.factor(doc_nurses_per_cntry_western$country)

levels(doc_nurses_per_cntry_western$country)[levels(doc_nurses_per_cntry_western$country)=="Bosnia and Herzegovina"] <- "Bosnia & Herz"



# plot ratio of medics by nurses
doc_nurses_per_cntry_western %>% 
  ggplot(aes(x = country, y = med_docs/nurse_midwif)) +
  geom_bar(stat = "identity")+
  ylab("Ratio Medical Doctors to Nurses") +
  ggtitle("Ratio of Medical Doctors to Nurses Per Capita Per Country")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.01),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 14))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_text(aes( x = "New Zealand", y = 0.9, label = "more nurses than doctors"))+
  geom_text(aes( x = "New Zealand", y = 1.1, label = "fewer nurses than doctors"))+
  geom_text(aes( x = "Italy", y = 1.53, label = "Greece"), colour = "red")+
  geom_segment(aes(x = "Ireland", y = 1.5, xend = "Greece", yend = 1.4),
               arrow = arrow(length = unit(0.5, "cm")),  colour = "red")
  



# draw medics distribution
med_docs_Greece <- doc_nurses_per_cntry_western[
  doc_nurses_per_cntry_western$country =="Greece",]$med_docs

hist(doc_nurses_per_cntry_western$med_docs, xlab = "", main = "Distribution of Medical Doctors Per Capita")
abline(v = med_docs_Greece , col = "red", lty = 2, lwd = 2)

text(med_docs_Greece + 0.2, par("usr")[4]-2, "Greece", col = "red", pos = 2)


# draw nurse distribution
nurs_Greece <- doc_nurses_per_cntry_western[
  doc_nurses_per_cntry_western$country =="Greece",]$nurse_midwif

hist(doc_nurses_per_cntry_western$nurse_midwif, xlab = "", main = "Distribution of Nurses/Midwives Per Capita")
abline(v = nurs_Greece , col = "red", lty = 2, lwd = 2)

text(med_docs_Greece + 0.2, par("usr")[4]-2, "Greece", col = "red", pos = 2)


