library("readxl")
library("tidyverse")
library(openxlsx)

aml <- read_excel("~/Downloads/AML survivors relapsing _3y post IC, no allo in CR1 analysis.xlsx")


aml %>% 
  count(`Age diag`) 


aml$dic<- ifelse(aml$`Age diag` >=60,1,0  )

aml$dic<- factor(aml$dic, levels = c(0,1), labels = c("< 60", ">= 60"))

aml %>% 
  count(dic) %>% 
  mutate(perc = n/sum(n)*100)



aml$order <- order(aml$`Age diag`)


# Age graph
age <- aml %>% 
  ggplot(aes( x = order, y = `Age diag`, fill = factor(dic, levels = c( "< 60" , ">= 60"))  )) + 
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 60, linetype = "dashed") +
  annotate("text", x=8, y=80, label= "5/13 (38%) < 60 years", size = 6 )+
   xlab ("patients" )+
   ylab ("Age at diagnosis") +
  ggtitle ("Age of participants")

age + theme(
  plot.title = element_text(size=16, face="bold")) +
  theme(legend.title=element_blank()) +
  theme(
    axis.text.x = element_blank())


# Treatment graph
treatment <- aml %>%  
  count(Treatment) %>% 
  mutate(perc = n/sum(n)*100)

treatment %>% 
  ggplot(aes(Treatment, n) +
  geom_bar(stat = "identity")

#### same with percentages

treatment %>% 
  ggplot(aes(Treatment, perc, fill = Treatment)) +
  geom_bar(stat = "identity")+
  ggtitle("Treatment Allocation")+
  theme(
  plot.title = element_text(size=16, face="bold.italic"))


# Presenting white count

WBC_age <- aml %>% 
  group_by(`Age diag`) %>% 
  count(WCC)


wbc_by_age <-  WBC_age %>% 
  ggplot(aes(factor (`Age diag`), WCC)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label=WCC),  vjust=-0.25)  +
labs ( x = "Age")+
  ggtitle("WBC by Age")

wbc_by_age +
  theme(
  plot.title = element_text(size=16, face="bold.italic"))

# Length of CR1 by age

CR1_age <- aml %>% 
  group_by(`Age diag`) %>% 
  count(`Time to CR1`)

CR1_age

CR1_age %>% 
  ggplot(aes(factor (`Age diag`), `Time to CR1`)) +
  geom_bar(stat = "identity") +
  labs ( x = "Age") +
  ggtitle("Time to CR1 by Age")+
  theme(
    plot.title = element_text(size=16, face="bold.italic"))

# NPM1

npm1 <- aml %>% 
  count(NPM1) %>% 
  mutate(perc = n/sum(n)*100)

npm1 %>% 
  ggplot(aes(x = NPM1, y = perc, fill = NPM1))+
  geom_bar(stat = "identity") +
  ggtitle("NPM1 frequency")

theme(
  plot.title = element_text(size=16, face="bold.italic"))




under60 <- 511
over60 <- 1078 -511

306 - 5
567 8

observed_table <- matrix(c(430, 5, 279, 8), nrow = 2, ncol = 2, byrow = T)
observed_table
rownames(observed_table) <- c('under_60', 'over_60')
colnames(observed_table) <- c("no_relapse", "relapse")
observed_table

chisq.test(observed_table)


