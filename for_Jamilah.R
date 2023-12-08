
# some code for Jamilah's PISA project ------------------------------------

###note you will need to rerun some of it e.g. chi squares with the stratification weights
### for which you will need to use the survey package
## I have also made a note at the lmes below where to add weights.

###########
 library(tidyverse)
 library(readxl)
 library(lme4)
 
df_for_argyris_30percent <- read_excel("~/Downloads/df_for_argyris_30percent.xlsx")


# inspect dimensions and missing on irritability --------------------------

# dimensions per country
dims_per_country <- 
  df_for_argyris_30percent %>% 
  split(.$CNTRYID) %>% 
map_df(~ dim(.)) 
 
# merge with irritability missingness
dims_per_country <- data.frame(rbind(dims_per_country, df_for_argyris_30percent %>%   
  split(.$CNTRYID) %>% 
map_dbl(~ sum(is.na(.$WB154Q05HA)) ) ) )


row.names(dims_per_country) <- c("row", "cols", "missing_irrit")

dims_per_country



# graph irritabilty by country -------------------------------------------------------
# create summary stats by country
sum_df <- df_for_argyris_30percent %>% 
  drop_na() %>% 
  group_by(CNTRYID, WB154Q05HA) %>% 
  summarise(n = n()) %>% 
  mutate(percent = 100 * n / sum( n ) ) %>% 
  mutate(binary = case_when(WB154Q05HA <4 ~ "low", WB154Q05HA >=4 ~ "high"))  %>% 
  group_by(CNTRYID, binary) %>% 
  mutate(percentage_high = sum(percent))
sum_df  


# create the text for percentage above 3
high_percentages <- sum_df %>% 
  filter(binary=="high"& row_number()==1)

# in order to re-order the variable I had to do this trick that creates new variables for both cntry ids
sum_df$CNTRYID_2 <- factor(sum_df$CNTRYID, levels=high_percentages[order
                                                                   (high_percentages$percentage_high),]$CNTRYID)  

high_percentages$CNTRYID_2  <- factor(high_percentages$CNTRYID, levels=high_percentages[order
                                                                                        (high_percentages$percentage_high),]$CNTRYID)


# now create a factor and assign labels to the numerical values for irritability
sum_df$WB154Q05HA_2 <- as.factor(sum_df$WB154Q05HA) 
levels(sum_df$WB154Q05HA_2) <-  c("rarely or never",           ##### from the PISA codebook
                             "every month",
                             "every week",
                             "more than once a week",
                             "every day")  


# the plot ordered by the sum of the percentages of the top two categroies (more than once a week and every day)
# with percentages within each bar
sum_df %>%   
  ggplot(aes(x = WB154Q05HA_2, y = percent ))   +
  geom_bar(stat = "identity") +
  geom_text(
    data    = sum_df,
    mapping = aes(x = WB154Q05HA, y = percent, label = round( percent, 1),
    ),
    # hjust   = -0.32,
    vjust   = 1.2, 
    colour = "white", size = 2.5
  ) +
  ggtitle("Irritability frequency by country")+
  
  geom_text(
    data    = high_percentages,
    mapping = aes(x = -Inf, y = -Inf, label = paste0("high irritability = ", round( percentage_high, 1),
                                                     "%")),
    hjust   = -0.30,
    vjust   = -11, colour = "grey"
  )+
  
  facet_wrap(~CNTRYID_2) + 
  xlab("")+
  ylab("percentage") +
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1)) +
  theme(panel.background  = element_blank())


# Assess contribution of country effects to irritability ------------------

# lme to test whether country level variation substantial 
# this is the null model, with country only in the random effects.
mod_lme_irrit_null <- lmer(WB154Q05HA ~1 + (1|CNTRYID), data = df_for_argyris_30percent)
summary(mod_lme_irrit_null)

# estimate the ICC for this, which tells you proportion of variance due to countries
if (!require(performance)) install.packages('performance')
library(performance)
performance::icc(mod_lme_irrit_null)
# this is very little of the variance explained at the country level, LMEs redundant.

# and all seems to be done right as we are getting a variance close to what JB Pingault got for life statisfaction
# namely about 5 %--see below
mod_lme_lifesat_null <- lmer(ST016Q01NA ~1 + (1|CNTRYID), data = df_for_argyris_30percent)
summary(mod_lme_lifesat_null)

# estimate the ICC for this, which tells you proportion of variance due to countries
if (!require(performance)) install.packages('performance')
library(performance)
performance::icc(mod_lme_lifesat_null)



# effects of gender -------------------------------------------------------
df_for_argyris_30percent$WB154Q05HA <- as.factor(df_for_argyris_30percent$WB154Q05HA) 
levels(df_for_argyris_30percent$WB154Q05HA) <-  c("rarely or never",           ##### from the PISA codebook
                             "every month",
                             "every week",
                             "more than once a week",
                             "every day")  


df_for_argyris_30percent$ST004D01T <- as.factor(df_for_argyris_30percent$ST004D01T) 
levels(df_for_argyris_30percent$ST004D01T) <-  c("female",           ##### from the PISA codebook
                                                  "male")  


# overall
prop.table(with(df_for_argyris_30percent, table(ST004D01T, WB154Q05HA)))
chisq.test(with(df_for_argyris_30percent, table(ST004D01T, WB154Q05HA)))


# gender and irritability across countries
# this loop gives you a list of the chisquare summaries
cntryids <- unique(df_for_argyris_30percent$CNTRYID)
chi_sq_per_country <- list()
chi_sq_value <- 0
df_value <- 0
p_value <- 0
n_by_gender <- list()
percent_by_gender <- list()
frequencies_by_gender <- list()


for (i in 1: length(cntryids)){
chi_sq_per_country[[i]]  <-chisq.test(subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$WB154Q05HA, 
                                      subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$ST004D01T)
names(chi_sq_per_country)[i] <- cntryids[i]
chi_sq_value[i] <- round(chisq.test(subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$WB154Q05HA, 
           subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$ST004D01T)$statistic[[1]], 2)

df_value[i] <- chisq.test(subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$WB154Q05HA, 
                              subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$ST004D01T)$parameter[[1]]

p_value[i]  <- chisq.test(subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$WB154Q05HA, 
                                         subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$ST004D01T)$p.value[[1]]


n_by_gender[[i]] <-  table(subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$WB154Q05HA, 
      subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$ST004D01T)

names(n_by_gender)[i] <- cntryids[i]

percent_by_gender[[i]] <- prop.table(table(subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$WB154Q05HA, 
                                      subset(df_for_argyris_30percent, CNTRYID == cntryids[i],)$ST004D01T))

names(percent_by_gender)[i] <- cntryids[i]

frequencies_by_gender[[i]] <- cbind( data.frame(percent_by_gender[[i]]), n = data.frame(n_by_gender[[i]])$Freq, country = cntryids[i])

colnames(frequencies_by_gender[[i]]) <- c("levels_irrit", "gender", "percent", "n" , "country")

names(frequencies_by_gender)[i] <- cntryids[i]

}

df_chi_sq_by_country <- data.frame(cbind(cntryids, chi_sq_value, df_value, p_value))

df_chi_sq_by_country$country <- df_chi_sq_by_country$cntryids

frequencies_by_gender <- data.frame(do.call("rbind", frequencies_by_gender))

df_irrit_gender_stats <- merge(frequencies_by_gender, df_chi_sq_by_country, by = "country")
df_irrit_gender_stats  <- df_irrit_gender_stats %>% 
select(-c(cntryids))

#### sum ns for the graph
n_for_plots_gender <- df_irrit_gender_stats %>% 
  group_by(country) %>% 
  summarise(n_total = sum(n))

df_irrit_gender_stats <- merge(df_irrit_gender_stats, n_for_plots_gender )

######## graph irritability by gender 
plot_irrit_gender_stats <- df_irrit_gender_stats %>% 
  ggplot(aes(x = levels_irrit, y = percent, fill = gender)) +
  geom_bar(stat = "identity")+
  facet_wrap(~country) +
  xlab("")+
  ylab("percentage") +
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1)) +
  theme(panel.background  = element_blank())
plot_irrit_gender_stats 
# 
# paste0("n = ", n_total, 
#        ", χ2 = ", round( as.numeric(chi_sq_value), 1), ", df = 4"

plot_irrit_gender_stats +
geom_text(
  data    = df_irrit_gender_stats,
  mapping = aes(x = -Inf, y = -Inf, label = paste0("χ2(4, ", n_total,") = " , round(as.numeric(chi_sq_value), 1) 
                                                   )),
  hjust   = -0.45,
  vjust   = -17, colour = "grey", size = 3)+
ggtitle("irritability by country and gender in adolescents", subtitle = "statistics for gender by irritability (all p-values significant at corrected p<0.005)") +
   scale_fill_brewer(palette = "Paired")


# Relationship between irritability and other variables -------------------
library(tidyverse)

df_for_argyris_30percent <- read_excel("~/Downloads/df_for_argyris_30percent.xlsx")

df_for_argyris_30percent_reduced <- df_for_argyris_30percent %>% 
  dplyr:: select (c
          (CNTSTUID, CNTRYID, WB154Q01HA,
            WB154Q02HA,
            WB154Q03HA,
            WB154Q04HA,
            WB154Q05HA ,
            WB154Q06HA,
            WB154Q07HA ,
            WB154Q08HA,
            WB154Q09HA)
  )



df_for_argyris_30percent_reduced <- 
  df_for_argyris_30percent_reduced %>% 
  rename(
    "headache" ="WB154Q01HA",
    "stomach" = "WB154Q02HA" ,
    "back" = "WB154Q03HA",
    "depressed" = "WB154Q04HA",
    "irritability" = "WB154Q05HA",
    "nervous" = "WB154Q06HA",
    "sleep" = "WB154Q07HA",
    "dizzy" = "WB154Q08HA",
    "anxious" = "WB154Q09HA" 
  )


countries <- unique(df_for_argyris_30percent_reduced$CNTRYID)

library(metan) # https://tiagoolivoto.github.io/metan/reference/corr_ci.html


#prepare symptom list for later
symptoms <- colnames(df_for_argyris_30percent_reduced[, 3:11 ])
symptoms <- symptoms[symptoms !="irritability"]

df_list<-list()

for(i in 1: length(unique(df_for_argyris_30percent_reduced$CNTRYID))){
df_list[[i]] <-  data.frame(metan::corr_ci(df_for_argyris_30percent_reduced[df_for_argyris_30percent_reduced$CNTRYID==countries[i], ]) ,
country = countries[i])
df_list[[i]] <-df_list[[i]] %>% 
  filter(V1=="irritability" | V2=="irritability") %>% 
  filter(V1!="CNTSTUID")
for (j in 1: length(df_list)){
  df_list[[j]]$symptoms <-symptoms
}

}

df_correlations_with_ci_irritability_other_symps <- do.call("rbind",df_list)

###### now graph it like above but with confidence intervals
df_correlations_with_ci_irritability_other_symps %>% 
ggplot(aes(x=symptoms, y=Corr)) + 
  geom_line(aes(group = 1)) + 
  geom_point()+
  geom_errorbar(aes(ymin=Corr-CI, ymax=Corr+CI), width=.1, 
                position=position_dodge(0.05)) +
  #scale_color_brewer(palette="Paired")+theme_minimal()+
  facet_wrap(~country) +
  xlab("")+
  ylab("coefficients with 95% CIs") +
  theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1))+
  theme(panel.background  = element_blank())+
  ggtitle("Association of adolescent irritability with other symptoms by country", subtitle = "Pearson's correlation coefficients with 95% CIs")+
  scale_fill_brewer(palette = "Paired") +
  geom_text(
    data    = df_irrit_gender_stats,
    mapping = aes(x = -Inf, y = -Inf, label = paste0("n = " , n_total), 
    ),
    hjust   = -0.45,
    vjust   = -17, colour = "grey", size = 3)


#### add ns to the graphs
n_for_plots_gender <- df_irrit_gender_stats %>% 
  group_by(country) %>% 
  summarise(sum(n))



#### do the linear regression coefficient comparisons

### do the association with other variables in LME.


# some variable creation  -------------------------------------------------
df_for_argyris_30percent$irritability <-  as.factor(df_for_argyris_30percent$WB154Q05HA) 
levels(df_for_argyris_30percent$irritability) <-  c("rarely or never",           ##### from the PISA codebook
                                                    "every month",
                                                    "every week",
                                                    "more than once a week",
                                                    "every day")  

df_for_argyris_30percent$life_satisfaction <- df_for_argyris_30percent$ST016Q01NA

df_for_argyris_30percent$gender <- as.factor(df_for_argyris_30percent$ST004D01T) 

levels(df_for_argyris_30percent$gender) <- c("female", "male")

# histogram of main variables ---------------------------------------------

# Define the list of variables for which histograms will be created
variables <- c("ESCS", "combined_scale_bullying",
                "life_satisfaction")


#open PDF
pdf("histograms_other_variables.pdf")

# Create a list to store plots
histogram_plots <- list()

# Iterate over variables and create histograms
for (i in variables) {
  # Create histogram for the current variable
  plot <- ggplot(df_for_argyris_30percent, aes(x = .data[[i]])) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = paste("Histogram of", i),
         x = i, y = "Frequency") +
    theme_minimal() 
  
  # Store the plot in the list
  histogram_plots[[i]] <- plot
}

# Print  histograms
for (i in variables) {
  print(histogram_plots[[i]])
}

dev.off()

########### NOTE: variables seem reasonable, but they do have a skew. It should not present a problem for the models, 
########### we should see what others have done e.g. for attainment in terms of analyses. 



# plot association betwen irritability and other variables ----------------


#  outcome variables
y_vars <- c("ESCS", "combined_scale_bullying",
            "life_satisfaction")


# Open PDF device to start saving plots
pdf("irritability_other_variables_by_country_and_gender.pdf")



# Iterate over outcome variables and create separate plots for each
for (y_var in y_vars) {
  plot <- df_for_argyris_30percent %>% 
    drop_na() %>% 
    ggplot(aes(x = irritability, y = .data[[y_var]], fill = factor(gender))) +
    geom_boxplot() +
    facet_wrap(~CNTRYID) +
    labs(title = paste("Relationship between irritability and", y_var),
         x = "Irritability", y = y_var) +
    theme(axis.text.x.bottom =  element_text(angle = 45, size = 8, vjust = 0.9, hjust = 1)) +
    theme(panel.background  = element_blank())+
    scale_fill_brewer(palette = "Paired")
  
  # Save the plot in the PDF file
  print(plot)
}

# Close the PDF device to finish saving plots
dev.off()




# Are country REs necessary--ICC will help decide--------


# List of variables you want to iterate over
variables_of_interest <- c("ESCS",  "combined_scale_bullying",
                           "life_satisfaction" 
)



# Iterate over the variables and fit mixed-effects models
icc_results <- list()
model <- list()
for (i in variables_of_interest) {
  # Fit a linear mixed-effects model
  model[[i]] <- lmer(get(i) ~ 1  + (1 | CNTRYID), data = df_for_argyris_30percent)
  
  # Calculate ICC from the model
icc_results[[i]] <- performance::icc(model[[i]])
  

}

# Print or use icc_results as needed
print(icc_results)



########### All well above 1% it makes sense to have REs



# Now run the LMEs --------------------------------------------------------


####### the lmes

outcome_variables <- c("ESCS", "combined_scale_bullying", "ST016Q01NA")

# create empty vectors
mod_null <- list()
mod_rand_int <- list()
mod_rand_slopes <- list()
mod_rand_slopes_gender <- list()
mod_rand_slopes_gender_interact_fixed_rinterc <- list()
mod_rand_slopes_gender_interact_fixed_rslope <- list()

best_models <- list()  # Initialize best_models list outside the loop

# Define the models
for (i in outcome_variables) {
  mod_null[[i]] <- lmer(get(i) ~ 1 + (1 | CNTRYID), data = df_for_argyris_30percent, REML = FALSE, 
                        control = lmerControl(optimizer = "bobyqa"))
  mod_rand_int[[i]] <- lmer(get(i) ~ WB154Q05HA + (1 | CNTRYID), data = df_for_argyris_30percent, REML = FALSE, 
                            control = lmerControl(optimizer = "bobyqa"))
  mod_rand_slopes[[i]] <- lmer(get(i) ~ WB154Q05HA + (WB154Q05HA | CNTRYID), data = df_for_argyris_30percent, REML = FALSE, 
                               control = lmerControl(optimizer = "bobyqa"))
  mod_rand_slopes_gender[[i]] <- lmer(get(i) ~ WB154Q05HA + gender + (1 | CNTRYID), data = df_for_argyris_30percent, REML = FALSE, 
                                      control = lmerControl(optimizer = "bobyqa"))
  mod_rand_slopes_gender_interact_fixed_rinterc[[i]] <- lmer(get(i) ~ WB154Q05HA * gender + (1 | CNTRYID), 
                                                             data = df_for_argyris_30percent, REML = FALSE, 
                                                             control = lmerControl(optimizer = "bobyqa"))
  mod_rand_slopes_gender_interact_fixed_rslope[[i]] <- lmer(get(i) ~ WB154Q05HA * gender + (WB154Q05HA | CNTRYID), 
                                                            data = df_for_argyris_30percent, REML = FALSE, 
                                                            control = lmerControl(optimizer = "bobyqa"))
  
  # Create a list of models for the current outcome variable
  models <- list(mod_null = mod_null[[i]], 
                 mod_rand_int = mod_rand_int[[i]], 
                 mod_rand_slopes = mod_rand_slopes[[i]], 
                 mod_rand_slopes_gender = mod_rand_slopes_gender[[i]], 
                 mod_rand_slopes_gender_interact_fixed_rinterc = mod_rand_slopes_gender_interact_fixed_rinterc[[i]], 
                 mod_rand_slopes_gender_interact_fixed_rslope = mod_rand_slopes_gender_interact_fixed_rslope[[i]])
  
  # Find the model with the lowest AIC for the current outcome variable
  best_model <- models[[names(models)[which.min(sapply(models, AIC))]]]
  
  # Store the best model for the current outcome variable
  best_models[[i]] <- best_model
  
  # Extract summary statistics for the best model (if needed)
  # summary_stats <- summary(best_model)
}

library(memisc)
list_coeffs_df <- list ()
for(i in 1: length(best_models)){
  
list_coeffs_df [[i]] <- data.frame(summary(best_models[[i]])$coefficients ) 



}
names(list_coeffs_df) <- outcome_variables

library(openxlsx)

# Create a new Excel workbook
wb <- createWorkbook()

for (i in 1:length(list_coeffs_df)) {
  addWorksheet(wb, sheetName = paste0("Sheet", i))
  writeData(wb, sheet = i, x = list_coeffs_df[[i]])
}

# Save the Excel workbook
saveWorkbook(wb, file = "coefficents_lmer.xlsx", overwrite = TRUE)


# network analyses --------------------------------------------------------
df_for_argyris_30percent <- read_excel("~/Downloads/df_for_argyris_30percent-2.xlsx")

pisa_net <- df_for_argyris_30percent %>% 
  select (c
          (CNTRYID, WB154Q01HA,
            WB154Q02HA,
            WB154Q03HA,
            WB154Q04HA,
            WB154Q05HA ,
            WB154Q06HA,
            WB154Q07HA ,
            WB154Q08HA,
            WB154Q09HA,
            SENWT)
  )

pisa_net <- 
  pisa_net %>% 
  rename(
    "headache" ="WB154Q01HA",
    "stomach" = "WB154Q02HA" ,
    "back" = "WB154Q03HA",
    "depressed" = "WB154Q04HA",
    "irritability" = "WB154Q05HA",
    "nervous" = "WB154Q06HA",
    "sleep" = "WB154Q07HA",
    "dizzy" = "WB154Q08HA",
    "anxious" = "WB154Q09HA" 
  )

################################ missing data - listwise deletion
pisa_net <- na.omit(pisa_net) 
table(pisa_net$CNTRYID)

################################################# Fused network (random sample )
#s_B <- filter(pisa_net, CNTRYID == "Bulgaria") %>%
# select(headache, stomach, back, depressed, irritability, nervous, sleep, dizzy, anxious) %>%
#cor()
#s_G <- filter(pisa_net, CNTRYID == "Georgia") %>%
#select(headache, stomach, back, depressed, irritability, nervous, sleep, dizzy, anxious) %>%
#cor()
#s_H <- filter(pisa_net, CNTRYID == "Hong Kong") %>%
#select(headache, stomach, back, depressed, irritability, nervous, sleep, dizzy, anxious) %>%
#cor()
#s_I <- filter(pisa_net, CNTRYID == "Ireland") %>%
#select(headache, stomach, back, depressed, irritability, nervous, sleep, dizzy, anxious) %>%
#cor()
#s_M <- filter(pisa_net, CNTRYID == "Mexico") %>%
# select(headache, stomach, back, depressed, irritability, nervous, sleep, dizzy, anxious) %>%
# cor()
#s_P <- filter(pisa_net, CNTRYID == "Panama") %>%
# select(headache, stomach, back, depressed, irritability, nervous, sleep, dizzy, anxious) %>%
# cor()
#s_S <- filter(pisa_net, CNTRYID == "Serbia") %>%
#  select(headache, stomach, back, depressed, irritability, nervous, sleep, dizzy, anxious) %>%
#  cor()
#s_SP <- filter(pisa_net, CNTRYID == "Spain") %>%
#  select (headache, stomach, back, depressed, irritability, nervous, sleep, dizzy, anxious) %>%
#  cor()
#s_U <- filter(pisa_net, CNTRYID == "United Arab Emirates") %>%
#  select(headache, stomach, back, depressed, irritability, nervous, sleep, dizzy, anxious) %>%
#  cor()

#Jamilah's Modified code to incorporate weighted variable 
library(psych)


# Assuming pisa_net is your data frame
weighted_cor <- function(country) {
  country_data <- filter(pisa_net, CNTRYID == country) %>%
    select(headache, stomach, back, depressed, irritability, nervous, sleep, dizzy, anxious)
  
  weights <- filter(pisa_net, CNTRYID == country) %>%
    select(SENWT) %>%
    unlist()  # Convert the data frame to a vector
  
  if (length(weights) != nrow(country_data)) {
    stop("Length of 'SENWT' does not equal the number of rows in the data.")
  }
  
  weighted_cor_matrix <- cov.wt(country_data, wt = weights)$cov
  
  return(weighted_cor_matrix)
}

# Example usage
s_B <- weighted_cor("Bulgaria")
s_G <- weighted_cor("Georgia")
s_H <- weighted_cor("Hong Kong")
s_I <- weighted_cor("Ireland")
s_M <- weighted_cor("Mexico")
s_P <- weighted_cor("Panama")
s_S <- weighted_cor("Serbia")
s_SP <- weighted_cor("Spain")
s_U <- weighted_cor("United Arab Emirates")

table(pisa_net$CNTRYID)

library(EstimateGroupNetwork)
network1 <- EstimateGroupNetwork(list("Bulgaria" = s_B,
                                      "Georgia" = s_G,
                                      "HongKong" = s_H,
                                      "Ireland" = s_I,
                                      "Mexico" = s_M,
                                      "Panama" = s_P,
                                      "Serbia" = s_S,
                                      "Spain" = s_SP,
                                      "UnitedArabEmirates" = s_U),
                                 n = c(1081, 1215, 1519, 1498, 1779, 1351, 1370, 7904, 4832))

#lavbels for network graphs/centrality plots
shortnames <- c("headache", "stomach", "back", "depressed", "irritability", "nervous", "sleep", "dizzy", "anxious")

#install.packages(c("qgraph", "igraph", "ggraph", "ggplot2", "RColorBrewer"))  # Uncomment and run if you haven't installed the packages
library(qgraph)
library(igraph)
library(ggraph)
library(ggplot2)
library(RColorBrewer)

# Define the list of countries
countries <- c("Bulgaria", "Georgia", "HongKong", "Ireland", "Mexico", "Panama", "Serbia", "Spain", "UnitedArabEmirates")

# Set the PDF filename
pdf_filename <- "network_visualizations.pdf"

# Open the PDF file
pdf(pdf_filename, width = 14, height = 8)

# Loop through each country
for (country in countries) {
  # Extract the network for the current country
  current_network <- network1[[country]]
  
  # Convert the adjacency matrix to an igraph object
  g <- graph_from_adjacency_matrix(current_network, mode = "undirected", weighted = TRUE)
  
  # Create a layout for the plot
  layout <- layout_with_fr(g)
  
  # Plot the network using ggraph and ggplot2 with blue colors
  print(ggraph(g, layout = layout) +
          geom_edge_link(aes(color = weight, edge_width = 1),  arrow = arrow(length = unit(0.25, "cm")), lineend = "round") +
          geom_node_point(size = 5, color = "blue") +
          geom_node_text(aes(label = name), colour = "red", size = 5, vjust = 1.5) +  # Add symptom labels
          scale_color_gradient(low = "blue", high = "blue") +  # Use blue color
          theme_void() +
          ggtitle(paste("Figure 3. Network Visualization:", country)))
}

# Close the PDF file
dev.off()





