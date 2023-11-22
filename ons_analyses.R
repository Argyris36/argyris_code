my_packages <- c("tidyverse", "srvyr", "survey", "broome", "lme4", "foreign", "stringr", "haven", "readr",
                 "sjlabelled", "openxlsx")
lapply(my_packages, require, character.only = TRUE)



####NOTE on import and data source


# I used Pablo's cleaning fromn his do sheet in stata, which looked good to me.
ons_data <- read_csv("N:/Projects/ons_epi/ons_data_from_stata_pablo_code.csv")

ons_data <- ons_data %>% 
  filter (ChldAge > 4) %>% # only those older than 4 and a binary adol variable
  mutate (adolescent = cut(ChldAge,breaks=c(5, 11, 19), labels=c('child', 'adolescent')))

ons_data$sex <- 
  add_labels(ons_data$ChldSx, labels = c('boy' = 0, 'girl' = 1))



##########################################################################################################
# SAMPLE CHARACTERISTICS

# declare child sex as ordered factor
ons_data$ChldSx <- factor(ons_data$ChldSx, levels=c(1,2), labels=c("1", "2"), NA, ordered = F) 



# declare ethnic5 as ordered factor
ons_data$ethnic5 <- factor(ons_data$ethnic5, levels=c(1:5), labels=c("white brit", "white oth", "black", "asian", "mixed"), NA, ordered = F) 
ethnic5_table <-table(ons_data$ethnic5)
prop.table(ethnic5_table)

# declare ethnic2 as ordered factor
ons_data$ethnic_bme <- factor(ons_data$ethnic_bme, levels=c(1:2), labels=c("non-bme", "bme"), NA, ordered = F) 
ethnic_bme_table <-table(ons_data$ethnic_bme)
prop.table(ethnic_bme_table)


######################






## Didn't do the all cleaning below as I used Pablo's cleaning for consistency

# 
# #several recodings from numeric to missing and  other recodes according to Pablo's stata file
# but did use the following just to make recoding easier and universal.
ons_data <- ons_data %>%
  mutate(across(where(is.numeric), ~na_if(., -1)))

ons_data <- ons_data %>%
  mutate(across(where(is.numeric), ~na_if(., -9)))

ons_data <- ons_data %>%
  mutate(across(where(is.numeric), ~na_if(., -8)))
# 
# 
# ons_data<- ons_data %>% 
#   mutate_at(vars(DVDrugIn, PoliceYP), ~ifelse(. == 1, 0, .))%>%
#               mutate_at(vars(DVDrugIn, PoliceYP), ~ifelse(. == 2, 1, .) )
# 
# 
# ons_data<- ons_data %>% 
#   mutate_at(vars(medstim, meddep, medmood, anypsychmed, exclsch, pypq21, help,
#                  Bullyexp, trutea, charm, pharm), ~ifelse(. == 2, 0, .))    
# 
# 
# ons_data<- ons_data %>% 
#   mutate_at(vars(dcany, dcemot, dcanyde, dcanyan, dcanyhk, dccd, dcodd,
#                  dcmania, dcdmdd, dcanycd, dcothcd), ~ifelse(. == 2, 1, .))    
# 
# # change all these to factor
# for_labels <- c('DVDrugIn', "PoliceYP", "medstim", "meddep", "medmood", "anypsychmed",
#   "exclsch", "pypq21", "help",
# "Bullyexp", "trutea", "charm", "pharm")
# 
# 
# ons_data[,for_labels] <- lapply(ons_data[, for_labels], factor)
# 
# 
# # also realised must change the -8s in the factors
# ons_data <- ons_data %>% 
#   mutate(across(where(is.factor), ~na_if(., -8)))
# 
# # create an any externalising variable
# ons_data <- ons_data %>% 
#   mutate(any_ext = ifelse(dcany, dcanycd==1 | dcanyhk==1 | dcodd==1,0) )
# 
# 




# use survey package instead because of difficulties with missing variables 

ons_data_design <- svydesign(id = ~ PSUa, weights = ~ calweight_resp, data = ons_data )



######### 1. SOME CUSTOM MADE FUNCTIONS ######### 

######### 1A. this is  a function for the proportions and 
######### SEs based on data.frame, based on 
######### (svyby(~SY1dic, ~sex  , design = ons_data_design, svymean, na.rm = T))

frequencies_func <- function (outcomes, cross_tab_var, data) {
  
  models <- vector("list", length = length(outcomes))
  
  for (i in 1: length(outcomes)){
    
    
    models[[i]] <- data.frame(svyby(~eval(parse(text = outcomes[[i]])), (~eval(parse(text = cross_tab_var))) , design = data, svymean, na.rm = T))
    
    colnames(models[[i]]) <- c("subs_var", "mean", "se") 
    names(models) <- outcomes
    
  }
  
  return(models)
  
}


######### 1B. A function that runs  regression models for age and sex interaction with an outcome, such as this 5-level variable. 
#### Do in the spirit of the above by running them through a loop. This will make the analyses scalable, so you don't have #### to copy and paste code all the time. 
#### As always, check this works before scaling. 
#### 
lin_reg_function <- function(outcomes, predictors, data, family){
  # the empty vector
  models_reg <- vector("list", length = length(outcomes))
  
  df_list_reg <- vector("list", length = length(outcomes))
  
  for(i in 1:length(outcomes)){
    
    
    models_reg[[i]] <- as.formula(paste(outcomes[i], paste(predictors, collapse = "+"),
                                        sep = "~"))
    
    df_list_reg[[i]] <- data.frame(summary(eval(bquote(svyglm(.(models_reg[[i]]), na.action = na.omit, family = family , data))))$coefficients)
    
    names(df_list_reg) <- outcomes
    
  }
  return(df_list_reg)
}




# 1C. This function will churn out means and SEs by group for survey designs.

func_means_survey <- function(vars_for_means, predictors, data){
  means_ses <- vector("list", length(vars_for_means))
  for (i in 1:length(vars_for_means)){
    
    means_ses[[i]] <- data.frame(svyby(as.formula(paste("", vars_for_means[[i]], sep = "~")), 
                                       as.formula(paste("", predictors, collapse = "+", sep = "~")), ons_data_design, svymean, na = T ))
    
    
  }
  return(means_ses)
}








#### Question 1: Prevalence of irritable mood and temper outbursts ########
# A. get frequencies for each category of the five-level screener item first for children and then for adolescents
# this is the CHILD dataset
child_sub <- subset(ons_data_design, adolescent == "child")

# declare the non dichotomous irritable mood and temper outburst variables by informant
non_dic_outcomes_child <- c("PY1", "PY2") # because we only have parent data for this age group

# loop over those variables to get the weighted frequencies
non_dic_child_freq<-list ()
for (i in 1:length(non_dic_outcomes_child)){
  
  non_dic_child_freq[[i]] <- data.frame(prop.table(svytable(reformulate(non_dic_outcomes_child[[i]]), child_sub)))

#names(non_dic_child_freq[[i]]) <- non_dic_outcomes[[i]]

colnames(non_dic_child_freq[[i]]) <- c(non_dic_outcomes_child[[i]], "freq")

}

# save these as 
openxlsx:: write.xlsx(non_dic_child_freq, file = "non_dic_child_freq.xlsx")

#### Question 1: Prevalence of irritable mood and temper outbursts ########
# B. get frequencies for each category of the five-level screener item first for children and then for adolescents
# this is the ADOLESCENT dataset

adolescent_sub <- subset(ons_data_design, adolescent == "adolescent")

# declare the non dichotomous irritable mood and temper outburst variables by informant
non_dic_outcomes_adol <- c("PY1", "PY2", "SY1", "SY2") # because we only have parent data for this age group

# loop over those variables to get the weighted frequencies
non_dic_adol_freq<-list ()
for (i in 1:length(non_dic_outcomes_adol)){
  
  non_dic_adol_freq[[i]] <- data.frame(prop.table(svytable(reformulate(non_dic_outcomes_adol[[i]]), adolescent_sub)))
  
  #names(non_dic_child_freq[[i]]) <- non_dic_outcomes[[i]]
  
  colnames(non_dic_adol_freq[[i]]) <- c(non_dic_outcomes_adol[[i]], "freq")
  
}
openxlsx:: write.xlsx(non_dic_adol_freq, file = "non_dic_adol_freq.xlsx")



#### Question 1: Prevalence of irritable mood and temper outbursts ########
# C. get frequencies for each category of the DICHOTOMOUS items  for children and then for adolescents
# use my custom-made frequency function

# this is the CHILD dataset
child_sub <- subset(ons_data_design, adolescent == "child")

# run the function and bind it together; NEVER MIND about using all four variables, we will ignore the self-report
dic_outcomes_child <- c("PY1dic", "PY2dic", "SY1dic", "SY2dic") 

dic_child_freq <- frequencies_func(outcomes = dic_outcomes_child, cross_tab_var = "sex", data =  child_sub)

dic_child_freq <- do.call(rbind, dic_child_freq)

dic_child_freq <- data.frame(cbind(dic_child_freq, outcome = rep(c("PY1dic", "PY2dic", "SY1dic", "SY2dic")
                                                         , each = 2),
                               informant = c(rep("parent",4), rep("self", 4))))

dic_child_freq$subs_var <- factor(ifelse(dic_child_freq$subs_var==0, "boy", "girl"))
dic_child_freq
openxlsx:: write.xlsx(dic_child_freq, file = "dic_child_freq.xlsx")


# this is the ADOLESCENT dataset
adolescent_sub <- subset(ons_data_design, adolescent == "adolescent")
# run the function and bind it together; NEVER MIND about using all four variables, we will ignore the self-report
dic_outcomes_adol <- c("PY1dic", "PY2dic", "SY1dic", "SY2dic") 

dic_adol_freq <- frequencies_func(outcomes = dic_outcomes_adol, cross_tab_var = "sex", data =  adolescent_sub)

dic_adol_freq <- do.call(rbind, dic_adol_freq)

dic_adol_freq <- data.frame(cbind(dic_adol_freq, outcome = rep(c("PY1dic", "PY2dic", "SY1dic", "SY2dic")
                                                                 , each = 2),
                                   informant = c(rep("parent",4), rep("self", 4))))

dic_adol_freq$subs_var <- factor(ifelse(dic_adol_freq$subs_var==0, "boy", "girl"))
dic_adol_freq
openxlsx:: write.xlsx(dic_adol_freq, file = "dic_adol_freq.xlsx")

# REMEMBER TO PLOT THESE






#### Question 1. Statistical evaluation of age and sex effects
####here we are using the linear regression function to get outcomes for children and adolescents

parent_outcomes <- c("PY1", "PY2")
parent_predictors <- c("sex*adolescent") # because we have these across childhood and adolescence



# use function for parent report across development
parent_age_sex_inter_reg <- lin_reg_function(outcomes = parent_outcomes, predictors = parent_predictors,
                                      data = ons_data_design, 
                                      family = "gaussian")

openxlsx:: write.xlsx(parent_age_sex_inter_reg, file = "parent_age_sex_inter_reg.xlsx", rowNames = TRUE)


# use function for self report for adolescents only (doesn't make sense to do in children)
self_outcomes <- c("SY1", "SY2")
self_predictors <- c("sex") # development doesn't make sense

self_sex_inter_reg <- lin_reg_function(outcomes = self_outcomes, predictors = self_predictors,
                                      data = subset(ons_data_design, adolescent == "adolescent"), 
                                                    family = "gaussian")

openxlsx:: write.xlsx(self_sex_inter_reg, file = "self_sex_inter_reg.xlsx", rowNames = TRUE)





###### Question 2. Descriptives of associations with psychopathology and impact
# Here we use my custom-made means function to get SDQs for each informant across many variables.

# within informant for self repeort
s_means_sdqs_SY1 <- func_means_survey(vars_for_means = c("semotion", "shyper", 
                                                         "sconduct", "simpact", "speer"),
                                      predictors = c("SY1dic + sex + adolescent"), 
                                      data = ons_data_design)

openxlsx:: write.xlsx(s_means_sdqs_SY1, file = "s_means_sdqs_SY1.xlsx", rowNames = TRUE)


# across informant for self repeort
p_means_sdqs_SY1 <- func_means_survey(vars_for_means = c("pemotion", "phyper_corrected", 
                                                         "pconduct", "pimpact", "ppeer"),
                                      predictors = c("SY1dic + sex + adolescent"), 
                                      data = ons_data_design)

openxlsx:: write.xlsx(p_means_sdqs_SY1, file = "p_means_sdqs_SY1.xlsx", rowNames = TRUE)


# within informant for parent repeort
p_means_sdqs_PY1 <- func_means_survey(vars_for_means = c("pemotion", "phyper_corrected", 
                                                         "pconduct", "pimpact", "ppeer"),
                                      predictors = c("PY1dic + sex + adolescent"), 
                                      data = ons_data_design)

openxlsx:: write.xlsx(p_means_sdqs_PY1, file = "p_means_sdqs_PY1.xlsx", rowNames = TRUE)


# across informant for parent repeort
s_means_sdqs_PY1 <- func_means_survey(vars_for_means = c("semotion", "shyper", 
                                                         "sconduct", "simpact", "speer"),
                                      predictors = c("PY1dic + sex + adolescent"), 
                                      data = ons_data_design)


openxlsx:: write.xlsx(s_means_sdqs_PY1, file = "s_means_sdqs_PY1.xlsx", rowNames = TRUE)



###### Question 2. Statistical evaluation  of associations with psychopathology and impact
####### use reg function for psychopathology and impact to demonstrate statistically the imapct

# these are the overall outcomes for this question. You are going to use both for each informant (within and across prediction)

self_outcomes <- c("semotion", "shyper", 
  "sconduct", "simpact", "speer")

parent_outcomes <- c("pemotion", "phyper_corrected", 
                     "pconduct", "pimpact", "ppeer")




###Question 2
#A. Self-reported Mood and Temper outbursts within and across informant outcomes


# These are the predictors for self report.
self_predictors_mood = c("SY1*sex")

self_predictors_temper = c("SY2*sex")

# Regression models self reported outcomes and SY1
self_reg_SY1 <- lin_reg_function(outcomes = self_outcomes , predictors = self_predictors_mood, data = 
                                   subset(ons_data_design, adolescent == adolescent), family = "gaussian")

openxlsx:: write.xlsx(self_reg_SY1, file = "self_reg_SY1.xlsx", rowNames = TRUE)

# Regression models self reported outcomes and SY2
self_reg_SY2 <- lin_reg_function(outcomes = self_outcomes , predictors = self_predictors_temper, data = 
                                   subset(ons_data_design, adolescent == adolescent), family = "gaussian")

openxlsx:: write.xlsx(self_reg_SY2, file = "self_reg_SY2.xlsx", rowNames = TRUE)


# Regression models parent reported outcomes and SY1
parent_reg_SY1 <- lin_reg_function(outcomes =parent_outcomes , predictors = self_predictors_mood, data = 
                                   subset(ons_data_design, adolescent == adolescent), family = "gaussian")

openxlsx:: write.xlsx(parent_reg_SY1, file = "parent_reg_SY1.xlsx", rowNames = TRUE)

# Regression models parent reported outcomes and SY2

parent_reg_SY2 <- lin_reg_function(outcomes = parent_outcomes , predictors = self_predictors_temper, data = 
                                     subset(ons_data_design, adolescent == adolescent), family = "gaussian")

openxlsx:: write.xlsx(parent_reg_SY2, file = "parent_reg_SY2.xlsx", rowNames = TRUE)

###Question 2
#B. Parent-reported Mood and Temper outbursts within and across informant outcomes

# these are the predictors for parent report

parent_predictors_mood = c("PY1*sex*adolescent")


parent_predictors_temper = c("PY2*sex*adolescent")



parent_reg_PY1 <- lin_reg_function(outcomes = parent_outcomes , predictors = parent_predictors_mood, data = 
                                     ons_data_design, family = "gaussian")

openxlsx:: write.xlsx(parent_reg_PY1, file = "parent_reg_PY1.xlsx", rowNames = TRUE)




parent_reg_PY2 <- lin_reg_function(outcomes = parent_outcomes , predictors = parent_predictors_temper, data = 
                                     ons_data_design, family = "gaussian")

openxlsx:: write.xlsx(parent_reg_PY2, file = "parent_reg_PY2.xlsx", rowNames = TRUE)




self_reg_PY1 <- lin_reg_function(outcomes = self_outcomes , predictors = parent_predictors_mood, data = 
                                     ons_data_design, family = "gaussian")

openxlsx:: write.xlsx(self_reg_PY1, file = "self_reg_PY1.xlsx", rowNames = TRUE)




self_reg_PY2 <- lin_reg_function(outcomes = self_outcomes , predictors = parent_predictors_temper, data = 
                                   ons_data_design, family = "gaussian")

openxlsx:: write.xlsx(self_reg_PY2, file = "self_reg_PY2.xlsx", rowNames = TRUE)



###Question 2 & 3
###C. Do the same thing as above with the categories for disorders as outcomes.
# FIRST, the logistic models for the binary outcomes

disorders_other_outcomes <- c("dcany", "dcemot", "dcanyext",  "pharm", "charm", "exclsch", "help")

disorders_log_reg_SY1 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = self_predictors_mood, data = 
                                            subset(ons_data_design, adolescent == adolescent), family = "quasibinomial")

openxlsx:: write.xlsx(disorders_log_reg_SY1, file = "disorders_log_reg_SY1.xlsx", rowNames = TRUE)



disorders_log_reg_SY2 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = self_predictors_temper, data = 
                                            subset(ons_data_design, adolescent == adolescent), family = "quasibinomial")

openxlsx:: write.xlsx(disorders_log_reg_SY2, file = "disorders_log_reg_SY2.xlsx", rowNames = TRUE)




disorders_log_reg_PY1 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = parent_predictors_mood, data = 
                                            ons_data_design, family = "quasibinomial")

openxlsx:: write.xlsx(disorders_log_reg_PY1, file = "disorders_log_reg_PY1.xlsx", rowNames = TRUE)



disorders_log_reg_PY2 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = parent_predictors_temper, data = 
                                            ons_data_design, family = "quasibinomial")

openxlsx:: write.xlsx(disorders_log_reg_PY2, file = "disorders_log_reg_PY2.xlsx", rowNames = TRUE)





###C. 
# Second, the linear models for the non-binary outcomes

linear_other_outcomes <- c("sescr", "ffscr", "generalhealthcomb", "ghq12scr" )

#sescr ((D) Rosenberg self esteem score)

#ffscr ((D) Score on Family Functioning Scale)

#generalhealthcomb ((D) Child's general health combined)

#ghq12scr ((D) Parental GHQ Score - 12 point scale)



others_lin_reg_SY1 <- lin_reg_function(outcomes = linear_other_outcomes , predictors = self_predictors_mood, data = 
                                            subset(ons_data_design, adolescent == adolescent), family = "gaussian")

openxlsx:: write.xlsx(others_lin_reg_SY1, file = "others_lin_reg_SY1.xlsx", rowNames = TRUE)



others_lin_reg_SY2 <- lin_reg_function(outcomes = linear_other_outcomes , predictors = self_predictors_temper, data = 
                                            subset(ons_data_design, adolescent == adolescent), family = "gaussian")

openxlsx:: write.xlsx(others_lin_reg_SY2, file = "others_lin_reg_SY2.xlsx", rowNames = TRUE)




others_lin_reg_PY1 <- lin_reg_function(outcomes = linear_other_outcomes , predictors = parent_predictors_mood, data = 
                                            ons_data_design, family = "gaussian")

openxlsx:: write.xlsx(others_lin_reg_PY1, file = "others_lin_reg_PY1.xlsx", rowNames = TRUE)



others_lin_reg_PY2 <- lin_reg_function(outcomes = linear_other_outcomes , predictors = parent_predictors_temper, data = 
                                            ons_data_design, family = "gaussian")

openxlsx:: write.xlsx(others_lin_reg_PY2, file = "others_lin_reg_PY2.xlsx", rowNames = TRUE)







#### now look at numbers using the non-weighted sample for the following:
#### what happens if create a category with cases that are "comorbid" with irritable mood 
#### and emotional disorders?




ons_data <- ons_data %>% 
  
  mutate(SY1_comorbid_emot = case_when(
    
    (SY1dic == 0 & dcemot == 0)~ "0",
    
    (SY1dic == 1 & dcemot == 0)~ "1",
    
    (SY1dic == 0 & dcemot) == 1~ "2",
    
    (SY1dic == 1 & dcemot) == 1~ "3"
    
  ))


ons_data %>% count(SY1_comorbid_emot)



irritable_mood_comorbidity_boys <- ons_data %>% 
  filter(adolescent == "adolescent", sex == 0) %>% 
  drop_na(SY1_comorbid_emot) %>% 
  group_by(dcanycd) %>% 
  count(SY1_comorbid_emot) %>% 
  mutate(perc = n/sum(n)) %>% 
  mutate(sex = rep("boys", 4)) %>% 
  mutate(condition = rep(c("no_irrit & no_emot", "irrit & no_emot", "no_irrit & emot", "irrit & emot"), 1))

irritable_mood_comorbidity_boys <- data.frame(cbind(irritable_mood_comorbidity,
                                                    ext_status = rep(c("no ext", "ext"),1, each = 4)))  


irritable_mood_comorbidity_girls <- ons_data %>% 
  filter(adolescent == "adolescent", sex == 1) %>% 
  drop_na(SY1_comorbid_emot) %>% 
  group_by(dcanycd) %>% 
  count(SY1_comorbid_emot) %>% 
  mutate(perc = n/sum(n)) %>% 
  mutate(sex = rep("girls", 4)) %>% 
  mutate(condition = rep(c("no_irrit & no_emot", "irrit & no_emot", "no_irrit & emot", "irrit & emot"), 1))

irritable_mood_comorbidity_girls <- data.frame(cbind(irritable_mood_comorbidity_girls,
                                                     ext_status = rep(c("no ext", "ext"),1, each = 4)))  


irritable_mood_comorbidity_overall <- data.frame(rbind(irritable_mood_comorbidity_boys, irritable_mood_comorbidity_girls))



openxlsx:: write.xlsx(irritable_mood_comorbidity_overall, file = "irritable_mood_comorbidity_overall.xlsx", rowNames = TRUE)






###Question 3
### 









# relate SY1 to negative external outcomes
frequencies_func(outcomes = "SY1dic", cross_tab_var = "dcanyhk", 
                 subset(ons_data_design, adolescent == adolescent & sex == 0))

frequencies_func(outcomes = "SY1dic", cross_tab_var = "dcanyhk", 
                 subset(ons_data_design, adolescent == adolescent & sex == 1))

summary(glm(dcanyhk ~ SY1*sex, data = ons_data, family = binomial))


frequencies_func(outcomes = "charm", cross_tab_var = "SY1dic", 
                 subset(ons_data_design, adolescent == adolescent & sex == 0))

frequencies_func(outcomes = "charm", cross_tab_var = "SY1dic", 
                 subset(ons_data_design, adolescent == adolescent & sex == 1))

summary(glm(charm ~ SY1*sex, data = ons_data, family = binomial))





#VERY IMPORTANT run the frequency function tshow how the proportion of irritability 
#and temper varies by adhd status and sex. Much higher in girls


vars <- c("SY1dic", "SY2dic")


df <- data.frame(i = I(vector(mode = "list", length = length(vars)*2)), # this is a neat trick
                 j = I(vector(mode = "list", length = 2))) # that allows you to create a df that
                                                          # will accept a list of dfs in its cells




for(i in 1: length(vars)){    
  for (j in 1: 2){
   
     df[[i, j]] <-   as.matrix(frequencies_func(outcomes = vars[[i]], cross_tab_var = "dcanyhk", 
                     subset(ons_data_design, adolescent == adolescent & sex == sex[j])))
     
  }
}

test <- do.call(rbind, df)   # the following three lines allowo you to get the dfs out. sure there is a better way :)
test2 <- do.call(rbind, test)
adhd_data_per_sex_sy1_sy2 <- do.call(rbind, test2)
adhd_data_per_sex_sy1_sy2 <- data.frame(cbind(data.frame(adhd_data_per_sex_sy1_sy2),
                                              condition = rep(c("no","yes"),4),
                                              gender=rep (c("boys","girls"),each = 2, 2)))
adhd_data_per_sex_sy1_sy2

openxlsx:: write.xlsx(adhd_data_per_sex_sy1_sy2, file = "adhd_data_per_sex_sy1_sy2.xlsx", rowNames = TRUE)


# now loop a regression over the list to show effects statistically 

vars <- c("SY1dic", "SY2dic")
reg_with_adhd <- vector("list", length(vars))

for (i in 1: length(vars)){

  reg_with_adhd[[i]] <- data.frame(lin_reg_function(outcomes = vars[[i]] , "predictors" = "dcanyhk*sex", data = 
                                   subset(ons_data_design, adolescent == adolescent), family = "gaussian"))

}
 

openxlsx:: write.xlsx(reg_with_adhd, file = "reg_with_adhd.xlsx", rowNames = TRUE)






### Now check how important irritability is in and of itself:

# first with impact as an outcome and dep as control
model_without_dep <- lin_reg_function(outcomes = c("pimpact", "simpact"), predictors = c("sex*SY1"),
                                   data = subset(ons_data_design, adolescent == adolescent), 
                                   family = "gaussian")

model_with_dep <- lin_reg_function(outcomes = c("pimpact", "simpact"), predictors = c("sex*SY1", "dcanyde"),
                 data = subset(ons_data_design, adolescent == adolescent), 
                 family = "gaussian")




simp_model_without_irrit <- svyglm(simpact ~ sex*dcanyde, design = subset(ons_data_design, adolescent == adolescent) )

simp_model_with_irrit <- svyglm(simpact ~ sex*SY1 + sex*dcanyde, design = subset(ons_data_design, adolescent == adolescent) )


pimp_model_without_irrit <- svyglm(pimpact ~ sex*dcanyde, design = subset(ons_data_design, adolescent == adolescent) )

pimp_model_with_irrit <- svyglm(pimpact ~ sex*SY1 + sex*dcanyde , design = subset(ons_data_design, adolescent == adolescent) )

anova(simp_model_without_irrit, simp_model_with_irrit)
anova(pimp_model_without_irrit, pimp_model_with_irrit)


## now with impact as outcome and any emotional disorder as control 
simp_model_without_irrit <- svyglm(simpact ~ sex*dcemot, design = subset(ons_data_design, adolescent == adolescent) )

simp_model_with_irrit <- svyglm(simpact ~ sex*SY1 + sex*dcemot, design = subset(ons_data_design, adolescent == adolescent) )


pimp_model_without_irrit <- svyglm(pimpact ~ sex*dcemot, design = subset(ons_data_design, adolescent == adolescent) )

pimp_model_with_irrit <- svyglm(pimpact ~ sex*SY1 + sex*dcemot , design = subset(ons_data_design, adolescent == adolescent) )

anova(simp_model_without_irrit, simp_model_with_irrit)
anova(pimp_model_without_irrit, pimp_model_with_irrit)






# now with self-harm as an outcome


sdsh_model_without_irrit <- svyglm(charm ~ sex*dcanyde,
                                   family = "binomial", design = subset(ons_data_design, adolescent == adolescent) )

sdsh_model_with_irrit <- svyglm(charm ~ sex*SY1 + sex*dcanyde, 
                                family = "binomial", design = subset(ons_data_design, adolescent == adolescent) )


pdsh_model_without_irrit <- svyglm(pharm ~ sex*dcanyde, 
                                   family = "binomial", design = subset(ons_data_design, adolescent == adolescent) )

pdsh_model_with_irrit <- svyglm(pharm ~ sex*SY1 + sex*dcanyde , 
                                family = "binomial", design = subset(ons_data_design, adolescent == adolescent) )

anova(sdsh_model_without_irrit, sdsh_model_with_irrit)
anova(pdsh_model_without_irrit, pdsh_model_with_irrit)

# now with dsh as outcome and anyemot as predictor
sdsh_model_without_irrit <- svyglm(charm ~ sex*dcemot,
                                   family = "binomial", design = subset(ons_data_design, adolescent == adolescent) )

sdsh_model_with_irrit <- svyglm(charm ~ sex*SY1 + sex*dcemot, 
                                family = "binomial", design = subset(ons_data_design, adolescent == adolescent) )


pdsh_model_without_irrit <- svyglm(pharm ~ sex*dcemot, 
                                   family = "binomial", design = subset(ons_data_design, adolescent == adolescent) )

pdsh_model_with_irrit <- svyglm(pharm ~ sex*SY1 + sex*dcemot , 
                                family = "binomial", design = subset(ons_data_design, adolescent == adolescent) )

anova(sdsh_model_without_irrit, sdsh_model_with_irrit)
anova(pdsh_model_without_irrit, pdsh_model_with_irrit)







#### now look at numbers using the non-weighted sample for the following:
#### what happens if create a category with cases that are "comorbid" with irritable mood 
#### and emotional disorders?


  

  ons_data <- ons_data %>% 
  
  mutate(SY1_comorbid_emot = case_when(
                                        
   (SY1dic == 0 & dcemot == 0)~ "0",
   
 (SY1dic == 1 & dcemot == 0)~ "1",
 
  (SY1dic == 0 & dcemot) == 1~ "2",
         
  (SY1dic == 1 & dcemot) == 1~ "3"

))


ons_data %>% count(SY1_comorbid_emot)



irritable_mood_comorbidity_boys <- ons_data %>% 
  filter(adolescent == "adolescent", sex == 0) %>% 
  drop_na(SY1_comorbid_emot) %>% 
  group_by(dcanycd) %>% 
  count(SY1_comorbid_emot) %>% 
  mutate(perc = n/sum(n)) %>% 
  mutate(sex = rep("boys", 4)) %>% 
  mutate(condition = rep(c("no_irrit & no_emot", "irrit & no_emot", "no_irrit & emot", "irrit & emot"), 1))

irritable_mood_comorbidity_boys <- data.frame(cbind(irritable_mood_comorbidity,
                                               ext_status = rep(c("no ext", "ext"),1, each = 4)))  


irritable_mood_comorbidity_girls <- ons_data %>% 
  filter(adolescent == "adolescent", sex == 1) %>% 
  drop_na(SY1_comorbid_emot) %>% 
  group_by(dcanycd) %>% 
  count(SY1_comorbid_emot) %>% 
  mutate(perc = n/sum(n)) %>% 
  mutate(sex = rep("girls", 4)) %>% 
  mutate(condition = rep(c("no_irrit & no_emot", "irrit & no_emot", "no_irrit & emot", "irrit & emot"), 1))

irritable_mood_comorbidity_girls <- data.frame(cbind(irritable_mood_comorbidity_girls,
                                                    ext_status = rep(c("no ext", "ext"),1, each = 4)))  


irritable_mood_comorbidity_overall <- data.frame(rbind(irritable_mood_comorbidity_boys, irritable_mood_comorbidity_girls))



openxlsx:: write.xlsx(irritable_mood_comorbidity_overall, file = "irritable_mood_comorbidity_overall.xlsx", rowNames = TRUE)




# also create a simple graph with simpact and pimapct for the comorbidity

#self_report
ons_data %>% 
  group_by(sex, SY1_comorbid_emot) %>% 
  filter(!is.na(SY1_comorbid_emot) ) %>% 
  summarise(total_problems = mean(sebdtot, na.rm= T), total_problems_sd = sd(sebdtot, na.rm= T))


#parent_report
ons_data %>% 
  group_by(sex, SY1_comorbid_emot) %>% 
  filter(!is.na(SY1_comorbid_emot) ) %>% 
  summarise(total_problems = mean(sebdtot, na.rm= T), total_problems_sd = sd(sebdtot, na.rm= T))

##### this is a function to do t-tests across many variables, but is not terribly helpful as I haven't figured out 
# a good automated subsetting.
ttests_for_survey <- function(vars_to_test, predictors, data){

p_vals <- 0
t_vals <- 0 
dfs <- 0
together <- vector("list", length(vars_for_t_test))
means_and_ses <- vector("list", length(vars_for_t_test))
means_ses_stats <- vector("list", length(vars_for_t_test))
for (i in 1:length(vars_for_t_test)){

p_vals[i] <- svyttest(as.formula(paste(vars_for_t_test[i], paste(predictors, 
                                                                 collapse = "+"),
                                       sep = "~")), ons_data_design)$p.value

t_vals[[i]] <- svyttest(as.formula(paste(vars_for_t_test[i], paste(predictors, 
                                                                   collapse = "+"),
                                         sep = "~")), ons_data_design)$statistic

dfs[[i]] <- svyttest(as.formula(paste(vars_for_t_test[i], paste(predictors, 
                                                           collapse = "+"),
                                 sep = "~")), ons_data_design)$parameter



means_and_ses[[i]] <- data.frame(svyby(as.formula(paste("", vars_for_t_test[i], sep = "~")), as.formula(paste("", predictors, sep = "~")), ons_data_design, svymean, na = T ))


means_ses_stats[[i]] <- data.frame(cbind(means_and_ses[[i]], t = t_vals[[i]],
                                        df = dfs[[i]], p = p_vals[[i]]))


}

return(means_ses_stats)
}


child_p_means_female <- ttests_for_survey(vars_to_test =  c("semotion", "shyper", "sconduct", "simpact"),
                  predictors = "SY1dic", data = ons_data_design )

child_p_means_male<- ttests_for_survey(vars_to_test =  c("semotion", "shyper", "sconduct", "simpact"),
                  predictors = "SY1dic", data = ons_data_design )

ttests_for_survey(vars_to_test =  c("semotion", "shyper", "sconduct", "simpact"),
                  predictors = "SY1dic", data = ons_data_design )

ttests_for_survey(vars_to_test =  c("semotion", "shyper", "sconduct", "simpact"),
                  predictors = "SY1dic", data = ons_data_design )



checkitout <- ttests_for_survey(vars_to_test =  c("semotion", "shyper", "sconduct", "simpact"),
                                predictors = "SY1dic", data = ons_data_design )

checkitout 

















# estimate SY1 for each age point
# unweighted
sy1_adol_unweighted <- ons_data %>% filter(adolescent == "adolescent")%>%  
  group_by(sex, ChldAge) %>% summarise(avgSY1 = mean(SY1, na.rm = TRUE))
openxlsx:: write.xlsx(sy1_adol_unweighted, file = "sy1_adol_unweighted.xlsx")
# weighted
sy1_adol_weighted <- svyby(~ SY1,~sex+ ChldAge, 
                           design = subset(ons_data_design, adolescent == "adolescent"), na  = T, svymean)
openxlsx:: write.xlsx(sy1_adol_weighted, file = "sy1_adol_weighted.xlsx")

# estimate SY1 for each age point
py1_adol_unweighted <- ons_data %>%   
  group_by(sex, ChldAge) %>% summarise(avgPY1 = mean(PY1, na.rm = TRUE))
openxlsx:: write.xlsx(py1_adol_unweighted, file = "py1_unweighted.xlsx")
# weighted
py1_weighted <- svyby(~ PY1,~sex + ChldAge, 
                      design = ons_data_design, na  = T, svymean)
openxlsx:: write.xlsx(py1_weighted, file = "py1_weighted.xlsx")

# put self and parent together
colnames(sy1_adol_weighted) <- c("sex", "age", "Y1", "se")
colnames(py1_adol_weighted) <- c("sex", "age", "Y1", "se")
all_ages_weighted <- data.frame(rbind(sy1_adol_weighted, py1_adol_weighted))
openxlsx:: write.xlsx(all_ages_weighted, file = "py1_adol_weighted.xlsx")





vars_sdq<- c("semotion", "sconduct", "shyper", "sprosoc", "speer", "simpact")
pred_vars<- "SZ1"
starting <- Sys.time()
lin_reg_function(vars_sdq, pred_vars, subset(ons_data_design, adolescent== "adolescent"), "gaussian")
ending <- Sys.time()
ending-starting

############for Georgina
ons_data$SZ1_dic <- ifelse(ons_data$SZ1>1,1,0)
s_means_sdqs_SZ1 <- func_means_survey(vars_for_means = c("semotion", "shyper", 
                                                         "sconduct", "simpact", "speer"),
                                      predictors = c("SZ1_dic + sex + adolescent"), 
                                      data = ons_data_design)

openxlsx:: write.xlsx(s_means_sdqs_SZ1, file = "s_means_sdqs_SZ1.xlsx")



######## this is code for plotting that I reated out of here

library(tidyverse)
library(patchwork)
library(readxl)


all_ages <- read_excel("Downloads/all_ages 3.xlsx") # from the R-drive of DSH ONSYP on 23rd September 2022
all_ages$sex <- factor(all_ages$subs_var)
levels(all_ages$sex ) = c("boys", "girls")
all_ages$informant = factor(all_ages$outcome)
levels(all_ages$informant)<-  c("parent", "parent", "child", "child")
all_ages$age_group = factor(all_ages$age_group)






# # # #  Create graphs for the frequency of parent and self reported mood and outbursts    
# parent rated irritable mood    
p_irritable_mood <- all_ages %>% 
  filter(informant == "parent", outcome == "PY1dic") %>% 
  ggplot( aes(x= factor(age_group, levels = c("child", "adolescent")), y=100*mean, colour = sex)) + 
  geom_pointrange(aes(ymin= 100*(mean - se), ymax=100*(mean+se )), fatten = 6,
                  position = position_dodge(width = .5))

p_irritable_mood  <- p_irritable_mood    + ylab("percentage +/- se")+ xlab("") + ylim(12, 32) + ggtitle("Irritable Mood by\n parent report across development")

p_irritable_mood  +theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                         axis.title=element_text(size=16))


# parent rated outbursts    
p_outbursts <- all_ages %>% 
  filter(informant == "parent", outcome == "PY2dic") %>% 
  ggplot( aes(x= factor(age_group, levels = c("child", "adolescent")), y=100*mean, colour = sex)) + 
  geom_pointrange(aes(ymin= 100*(mean - se), ymax=100*(mean+se )), fatten = 6,
                  position = position_dodge(width = .5))

p_outbursts  <- p_outbursts    + ylab("percentage +/- se")+ xlab("") + ylim(12, 32) + ggtitle("Temper Outbursts by\n parent report across development")

p_outbursts  + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                     axis.title=element_text(size=16))

# mood and outbursts together   
p_irritable_mood + p_outbursts 


# irritable mood by self report
s_irritable_mood <- all_ages %>% 
  filter(age_group == "adolescent", outcome == "SY1dic") %>% 
  ggplot( aes(x= factor(sex, levels = c("boys", "girls")), y=100*mean, colour = sex)) + 
  geom_pointrange(aes(ymin= 100*(mean - se), ymax=100*(mean+se )), fatten = 6,
                  position = position_dodge(width = .5))

s_irritable_mood   <- s_irritable_mood   + ylab("percentage +/- se")+ xlab("") + ylim(12, 32) + ggtitle("Irritable mood by \nself report in adolescence")

s_irritable_mood  + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                          axis.title=element_text(size=16))



# irritable mood by self report
s_temper_outbursts <- all_ages %>% 
  filter(age_group == "adolescent", outcome == "SY2dic") %>% 
  ggplot( aes(x= factor(sex, levels = c("boys", "girls")), y=100*mean, colour = sex)) + 
  geom_pointrange(aes(ymin= 100*(mean - se), ymax=100*(mean+se )), fatten = 6,
                  position = position_dodge(width = .5))

s_temper_outbursts   <- s_temper_outbursts + ylab("percentage +/- se")+ xlab("") + ylim(12, 32) + ggtitle("Temper Outbursts by \nself report in adolescence")

s_temper_outbursts  + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                            axis.title=element_text(size=16))


# mood and outbursts together   
(p_irritable_mood + s_irritable_mood)/ (p_outbursts + s_temper_outbursts) 



### use regression tables

parent_age_sex_inter_reg_py1 <- read_excel("Downloads/documents_20220925/parent_age_sex_inter_reg.xlsx", 
                                           sheet = "PY1")
parent_age_sex_inter_reg_py2 <- read_excel("Downloads/documents_20220925/parent_age_sex_inter_reg.xlsx", 
                                           sheet = "PY2")

self_sex_inter_reg_sy1 <- read_excel("Downloads/documents_20220925/self_sex_inter_reg.xlsx", 
                                     sheet = "SY1")
self_sex_inter_reg_sy2 <- read_excel("Downloads/documents_20220925/self_sex_inter_reg.xlsx", 
                                     sheet = "SY2")


library(kableExtra)
knitr:: kable(parent_age_sex_inter_reg_py1 , caption = "parent irritable mood", col.names = c("predictors",
                                                                                              'β', "SE", "t", "p"),
              digits = 3, "html") %>% 
  save_kable("/Users/a.stringaris/Downloads/py1.pdf")


knitr:: kable(parent_age_sex_inter_reg_py2 , caption = "parent temper outbursts", col.names = c("predictors",
                                                                                                'β', "SE", "t", "p"),
              digits = 3, "html") %>% 
  save_kable("/Users/a.stringaris/Downloads/py2.pdf")


knitr:: kable(self_sex_inter_reg_sy1 , caption = "self irritable mood", col.names = c("predictors",
                                                                                      'β', "SE", "t", "p"),
              digits = 3, "html") %>% 
  save_kable("/Users/a.stringaris/Downloads/sy1.pdf")



##### the linear function for age REDO with bigger binning of age

all_ages_weighted <- read_excel("Downloads/all_ages_weighted.xlsx") # from the R-drive of DSH ONSYP
all_ages_weighted <- all_ages_weighted [all_ages_weighted$age<=17,]
head(all_ages_weighted, 20)

informant <- c(rep("self", length(12:17)*2), rep("parent", length(5:17)*2)  ) # really bad, I should have done it when I was creating the df in DSH
all_ages_weighted$informant <- informant 

all_ages_weighted$sex <- factor(all_ages_weighted$sex)
levels(all_ages_weighted$sex) = c("boys", "girls")

all_ages_weighted$age <- factor(all_ages_weighted$age)
#"paste0(all_ages_weighted$age, collapse = "", sep = ",")"

age_sex_and_informant <-  all_ages_weighted %>% 
  
  ggplot( aes(x= as.numeric(age), y=100*Y1, colour = sex)) + 
  geom_pointrange(aes(ymin= 100*(Y1 - se), ymax=100*(Y1+se )),
                  position = position_dodge(width = .5))+
  geom_point(position = position_dodge(width = .5)) +
  geom_line(position = position_dodge(width = .5)) +
  facet_grid(~informant)

age_sex_and_informant     #+
scale_x_continuous(n.breaks = 3)


s_temper_outbursts   <- s_temper_outbursts + ylab("percentage +/- se")+ xlab("") + ylim(12, 32) + ggtitle("Temper Outbursts by self report in adolescence")

s_temper_outbursts  + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                            axis.title=element_text(size=16))   



#### import SDQ result sheets for graphing
semotion_SY1 <- read_excel("Downloads/s_means_sdqs_SY1.xlsx", 
                           sheet = "Sheet 1")


shyper_SY1 <- read_excel("Downloads/s_means_sdqs_SY1.xlsx", 
                         sheet = "Sheet 2")

sconduct_SY1 <- read_excel("Downloads/s_means_sdqs_SY1.xlsx", 
                           sheet = "Sheet 3")

simpact_SY1 <- read_excel("Downloads/s_means_sdqs_SY1.xlsx", 
                          sheet = "Sheet 4")

speer_SY1 <- read_excel("Downloads/s_means_sdqs_SY1.xlsx", 
                        sheet = "Sheet 5")


head(semotion_SY1, 15)



semotion_SY1$sex <- factor(semotion_SY1$sex)
levels(semotion_SY1$sex ) = c("boys", "girls")
semotion_SY1$SY1dic <- factor(semotion_SY1$SY1dic)
levels(semotion_SY1$SY1dic ) = c("no", "yes")

plot_semotion_SY1 <- semotion_SY1 %>% 
  filter(adolescent == "adolescent") %>% 
  ggplot( aes(x= factor(SY1dic, levels = c("no", "yes")), y= semotion, colour = sex)) + 
  geom_pointrange(aes(ymin= (semotion - se), ymax=(semotion +se )), fatten = 6,
                  position = position_dodge(width = .5))

plot_semotion_SY1    <- plot_semotion_SY1 + ylab("mean +/- se")+ xlab("") + ylim(0, 5) + 
  ggtitle("emotion problems \nself report in adolescence") +
  xlab("presence of irritabile mood")

plot_semotion_SY1   + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                            axis.title=element_text(size=16))



sconduct_SY1$sex <- factor(sconduct_SY1$sex)
levels(sconduct_SY1$sex ) = c("boys", "girls")
sconduct_SY1$SY1dic <- factor(sconduct_SY1$SY1dic)
levels(sconduct_SY1$SY1dic ) = c("no", "yes")

plot_sconduct_SY1 <- sconduct_SY1 %>% 
  filter(adolescent == "adolescent") %>% 
  ggplot( aes(x= factor(SY1dic, levels = c("no", "yes")), y= sconduct, colour = sex)) + 
  geom_pointrange(aes(ymin= (sconduct - se), ymax=(sconduct +se )), fatten = 6,
                  position = position_dodge(width = .5))

plot_sconduct_SY1   <- plot_sconduct_SY1 + ylab("mean +/- se")+ xlab("") + ylim(0, 5) + 
  ggtitle("conduct problems \nself report in adolescence") +
  xlab("presence of irritabile mood")

plot_sconduct_SY1   + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                            axis.title=element_text(size=16))


shyper_SY1$sex <- factor(shyper_SY1$sex)
levels(shyper_SY1$sex ) = c("boys", "girls")
shyper_SY1$SY1dic <- factor(shyper_SY1$SY1dic)
levels(shyper_SY1$SY1dic ) = c("no", "yes")

plot_shyper_SY1 <- shyper_SY1 %>% 
  filter(adolescent == "adolescent") %>% 
  ggplot( aes(x= factor(SY1dic, levels = c("no", "yes")), y= shyper, colour = sex)) + 
  geom_pointrange(aes(ymin= (shyper- se), ymax=(shyper +se )), fatten = 6,
                  position = position_dodge(width = .5))

plot_shyper_SY1   <- plot_shyper_SY1 + ylab("mean +/- se")+ xlab("") + ylim(0, 5) + 
  ggtitle("hyperactivity \nself report in adolescence") +
  xlab("presence of irritabile mood")

plot_shyper_SY1   + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                          axis.title=element_text(size=16))





speer_SY1$sex <- factor(speer_SY1$sex)
levels(speer_SY1$sex ) = c("boys", "girls")
speer_SY1$SY1dic <- factor(speer_SY1$SY1dic)
levels(speer_SY1$SY1dic ) = c("no", "yes")

plot_speer_SY1 <- speer_SY1 %>% 
  filter(adolescent == "adolescent") %>% 
  ggplot( aes(x= factor(SY1dic, levels = c("no", "yes")), y= speer, colour = sex)) + 
  geom_pointrange(aes(ymin= (speer- se), ymax=(speer +se )), fatten = 6,
                  position = position_dodge(width = .5))

plot_speer_SY1   <- plot_speer_SY1 + ylab("mean +/- se")+ xlab("") + ylim(0, 5) + 
  ggtitle("peer problems \nself report in adolescence") +
  xlab("presence of irritabile mood")

plot_speer_SY1   + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                         axis.title=element_text(size=16))


# put them all together
(plot_semotion_SY1 + plot_sconduct_SY1)/
  (plot_shyper_SY1 + plot_speer_SY1)



# same thing for SY2, plot SDQs 
### extract the excels sheets in cleverer way than I had done before!!!
library(readxl)
sheets <- excel_sheets("/Users/a.stringaris/Downloads/s_means_sdqs_SY2.xlsx")           # Get sheet names
sheets 

# create a list containing a df for each sheet
list_df <- lapply(sheets, function(x) {          # Read all sheets to list
  as.data.frame(read_excel("/Users/a.stringaris/Downloads/s_means_sdqs_SY2.xlsx", sheet = x)) } )

# name it
names(list_df) <- c("emotion", "conduct", "hyperactivity", "impact", "peer") 

# loop over it to create the factors in each
for (i in 1:length(list_df)){
  
  list_df[[i]]$sex <- factor(list_df[[i]]$sex)
  levels(list_df[[i]]$sex ) = c("boys", "girls")
  
  list_df[[i]]$SY2dic <- factor(list_df[[i]]$SY2dic)
  levels(list_df[[i]]$SY2dic) = c("no", "yes")
}

# create a filtered dataframe for each list element, this is necessary for the loop to work
filt_list_df <- vector("list", length(list_df))
for (i in 1: length(list_df)){
  
  filt_list_df[[i]] <- list_df[[i]] %>% 
    filter(adolescent == "adolescent")
  
}
# now create the plots
plot_list <- vector("list", length(filt_list_df))
test <- vector("list", length(filt_list_df))
for (i in 1: length(list_df)){
  
  p =   ggplot( filt_list_df[[i]], aes(x= factor(filt_list_df[[i]][,2], levels = c("no", "yes")), 
                                       y= filt_list_df[[i]][,5], colour = filt_list_df[[i]][,3])) + 
    geom_pointrange(aes(ymin= (filt_list_df[[i]][,5] - filt_list_df[[i]][,6]), 
                        ymax=(filt_list_df[[i]][,5] + filt_list_df[[i]][,6] )), 
                    fatten = 6,
                    position = position_dodge(width = .5)) +
    
    ylab("mean +/- se")+ xlab("") + ylim(0, 6) + 
    ggtitle(paste(names(list_df[i])," problems \nself report in adolescence")) +
    xlab("presence of temper outbursts") +
    theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
          axis.title=element_text(size=16))
  
  
  plot_list[[i]] = p
  
}

# now save the plots
for (i in 1:length(list_df)) {
  file_name = paste("plot_list_", names(list_df) [i], ".tiff", sep="")
  tiff(file_name)
  print(plot_list[[i]])
  dev.off()
}



######## Show impact
# first by self report
simpact_SY1$sex <- factor(simpact_SY1$sex)
levels(simpact_SY1$sex ) = c("boys", "girls")
simpact_SY1$SY1dic <- factor(simpact_SY1$SY1dic)
levels(simpact_SY1$SY1dic ) = c("no", "yes")

plot_simpact_SY1 <- simpact_SY1 %>% 
  filter(adolescent == "adolescent") %>% 
  ggplot( aes(x= factor(SY1dic, levels = c("no", "yes")), y= simpact, colour = sex)) + 
  geom_pointrange(aes(ymin= (simpact- se), ymax=(simpact +se )), fatten = 6,
                  position = position_dodge(width = .5))

plot_simpact_SY1   <- plot_simpact_SY1 + ylab("mean +/- se")+ xlab("") + ylim(0, 2) + 
  ggtitle("impairment \nself report in adolescence") +
  xlab("presence of irritabile mood")

plot_simpact_SY1   + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                           axis.title=element_text(size=16))

# now by parent report

pimpact_SY1 <- read_excel("Downloads/p_means_sdqs_SY1.xlsx", 
                          sheet = "Sheet 4")
pimpact_SY1$sex <- factor(pimpact_SY1$sex)
levels(pimpact_SY1$sex ) = c("boys", "girls")
pimpact_SY1$SY1dic <- factor(pimpact_SY1$SY1dic)
levels(pimpact_SY1$SY1dic ) = c("no", "yes")

plot_pimpact_SY1 <- pimpact_SY1 %>% 
  filter(adolescent == "adolescent") %>% 
  ggplot( aes(x= factor(SY1dic, levels = c("no", "yes")), y= pimpact, colour = sex)) + 
  geom_pointrange(aes(ymin= (pimpact- se), ymax=(pimpact +se )), fatten = 6,
                  position = position_dodge(width = .5))

plot_pimpact_SY1   <- plot_pimpact_SY1 + ylab("mean +/- se")+ xlab("") + ylim(0, 2) + 
  ggtitle("impairment \nparent report in adolescence") +
  xlab("presence of irritabile mood")

plot_pimpact_SY1   + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                           axis.title=element_text(size=16))


plot_simpact_SY1/plot_pimpact_SY1


######## Show relationship with ADHD
adhd_data_per_sex_sy1_sy2 <- read_excel("Downloads/adhd_data_per_sex_sy1_sy2.xlsx")
#had forgotten to add a column about which outcome, do this here
adhd_data_per_sex_sy1_sy2 <- data.frame(adhd_data_per_sex_sy1_sy2, outcome = rep(c("SY1", "SY2"), 1, each = 4)) 
adhd_data_per_sex_sy1_sy2

plot_adhd_SY1 <- adhd_data_per_sex_sy1_sy2  %>% 
  filter(outcome == "SY1") %>% 
  ggplot( aes(x= factor(condition, levels = c("no", "yes")), y= 100*mean, colour = gender)) + 
  geom_pointrange(aes(ymin= 100*(mean- se), ymax=100*(mean +se )), fatten = 6,
                  position = position_dodge(width = .5))

plot_adhd_SY1   <- plot_adhd_SY1 + ylab("percentage")+ xlab("ADHD") + ylim(0, 100) + 
  ggtitle("irritable mood \n self report by ADHD") 

plot_adhd_SY1 <- plot_adhd_SY1  + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                                        axis.title=element_text(size=16))

plot_adhd_SY1 

plot_adhd_SY2 <- adhd_data_per_sex_sy1_sy2  %>% 
  filter(outcome == "SY2") %>% 
  ggplot( aes(x= factor(condition, levels = c("no", "yes")), y= 100*mean, colour = gender)) + 
  geom_pointrange(aes(ymin= 100*(mean- se), ymax=100*(mean +se )), fatten = 6,
                  position = position_dodge(width = .5))

plot_adhd_SY2   <- plot_adhd_SY2 + ylab("percentage")+ xlab("ADHD") + ylim(0, 100) + 
  ggtitle("temper outbursts \n self report by ADHD") 

plot_adhd_SY2   <-plot_adhd_SY2  + theme(axis.text=element_text(size=14), legend.text=element_text(size=14), plot.title = element_text(size = 16),
                                         axis.title=element_text(size=16))

plot_adhd_SY1 /plot_adhd_SY2


reg_with_adhd <- read_excel("Downloads/reg_with_adhd.xlsx")



# something extra on 11th March 2023 upon request from Pablo
# prevalence of disorders by irritability and outbursts, boys and girls for different age groups.
dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") 
# PY1 for children boys
prev_disorders_PY1_children_boys <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "PY1dic", data =  subset(ons_data_design, sex == 0 &adolescent == "child" ))
# PY1 for adolescents boys
prev_disorders_PY1_adolescent_boys <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "PY1dic", data =  subset(ons_data_design, sex == 0 &adolescent == "adolescent" ))
# PY2 for children boys
prev_disorders_PY2_children_boys <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "PY2dic", data =  subset(ons_data_design, sex == 0 &adolescent == "child" ))
# PY2 for adolescents boys
prev_disorders_PY2_adolescent_boys <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "PY2dic", data =  subset(ons_data_design, sex == 0 &adolescent == "adolescent" ))



# PY1 for children girs
prev_disorders_PY1_children_girls <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "PY1dic", data =  subset(ons_data_design, sex == 1 &adolescent == "child" ))
# PY1 for adolescents girls
prev_disorders_PY1_adolescent_girls <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "PY1dic", data =  subset(ons_data_design, sex == 1 &adolescent == "adolescent" ))
# PY2 for children girls
prev_disorders_PY2_children_girls <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "PY2dic", data =  subset(ons_data_design, sex == 1 &adolescent == "child" ))
# PY2 for adolescents girls
prev_disorders_PY2_adolescent_girls <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "PY2dic", data =  subset(ons_data_design, sex == 1 &adolescent == "adolescent" ))




# SY1 for children boys
prev_disorders_SY1_children_boys <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "SY1dic", data =  subset(ons_data_design, sex == 0 &adolescent == "child" ))
# SY1 for adolescents boys
prev_disorders_SY1_adolescent_boys <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "SY1dic", data =  subset(ons_data_design, sex == 0 &adolescent == "adolescent" ))
# SY2 for children boys
prev_disorders_SY2_children_boys <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "SY2dic", data =  subset(ons_data_design, sex == 0 &adolescent == "child" ))
# SY2 for adolescents boys
prev_disorders_SY2_adolescent_boys <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "SY2dic", data =  subset(ons_data_design, sex == 0 &adolescent == "adolescent" ))


SY1_SY2_boys <- list(data.frame(prev_disorders_SY1_children_boys),  data.frame(prev_disorders_SY1_adolescent_boys), data.frame(prev_disorders_SY2_children_boys), data.frame(prev_disorders_SY2_adolescent_boys))



# SY1 for children girs
prev_disorders_SY1_children_girls <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "SY1dic", data =  subset(ons_data_design, sex == 1 &adolescent == "child" ))
# SY1 for adolescents girls
prev_disorders_SY1_adolescent_girls <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "SY1dic", data =  subset(ons_data_design, sex == 1 &adolescent == "adolescent" ))
# SY2 for children girls
prev_disorders_SY2_children_girls <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "SY2dic", data =  subset(ons_data_design, sex == 1 &adolescent == "child" ))
# SY2 for adolescents girls
prev_disorders_SY2_adolescent_girls <- frequencies_func(outcomes = dic_outcomes_for_disorders <- c("dcany", "dcemot", "dcanyext") , cross_tab_var = "SY2dic", data =  subset(ons_data_design, sex == 1 &adolescent == "adolescent" ))


SY1_SY2_girls <- list(data.frame(prev_disorders_SY1_children_girls),  data.frame(prev_disorders_SY1_adolescent_girls), data.frame(prev_disorders_SY2_children_girls), data.frame(prev_disorders_SY2_adolescent_girls))



#Pablo code 22 Sept 2023

### Question 1. Statistical evaluation of sex effects in adolescence
###here we are using the linear regression function to get outcomes for adolescents
parent_outcomes <- c("PY1", "PY2")
parent_predictors <- c("sex") # development doesn't make sense now because we only focus on adolescent



# use function for parent report 
parent_sex_inter_reg <- lin_reg_function(outcomes = parent_outcomes, predictors = parent_predictors,
                                         data = subset(ons_data_design, adolescent == "adolescent"), 
                                         family = "gaussian")

openxlsx:: write.xlsx(parent_sex_inter_reg, file = "parent_sex_inter_reg.xlsx", rowNames = TRUE)


# use function for self report for adolescents 
self_outcomes <- c("SY1", "SY2")
self_predictors <- c("sex") # development doesn't make sense

self_sex_inter_reg <- lin_reg_function(outcomes = self_outcomes, predictors = self_predictors,
                                       data = subset(ons_data_design, adolescent == "adolescent"), 
                                       family = "gaussian")

openxlsx:: write.xlsx(self_sex_inter_reg, file = "self_sex_inter_reg.xlsx", rowNames = TRUE)





###### Question 2. Descriptives of associations with psychopathology and impact
# Here we use my custom-made means function to get SDQs for each informant across many variables.

# within informant for self repeort
s_means_sdqs_SY1 <- func_means_survey(vars_for_means = c("semotion", "shyper", 
                                                         "sconduct", "simpact", "speer"),
                                      predictors = c("SY1dic + sex + adolescent"), 
                                      data = ons_data_design)

openxlsx:: write.xlsx(s_means_sdqs_SY1, file = "s_means_sdqs_SY1.xlsx", rowNames = TRUE)


# across informant for self repeort
p_means_sdqs_SY1 <- func_means_survey(vars_for_means = c("pemotion", "phyper_corrected", 
                                                         "pconduct", "pimpact", "ppeer"),
                                      predictors = c("SY1dic + sex + adolescent"), 
                                      data = ons_data_design)

openxlsx:: write.xlsx(p_means_sdqs_SY1, file = "p_means_sdqs_SY1.xlsx", rowNames = TRUE)


# within informant for parent repeort
p_means_sdqs_PY1 <- func_means_survey(vars_for_means = c("pemotion", "phyper_corrected", 
                                                         "pconduct", "pimpact", "ppeer"),
                                      predictors = c("PY1dic + sex + adolescent"), 
                                      data = ons_data_design)

openxlsx:: write.xlsx(p_means_sdqs_PY1, file = "p_means_sdqs_PY1.xlsx", rowNames = TRUE)


# across informant for parent repeort
s_means_sdqs_PY1 <- func_means_survey(vars_for_means = c("semotion", "shyper", 
                                                         "sconduct", "simpact", "speer"),
                                      predictors = c("PY1dic + sex + adolescent"), 
                                      data = ons_data_design)


openxlsx:: write.xlsx(s_means_sdqs_PY1, file = "s_means_sdqs_PY1.xlsx", rowNames = TRUE)



###### Question 2. Statistical evaluation  of associations with psychopathology and impact
####### use reg function for psychopathology and impact to demonstrate statistically the imapct

# these are the overall outcomes for this question. You are going to use both for each informant (within and across prediction)

self_outcomes <- c("semotion", "shyper", 
                   "sconduct", "simpact", "speer")

parent_outcomes <- c("pemotion", "phyper_corrected", 
                     "pconduct", "pimpact", "ppeer")




###Question 2
#A. Self-reported Mood and Temper outbursts within and across informant outcomes


# These are the predictors for self report.
self_predictors_mood = c("SY1*sex")

self_predictors_temper = c("SY2*sex")

# Regression models self reported outcomes and SY1
                                  
self_reg_SY1 <- lin_reg_function(outcomes = self_outcomes , predictors = self_predictors_mood, data =  
                                   subset(ons_data_design, adolescent == "adolescent"), family = "gaussian") 

openxlsx:: write.xlsx(self_reg_SY1, file = "self_reg_SY1.xlsx", rowNames = TRUE)

# Regression models self reported outcomes and SY2
self_reg_SY2 <- lin_reg_function(outcomes = self_outcomes , predictors = self_predictors_temper, data = 
                                   subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(self_reg_SY2, file = "self_reg_SY2.xlsx", rowNames = TRUE)


# Regression models parent reported outcomes and SY1
parent_reg_SY1 <- lin_reg_function(outcomes =parent_outcomes , predictors = self_predictors_mood, data = 
                                     subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(parent_reg_SY1, file = "parent_reg_SY1.xlsx", rowNames = TRUE)

# Regression models parent reported outcomes and SY2

parent_reg_SY2 <- lin_reg_function(outcomes = parent_outcomes , predictors = self_predictors_temper, data = 
                                     subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(parent_reg_SY2, file = "parent_reg_SY2.xlsx", rowNames = TRUE)

###Question 2
#B. Parent-reported Mood and Temper outbursts within and across informant outcomes

# these are the predictors for parent report

parent_predictors_mood = c("PY1*sex")


parent_predictors_temper = c("PY2*sex")



parent_adol_only_reg_PY1 <- lin_reg_function(outcomes = parent_outcomes , predictors = parent_predictors_mood, data = 
                                               subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(parent_adol_only_reg_PY1, file = "parent_adol_only_reg_PY1.xlsx", rowNames = TRUE)




parent_adol_only_reg_PY2 <- lin_reg_function(outcomes = parent_outcomes , predictors = parent_predictors_temper, data = 
                                               subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(parent_adol_only_reg_PY2, file = "parent_adol_only_reg_PY2.xlsx", rowNames = TRUE)




self_reg_PY1 <- lin_reg_function(outcomes = self_outcomes , predictors = parent_predictors_mood, data = 
                                   subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(self_reg_PY1, file = "self_reg_PY1.xlsx", rowNames = TRUE)




self_reg_PY2 <- lin_reg_function(outcomes = self_outcomes , predictors = parent_predictors_temper, data = 
                                   subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(self_reg_PY2, file = "self_reg_PY2.xlsx", rowNames = TRUE)



###Question 2 & 3
###C. Do the same thing as above with the categories for disorders as outcomes.
# FIRST, the logistic models for the binary outcomes

disorders_other_outcomes <- c("dcany", "dcemot", "dcanyext",  "pharm", "charm", "exclsch", "help")

disorders_log_reg_SY1 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = self_predictors_mood, data = 
                                            subset(ons_data_design, adolescent =="adolescent"), family = "quasibinomial")

openxlsx:: write.xlsx(disorders_log_reg_SY1, file = "disorders_log_reg_SY1.xlsx", rowNames = TRUE)



disorders_log_reg_SY2 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = self_predictors_temper, data = 
                                            subset(ons_data_design, adolescent =="adolescent"), family = "quasibinomial")

openxlsx:: write.xlsx(disorders_log_reg_SY2, file = "disorders_log_reg_SY2.xlsx", rowNames = TRUE)




disorders_adol_only_log_reg_PY1 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = parent_predictors_mood, data = 
                                                      subset(ons_data_design, adolescent == "adolescent"), family = "quasibinomial")

openxlsx:: write.xlsx(disorders_adol_only_log_reg_PY1, file = "disorders_adol_only_log_reg_PY1.xlsx", rowNames = TRUE)



disorders_adol_only_log_reg_PY2 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = parent_predictors_temper, data = 
                                                      subset(ons_data_design, adolescent == "adolescent"), family = "quasibinomial")

openxlsx:: write.xlsx(disorders_adol_only_log_reg_PY2, file = "disorders_adol_only_log_reg_PY2.xlsx", rowNames = TRUE)








############ NEW PABLO
#### Question 1. Statistical evaluation of sex effects in childhood
####here we are using the linear regression function to get outcomes for children only

parent_outcomes <- c("PY1", "PY2")
parent_predictors <- c("sex") # development doesn't make sense now 


# use function for parent report 
parent_sex_inter_child_reg <- lin_reg_function(outcomes = parent_outcomes, predictors = parent_predictors,
                                               data = subset(ons_data_design, adolescent == "child"), 
                                               family = "gaussian")

openxlsx:: write.xlsx(parent_sex_inter_child_reg, file = " parent_sex_inter_child_reg.xlsx", rowNames = TRUE)


# use function for self report for adolescents 
self_outcomes <- c("SY1", "SY2")
self_predictors <- c("sex") # development doesn't make sense

self_sex_inter_reg <- lin_reg_function(outcomes = self_outcomes, predictors = self_predictors,
                                       data = subset(ons_data_design, adolescent == "adolescent"), 
                                       family = "gaussian")

openxlsx:: write.xlsx(self_sex_inter_reg, file = "self_sex_inter_reg.xlsx", rowNames = TRUE)





###### Question 2. Descriptives of associations with psychopathology and impact
# Here we use my custom-made means function to get SDQs for each informant across many variables.

# within informant for self repeort
s_means_sdqs_SY1 <- func_means_survey(vars_for_means = c("semotion", "shyper", 
                                                         "sconduct", "simpact", "speer"),
                                      predictors = c("SY1dic + sex + adolescent"), 
                                      data = ons_data_design)

openxlsx:: write.xlsx(s_means_sdqs_SY1, file = "s_means_sdqs_SY1.xlsx", rowNames = TRUE)


# across informant for self repeort
p_means_sdqs_SY1 <- func_means_survey(vars_for_means = c("pemotion", "phyper_corrected", 
                                                         "pconduct", "pimpact", "ppeer"),
                                      predictors = c("SY1dic + sex + adolescent"), 
                                      data = ons_data_design)

openxlsx:: write.xlsx(p_means_sdqs_SY1, file = "p_means_sdqs_SY1.xlsx", rowNames = TRUE)


# within informant for parent repeort
p_means_sdqs_PY1 <- func_means_survey(vars_for_means = c("pemotion", "phyper_corrected", 
                                                         "pconduct", "pimpact", "ppeer"),
                                      predictors = c("PY1dic + sex + adolescent"), 
                                      data = ons_data_design)

openxlsx:: write.xlsx(p_means_sdqs_PY1, file = "p_means_sdqs_PY1.xlsx", rowNames = TRUE)


# across informant for parent repeort
s_means_sdqs_PY1 <- func_means_survey(vars_for_means = c("semotion", "shyper", 
                                                         "sconduct", "simpact", "speer"),
                                      predictors = c("PY1dic + sex + adolescent"), 
                                      data = ons_data_design)


openxlsx:: write.xlsx(s_means_sdqs_PY1, file = "s_means_sdqs_PY1.xlsx", rowNames = TRUE)



###### Question 2. Statistical evaluation  of associations with psychopathology and impact
####### use reg function for psychopathology and impact to demonstrate statistically the imapct

# these are the overall outcomes for this question. You are going to use both for each informant (within and across prediction)

self_outcomes <- c("semotion", "shyper", 
                   "sconduct", "simpact", "speer")

parent_outcomes <- c("pemotion", "phyper_corrected", 
                     "pconduct", "pimpact", "ppeer")




###Question 2
#A. Self-reported Mood and Temper outbursts within and across informant outcomes


# These are the predictors for self report.
self_predictors_mood = c("SY1*sex")

self_predictors_temper = c("SY2*sex")

# Regression models self reported outcomes and SY1
self_reg_SY1 <- lin_reg_function(outcomes = self_outcomes , predictors = self_predictors_mood, data = 
                                   subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(self_reg_SY1, file = "self_reg_SY1.xlsx", rowNames = TRUE)

# Regression models self reported outcomes and SY2
self_reg_SY2 <- lin_reg_function(outcomes = self_outcomes , predictors = self_predictors_temper, data = 
                                   subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(self_reg_SY2, file = "self_reg_SY2.xlsx", rowNames = TRUE)


# Regression models parent reported outcomes and SY1
parent_reg_SY1 <- lin_reg_function(outcomes =parent_outcomes , predictors = self_predictors_mood, data = 
                                     subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(parent_reg_SY1, file = "parent_reg_SY1.xlsx", rowNames = TRUE)

# Regression models parent reported outcomes and SY2

parent_reg_SY2 <- lin_reg_function(outcomes = parent_outcomes , predictors = self_predictors_temper, data = 
                                     subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(parent_reg_SY2, file = "parent_reg_SY2.xlsx", rowNames = TRUE)

###Question 2
#B. Parent-reported Mood and Temper outbursts within and across informant outcomes

# these are the predictors for parent report

parent_predictors_mood = c("PY1*sex")


parent_predictors_temper = c("PY2*sex")



parent_child_only_reg_PY1 <- lin_reg_function(outcomes = parent_outcomes , predictors = parent_predictors_mood, data = 
                                                subset(ons_data_design, adolescent == "child"), family = "gaussian")

openxlsx:: write.xlsx(parent_child_only_reg_PY1, file = "parent_child_only_reg_PY1.xlsx", rowNames = TRUE)




parent_child_only_reg_PY2 <- lin_reg_function(outcomes = parent_outcomes , predictors = parent_predictors_temper, data = 
                                                subset(ons_data_design, adolescent == "child"), family = "gaussian")

openxlsx:: write.xlsx(parent_child_only_reg_PY2, file = "parent_child_only_reg_PY2.xlsx", rowNames = TRUE)




self_reg_PY1 <- lin_reg_function(outcomes = self_outcomes , predictors = parent_predictors_mood, data = 
                                   subset(ons_data_design, adolescent == "adolescent"), family = "gaussian")

openxlsx:: write.xlsx(self_reg_PY1, file = "self_reg_PY1.xlsx", rowNames = TRUE)




self_reg_PY2 <- lin_reg_function(outcomes = self_outcomes , predictors = parent_predictors_temper, data = 
                                   subset(ons_data_design, adolescent =="adolescent"), family = "gaussian")

openxlsx:: write.xlsx(self_reg_PY2, file = "self_reg_PY2.xlsx", rowNames = TRUE)



###Question 2 & 3
###C. Do the same thing as above with the categories for disorders as outcomes.
# FIRST, the logistic models for the binary outcomes

disorders_other_outcomes <- c("dcany", "dcemot", "dcanyext",  "pharm", "charm", "exclsch", "help")

disorders_log_reg_SY1 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = self_predictors_mood, data = 
                                            subset(ons_data_design, adolescent == "adolescent"), family = "quasibinomial")

openxlsx:: write.xlsx(disorders_log_reg_SY1, file = "disorders_log_reg_SY1.xlsx", rowNames = TRUE)



disorders_log_reg_SY2 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = self_predictors_temper, data = 
                                            subset(ons_data_design, adolescent == "adolescent"), family = "quasibinomial")

openxlsx:: write.xlsx(disorders_log_reg_SY2, file = "disorders_log_reg_SY2.xlsx", rowNames = TRUE)




disorders_child_only_log_reg_PY1 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = parent_predictors_mood, data = 
                                                       subset(ons_data_design, adolescent =="child"), family = "quasibinomial")

openxlsx:: write.xlsx(disorders_child_only_log_reg_PY1, file = "disorders_child_only_log_reg_PY1.xlsx", rowNames = TRUE)



disorders_child_only_log_reg_PY2 <- lin_reg_function(outcomes = disorders_other_outcomes , predictors = parent_predictors_temper, data = 
                                                       subset(ons_data_design, adolescent == "child"), family = "quasibinomial")

openxlsx:: write.xlsx(disorders_child_only_log_reg_PY2, file = "disorders_child_only_log_reg_PY2.xlsx", rowNames = TRUE)











