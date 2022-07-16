###Simulations for AUGMENT's WP3 experimental study

# The central idea is that we need an instrument to capture sense of agency in a given context. 
# We will through co-work arrive at a max of 4 contexts, e.g. power imbalance, emotionally-laden situation etc. 

# To test that the instrument does what we want it to do, we will create an experiment where the same subject 
# will be asked to rate items across those 4 contexts. The contexts will be experimentally manipulated and presented 
# randomly and in an counter-balanced fashion 

# If the instrument we have developed is successful then the items that are a priori designated to 
# capture context with specificity; e.g. items that would provide information about power imbalance should predict
# sense of agency with a higher probability than items that are designed to provide information about
# emotionally-laden situations.

# To test this, I propose to run a multinomial logistic regression. However, since the main question
# here is about the post-hoc tests, i.e. how much better context A is predicted over context B, 
# the question reduces to a series of logistic regression comparisons. Therefore, I propose to 
# simulate logistic regression model for alphas corrected for multiple comparisons.

# The question arises about the criterion of the difference. Two criteria are important here; 
# a) The absolute probability with which the a priori chosen items correspond to the context. For
# example, what is the probability that items a priori of category A will correspond to 
# the experimentally induced context A? 
# This question is a simple binomial of the difference to .8 within certain bounds. I have code for this
# and will not revisit it below. 
# 
# b) The specificity of the prediction, i.e. how much more likely A items is to be associated with its 
# corresponding context (Context A) than, say, B items.
# Arbitrarily, I am choosing a difference of 20% and


# to run this, we will need a situation where we have two balanced groups as an outcome (~ 50/50, coded 0/1)
# and where the independent variable predicts with high accuracy, e.g. if it is a 0/1 variable, it 
# woudl ideally only turn a 1 only in the right half of cases. This can be constructed 
# similarly for scores. 

# coming to think of it, the best thing to do, would be to show that the predictor, in our case
# our variable that predicts, say, power imbalances as a context, does so with a high AUC, e.g.
# >= .8

# Perhaps, thinking further, we should run this model, with an outcome
###############READ ME###############
# NOTE 1: Create data that generate the desired AUC
# I have got this from here: https://stats.stackexchange.com/questions/422926/generate-synthetic-data-given-auc/424213#424213
# They refer to the paper here: http://dx.doi.org/10.5093/ejpalc2018a5, that I have also used before in order to establish the relationship 
# between different typse of effect sizes (e.g. OR with d). 
# NOTE 2: If you are running the code below for the first time and want to check, make sure you reduce the number of sims
# to a low number (e.g. 100 or 500), otherwise you will wait for quite long. The variable to control this is n_sims



#########SIMULATIONS FOR AUC for AUGMENT##########
library(pROC)
library(tidyverse)
library(ggplot2)
#first write a function to get a single AUC value out of a logistic based on synthetic data

auc_sim_function <- function(n, auc_value, criterion_value){ # n = sample size, auc_value = auc value of interest, criterion value = what auc  
                                                                    # you want your auc_value to be significantly different from 
  
  auc <- auc_value
  
  t <- sqrt(log(1/(1-auc)**2)) # this is the relationship between t and auc--this is a hack to arrive at the auc you need
  z <- t-((2.515517 + 0.802853*t + 0.0103328*t**2) / # turn the t to z according to Zelen and Severo 1993
            (1 + 1.432788*t + 0.189269*t**2 + 0.001308*t**3))   
  d <- z*sqrt(2) # turn to d which makes generating the continuous variable below easier
  
  x <- c(rnorm(n/2, mean = 0), rnorm(n/2, mean = d)) # this creates the differences on the basis of d across the two labels
  y <- c(rep(0, n/2), rep(1, n/2)) # the two labels
  
  # create as data frame
  auc_sim <- data.frame(cbind(x,y))
  
  log_mod_1 <- glm(y~x, family="binomial") # your logistic regression
  
  pred_log_mod_1  <- predict(log_mod_1,type=c("response")) # collect predictions
  
  roc_curve_mod <- roc(y ~ pred_log_mod_1) # get roc curves
  
  (v <- var(roc_curve_mod)) #, method = "bootstrap")) #collect the variances of the ROC. If you choose bootstrap you will have to wait for quit long
                                                      # on standard machine. The differences are minimal.
  
  b <- roc_curve_mod$auc - criterion_value  # this is where you get the difference in AUCs
  se <- sqrt(v)         # turn variances to SEs
  (se <- sqrt(v))
  
  
  z <- (b / se) # get z-values in order to do get p-value next
  p_val <- 2 * pt(-abs(z), df=Inf)  ## two-sided test ; arguably could be one-sided
  
}


# now feed the function into a loop that runs it over several sample sizes
num_over_crit <- 0
sample_size_for_sims <- seq(10,1000, 20) 
n_sims <- 5000 # may be an overkill


for(i in 1:length(sample_size_for_sims)){
  
  num_over_crit[i] <- sum(replicate(n_sims, auc_sim_function(sample_size_for_sims[i], 0.8 , 0.72))<0.0125)
  
}

perc_over_crit <- num_over_crit/n_sims
perc_over_crit

df_results_auc_sim <- data.frame(cbind(perc_over_crit, sample_size_for_sims))

df_results_auc_sim %>% 
  filter(perc_over_crit>=.9 )

ggplot(df_results_auc_sim, aes(x = sample_size_for_sims, y = perc_over_crit)) +
  geom_point() +
  xlim(0,1000) +
  ylim(0.20, 1) +
  geom_hline(yintercept = 0.9, color = "red", linetype = "dashed") +
  xlab("Sample Size")+
  ylab("Percentage of p-values < corrected alpha") +
  ggtitle("Power to detect AUC > criterion in Experiment")
##########END OF USE THIS##############################################################



