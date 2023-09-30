
# a script to compare regression coefficients -----------------------------
# the idea is: you have coefficients from different regression models
# in order to compare them you turn their difference into a z-score
# depending on the magnitude of the z-score
# you infer whether the difference between the coefficient is significant
# -------------------------------------------------------------------------



# 1. simulate dataset,  if you need to ------------------------------------
# I am doing this to emulate the data Jamilah has from Pisa
# basically I use a function from the faux package to simulate correlated variables

library(faux) 
df_sim_cor_irrit_intern <- rnorm_multi(n = 1000,  # 1000 subjects
                   mu = c(0, 0, 0, 0), # let all the means be the same
                   sd = c(1, 1, 1, 1), # let all sds be the same
                   r = c(0.5, 0.2, 0.3, 0.1, 0.1, 0.1), # these are the correlations
                   #A with each B, C, D; B with C, D, C with D
                   
                
                   varnames = c("irrit", "dep", "headache", "stomach_ache"),# just giving it names
                   empirical = FALSE)

cors_df_sim_cor_irrit_intern <- cor(df_sim_cor_irrit_intern ) # this is how the correlations look like
cors_df_sim_cor_irrit_intern 
corrplot:: corrplot(cors_df_sim_cor_irrit_intern )





# 2. this function allows you to run a regression with one outcome over many predictors --------
# I am doing this so that I don't have to run regression models by copying and pasting. 


lin_reg_function <- function(outcomes, predictors, data, family){ # I create a function with these arguments
  # the empty vector
  models_reg <- vector("list", length = length(outcomes)) # where I create this empty list
  
  df_list_reg <- vector("list", length = length(outcomes)) # and this empty list
  
  for(i in 1:length(predictors)){  # and run a loop 
    
    
    models_reg[[i]] <- as.formula(paste(outcomes, paste(predictors[i], collapse = "+"), # this is the way R
                                        sep = "~")) # can read the command as a linear model of the type y ~ x
                                                    # try to take this outside of the function
                                                    # to see what happens
    
    df_list_reg[[i]] <- data.frame(summary(eval(bquote(glm(.(models_reg[[i]]), na.action = na.omit, family = family , data))))$coefficients)
    # here you told it to run the model

    
  }
  return(df_list_reg) # and you ask it to return from the function that list of coefficients
}



# 3. use the function  ----------------------------------------------------


y_variable <- "irrit"
x_variables <- colnames(df_sim_cor_irrit_intern[2:length(colnames(df_sim_cor_irrit_intern))]) 
list_of_regression_models <- lin_reg_function(outcomes = y_variable, predictors = x_variables, data = df_sim_cor_irrit_intern, 
                 family = "gaussian")


# 4.  Extract the betas and standard errors -------------------------------

betas <- 0
std_errors <-0
for(i in 1: length(list_of_regression_models)){
betas[i] <- list_of_regression_models[[i]][[2,1]]  # this is basic subsetting play around with it
std_errors[i] <- list_of_regression_models[[i]][[2,2]]
}



# put them in dataframe ---------------------------------------------------


df_estimates <- data.frame(outcome = rep(y_variable, length(x_variables)),
                           predictors = c(x_variables), 
                           cbind(betas = betas, std_errors = std_errors)
                           )
df_estimates



# create the z-scores -----------------------------------------------------
# see here on why I am doing z-scores https://www.r-bloggers.com/2020/02/how-to-compute-the-z-score-with-r/

z_scores <- 0
for(i in 1: (nrow(df_estimates))){
  
  z_scores [i] <-  (betas[1] - betas[i])/
    sqrt((std_errors[1])^2 + (std_errors[i])^2)
  
}

z_scores



# now generate the df with the significant differences --------------------

lst_rows <- data.frame(expand.grid(y_variable,x_variables)) # create the combinations

row_combs <- 0
row_combs_2 <- 0
for (i in 1: nrow(lst_rows)){ # run a loop to generate the rownames for the new dataframe
  
  row_combs[i] <- paste(lst_rows[i,1], lst_rows[i,2])
  row_combs_2 [i] <- paste(row_combs[1], "_vs_", row_combs[i])
}
row_combs
row_combs_2

df_coeff_comparisons <- data.frame(z_score = z_scores)
rownames(df_coeff_comparisons) <- row_combs_2 # use row names
df_coeff_comparisons$significance =   ifelse(df_coeff_comparisons$z_score>1.96, 1, 0) # add a variable for significance -----------------------------------------

df_coeff_comparisons 


#############################################################################################

#  here is code to get regressions for all the variables ------------------

#for this I only need to change slightly the outcome variables so as to have
# more than one outcomes (notice the [i] next to the outcomes)

new_lin_reg_function <- function(outcomes, predictors, data, family){ # I create a function with these arguments
  # the empty vector
  models_reg <- vector("list", length = length(outcomes)) # where I create this empty list
  
  df_list_reg <- vector("list", length = length(outcomes)) # and this empty list
  
  for(i in 1:length(predictors)){  # and run a loop 
    
    
    models_reg[[i]] <- as.formula(paste(outcomes[i], paste(predictors[i], collapse = "+"), # this is the way R
                                        sep = "~")) # can read the command as a linear model of the type y ~ x
    # try to take this outside of the function
    # to see what happens
    
    df_list_reg[[i]] <- data.frame(summary(eval(bquote(glm(.(models_reg[[i]]), na.action = na.omit, family = family , data))))$coefficients)
    # here you told it to run the model
    
    
  }
  return(df_list_reg) # and you ask it to return from the function that list of coefficients
}


# this creates a dataframe that contains the names that you will need
# I use the function  combn to generate all possible combinations ofvariables
# from the dataset, which I will then use as x and y variables. 
df_test <- data.frame(combn(colnames(df_sim_cor_irrit_intern),2))

y_vars <- df_test[1,1:ncol(df_test)] # the first row of the df_test
x_vars <- df_test[2,1:ncol(df_test)] # the second row


# now I use the new function
new_list_of_regression_models <- new_lin_reg_function(outcomes = y_vars, predictors = x_vars, 
                                                     data = df_sim_cor_irrit_intern, 
                                                     family = "gaussian")


new_list_of_regression_models




# Now Extract the betas and standard errors as before-------------------------------

betas <- 0
std_errors <-0
for(i in 1: length(new_list_of_regression_models)){
  betas[i] <- new_list_of_regression_models[[i]][[2,1]]  # this is basic subsetting play around with it
  std_errors[i] <- new_list_of_regression_models[[i]][[2,2]]
}



# As before put them in dataframe ---------------------------------------------------


df_estimates <- data.frame(outcome = unlist(df_test[1,]),
                           predictors = unlist(df_test[2,]), 
                           cbind(betas = betas, std_errors = std_errors)
)
df_estimates


# Now create a new dataset with the labels and the the coefficients ---------------------------------------------------
the_labels <- data.frame(combn(paste(df_estimates$outcome, df_estimates$predictors),2)) # all unique combinations of labels in pairs
the_betas <- data.frame(combn(paste(df_estimates$betas),2)) # all unique combinations of betas in pairs
the_errors <- data.frame(combn(paste(df_estimates$std_errors),2))  # all unique combinations of errors in pairs

vector_of_labels <- 0
vector_of_betas <- 0
vector_of_errors <- 0


for(i in 1: ncol(the_labels)){
vector_of_labels [i] <- paste(the_labels[1,i],"vs", the_labels[2,i]) # put them next to each other
vector_of_betas [i] <- paste(the_betas[1,i], the_betas[2,i] )
vector_of_errors [i] <- paste(the_errors[1,i],the_errors[2,i] )
}

df_for_z <- data.frame(comparisons = vector_of_labels, coefficients = vector_of_betas, #put them in dataframe
           errors = vector_of_errors)

df_for_z$coef_a <- stringr:: str_split_fixed(df_for_z$coefficients, " ", 2)[,1] # you will need to split the betas and errors from each other to be able to do math operations on them
df_for_z$coef_b <- stringr:: str_split_fixed(df_for_z$coefficients, " ", 2)[,2] 


df_for_z$errors_a <- stringr:: str_split_fixed(df_for_z$errors, " ", 2)[,1] # same with the errors
df_for_z$errors_b <- stringr:: str_split_fixed(df_for_z$errors, " ", 2)[,2]

df = subset(mydata, select = -c(x,z) )
df_for_z <- subset(df_for_z, select = -c(coefficients, errors)) # drop the non-split columns


df_for_z[,2:length(df_for_z)] <- apply(df_for_z[,2:length(df_for_z)], 2, as.numeric) # turn all these columns into numeric


df_for_z$z_scores <- (df_for_z$coef_a - df_for_z$coef_b)/ # generate the z_scores
  sqrt((df_for_z$errors_a)^2 + (df_for_z$errors_b)^2)

df_for_z$significance =   ifelse(df_for_z$z_score>1.96, 1, 0) # declare siginificance


df_for_z # check

