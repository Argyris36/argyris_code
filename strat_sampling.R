### code snippet for creating synthetic dataset and stratified random sampling of cases from it.


# let there be a simple dataset that represents two measurements taken in 4 different countries.
# This is reflected in the two continuuous variables of the dataset ("a", and "b") from which you 
# want to randomly sample in a stratified way, that is, sampling the same percentage per country.
#. The stratifying variable here is "var_c" 
# which represents the countries.

# first create toy dataset

num_rows_df <- 1000 # n rows for your dataset


num_countries <- 4 # n countries



df_test <- data.frame(cbind(var_a = rnorm(num_rows_df,0,1), # this piece of code, creates the fictional dataset
                                                  # with two continuous and one categorical variables
                            
                            
                            var_b = rnorm(num_rows_df, 0.5, 1), var_c = c(rep("A",10*(num_rows_df/100)), rep("B",25*(num_rows_df/100)), rep("C", 
                                                                                 50*(num_rows_df/100)), rep("D",15*(num_rows_df/100))) ))
                            
                            #var_c = rep(LETTERS[1:4],times=25))) # toy df


dim(df_test) # check

countries <- c("A", "B", "C", "D") # create the list of countries corresponding to the letters in the dataframe

perc_subsample <- 30 # the percentage you want from teh subsample, you can change it of course


nrow_rand_vec <- 0
subsample_vec <- list() # create an empty list for the susample vector
df_test_subsample <- list () # create an empty list for the susample vector
for (i in 1: length(countries)){
  
  nrow_rand_vec[i]<- (perc_subsample*nrow(df_test[df_test$var_c == countries[i],]))/100 #applying the rule of three
  # to  find out how many of each country stratum
  
  
  subsample_vec[[i]] <- sample(nrow(df_test[df_test$var_c == countries[i],]), nrow_rand_vec[i]) # sample the row numbers
  
  df_test_subsample[[i]] <- df_test[df_test$var_c == countries[i],][subsample_vec[[i]], ] # subset the dataframe
  
}

df_test_subsample # this is the liσt of dataframes





