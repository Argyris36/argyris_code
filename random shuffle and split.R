### code snippet for random shuffling and splitting

# first create toy dataset
num_rows_df <- 100 # n rows for your dataset
num_countries <- 4 # n countries


df_test <- data.frame(cbind(var_a = rnorm(num_rows_df <- 100,0,1), 
                            var_b = rnorm(num_rows_df <- 100, 0.5, 1), var_c = rep(LETTERS[1:4],times=25))) # toy df
head(df_test) #check
dim(df_test) # check

rand_vec <- sample(nrow(df_test)) # vector to use to shuffle

df_test <- df_test[rand_vec,] # shuffle 
head(df_test) # check


nrow_rand_vec<- (40*100)/nrow(df_test)  # say you want to choose 40% of rows, how many rows?

subsample_vec <- sample(nrow(df_test),nrow_rand_vec  ) # sample the number of rows randomly

df_test_subsample <- df_test[subsample_vec , ] # create the subsample 


dim(df_test_subsample) # check

df_test_subsample$var_c <- sample(LETTERS[12:16], nrow(df_test_subsample), TRUE)  # replace country 
                                                                                  #labels with fake ones
            



### if you needed to split the rows by the "fake" countries in order to fit the model in each
### country, do the below, which I have commented out.

#split_dfs <- split(df_test_subsample, sample(rep(1:4,times=c(10,10,10,10))))

#and to access each one of them, use a number, e.g. this gives you the first df
#first_df <- split_dfs$"1" 
