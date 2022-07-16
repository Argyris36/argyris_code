# a little code snippet to create a synthetic dataset.

# num_rows <- 1000 # however many you want to have 
# num_cols <- 10 # however many variables you need
# num_continuous <- 5 # how many variables you want to be continuous
# num_binary <- 5 # how many variables you want to be binary
# prob <- seq(from =0, to = 1, length.out = num_binary)  #your probability for the binomial distribution, could also choose a vector of probabilities 
# 
# df_synthetic <- matrix(NA, nrow = num_rows, ncol= num_cols) # creates the empty dataframe
# 
# 
# for (j in 1: num_cols){ #start the loop for creating the variables you need
#   
#   df_synthetic[,j:num_continuous] <- rnorm(num_continuous,0,1) 
#   df_synthetic[, (num_continuous +1):num_cols] <- replicate(num_binary,(rbinom(num_binary,1,prob)))
#   
# }
# 
# dim(df_synthetic) # check dimensions right
# 
# colnames(df_synthetic) <- LETTERS[1:num_cols] # give them the names you like, I just used the inbuilt capital letters function here
# 
# head(df_synthetic) # check looks right  df_synthetic[, (num_continuous +1):num_cols]


# and a neater solution not using a loop
num_rows <- 1000 # however many you want to have 
num_cols <- 10 # however many variables you need
num_cont <- 5 # how many variables you want to be continuous
mean_cont <- 0 # the mean you want for your normal distributions, you could put a vector here, same lenght as your n col for the continuous, e.g. seq(1:4, by = 1)
sd_cont <- 1 # st devs for your normal, as with mean could use vector
num_binary <- 5 # how many variables you want to be binary
prob <- seq(from =0, to = 1, length.out = num_binary)  #your probability for the binomial distribution, for illustration, I put here a vector of probabilities 


cont<- replicate(num_binary,(rbinom(num_binary,1,prob)))
bin <- replicate(num_continuous,rnorm(num_continuous,mean_cont,sd_cont))

df_synthetic <- data.frame(cbind(bin, cont))

colnames(df_synthetic) <- LETTERS[1:num_cols] # give them the names you like, I just used the inbuilt capital letters function here

dim(df_synthetic) # check dimensions right
head(df_synthetic)
