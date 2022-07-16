# a little function to solve one of Maria's homework problems 
# she was asked to do the tedious job of casting a pair of dice 7 times in a row and repeat this 10 times. For each 
# row she was asked to calculate the min, max and average of the difference. 
# I didn't get the point of the exercise, but thought we could do it here. 
# I simulated it and recorded the outcomes. 
# the difference converges to 5, as you one would expect 
n_rounds <- 10
n_rolls <- 100

dice_diff <- function(n_rolls, nrounds){
alpha <- matrix(0, n_rounds, n_rolls)
for (i in 1:n_rounds){
 alpha[i,] <- round((runif(n_rolls, 1, 6)))
}

#alpha
alpha_min <- apply(alpha, 1, min)
alpha_max <- apply(alpha, 1, max)
alpha_dif <- alpha_max - alpha_min
#alpha_dif
alpha_dif_mean <- mean(alpha_dif)
alpha_dif_sd <-sd(alpha_dif)
return(c(alpha_dif_mean,alpha_dif_sd ))
}

# and if you wanted to iterate this over several rounds or rolls
vec1 <- c(2,4,6,8,10,20,50,80,100) # the number of rolls in this case
vec2 <- 100 # the number of rounds
mapply(dice_diff,vec1,vec2)
