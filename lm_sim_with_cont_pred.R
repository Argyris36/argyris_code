weight.height <- read.csv("~/argyris_code/weight.height.csv")
summary(lm(Weight ~ Height, data = weight.height))


# b_0 <- -350.73719
# b_1 <-7.71
# X_0 = rep(1, n_sample)
# X_1 <- runif(n_sample, min = 54, max = 78 )
# er <- rnorm(n_sample, 5,5)
# 
# y <- b_0 + b_1*X_1 + er
# 
# df_test <- data.frame(cbind(y = y, x = X_1))
# 
# summary(lm(y ~ X_1, data = df_test))
        
   
n_sample <- 10000  
b_0 <- -350.73719
b_1 <-7.71
X_0 = rep(1, n_sample)
X_1 <- rnorm(n_sample, mean = 66.37, sd = 3.85 )
num_sims = 1000
beta_hat_2 = rep(0, num_sims)
r_sq = rep(0, num_sims)
catch_y <- matrix(NA, nrow = n_sample, ncol= num_sims) # in order to get the y-values

for(i in 1:num_sims) {
  er           = rnorm(n_sample, mean = 0 , sd = 12)
  df_test$y    = b_0*X_0  + b_1 * X_1 + er
  fit           = lm(y ~ X_1, data = df_test)
  beta_hat_2[i] = coef(fit)[2]
  r_sq[i] = summary(fit)$r.squared
  catch_y[,i] <- predict(fit) #df_test$y #  extract the y values
  
}             

mean(beta_hat_2)
mean(r_sq)

# mean(catch_y[,1]) # to view individual y-columns
# sd(catch_y[,1])

avg <- mean(apply(catch_y, 1, mean)) # the mean of all y-columns
sds <- mean(apply(catch_y, 2, sd)) # the sd of all y-columns

