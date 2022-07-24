library(tidyverse)
weight.height <- read.csv("~/argyris_code/weight.height.csv")
summary(lm(Weight ~ Gender, data = weight.height))$r.squared

weight.height %>% 
  group_by(Gender) %>% 
  summarise(mean(Weight))



n_sample <- 10000  
b_0 <- 135.86
b_1 <-51.1605
X_0 = rep(1, n_sample)
X_1 <- rep(c("Male", "Female"), each = 5000)
er = rnorm(n_sample, mean = 0 , sd = 5)

y <- b_0 + b_1*(X_1=="Male") +  er
 
 df_test <- data.frame(cbind(y = y, x_1 = X_1 ))
# 
# summary(lm(y ~ X_1, data = df_test))



num_sims = 1000
beta_hat_2 = rep(0, num_sims)
r_sq = rep(0, num_sims)
catch_y <- matrix(NA, nrow = n_sample, ncol= num_sims) # in order to get the y-values

for(i in 1:num_sims) {
  er           = rnorm(n_sample, mean = 0 , sd = 19)
  df_test$y    = b_0 + b_1*(X_1=="Male") +  er
  fit           = lm(y ~ x_1, data = df_test)
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






