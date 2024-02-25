
# create the dataframe with the observed data on dogs
df_greek_dogs <- data.frame(
  
dog_heights = c(cgrk_shep_dogs <- c(71, 65, 61, 67, 72, 71, 63, 70, 71, 67), 
                kokoni_dogs <-c(28, 22, 30, 26, 23, 25, 27, 28, 30, 21)),
dog_type = rep(c("gr_shep", "kokoni"), each = 10)

)

observed_dif <- diff(tapply(df_greek_dogs$dog_heights, df_greek_dogs$dog_type, mean))

n_perms <- 10000
permuted_differences <- 0
for(i in 1:n_perms){
  
  permuted_differences[i] <- diff(tapply(df_greek_dogs$dog_heights, 
                                         sample(df_greek_dogs$dog_type, 
                                                length(df_greek_dogs$dog_type), F), mean))[[1]]
  
}

sum(permuted_differences>0)/length(permuted_differences)

sum(permuted_differences>abs(observed_dif))/length(permuted_differences)

# to plot 
library(tidyverse)
df_permuted_differences <- data.frame(permuted_differences = permuted_differences)

df_permuted_differences %>% 
ggplot(aes(permuted_differences))+
geom_histogram()+
  geom_vline(xintercept = observed_dif, colour = "pink", linetype = "dashed", size = 2)
