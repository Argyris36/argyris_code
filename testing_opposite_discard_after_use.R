
testing_sampling <- function(probs_for_sampling){
pop_A = 52000
pop_B = 48000
criterion_for_win = 0.5
# pop_A = 5000
# pop_B = 95000
# criterion_for_win = 0.045
samp_prob_A = probs_for_sampling
samp_prob_B = 1.0

# for this you need to first first create a dataframe with probabilities attached to the two groups
population_probs <- data.frame(population = c(rep("0",pop_A), rep("1", pop_B) ), 
                               probability = c(rep(samp_prob_A,pop_A), rep(samp_prob_B, pop_B)))

# then sample from it with different probs in the updated 
pop_sampling_diff_probs <- function(population, n){
  a_sample <- population_probs[sample(nrow(population_probs), n, prob = population_probs$probability), ]$population  # Draw sample of data frame
  a_percentage <- sum(a_sample==0)/length(a_sample) 
  return(a_percentage) 
}


n<- c(10, 30, 50, 100, 150, 300, 500, 1000, 5000)
percent_per_sample <- list()
for(i in 1:length(n)){
  
  percent_per_sample [[i]]<-   replicate(100, pop_sampling_diff_probs(population, n[i]))
}


avg<-0
std<-0
percentage_opposite_result<-0
for(i in 1:length(n)){
  avg[i] <- mean(percent_per_sample [[i]])
  std[i] <- sd(percent_per_sample [[i]])
  percentage_opposite_result[i] <- sum(percent_per_sample [[i]]<=criterion_for_win)/length(percent_per_sample [i])
}

return (percentage_opposite_result)

}

list_percentage_opposite <- list ()
probs <- c(1, 0.95, 0.9, 0.85, 0.8, 0.75, 0.7)
for  (i in 1: length(probs)){
  list_percentage_opposite[[i]] <- testing_sampling(probs[i])
  
}


list_percentage_opposite

list_percentage_opposite_df<- as.data.frame(do.call(cbind, list_percentage_opposite))
list_percentage_opposite_df
colnames(list_percentage_opposite_df) <- paste0("bias_",probs)


#turn to long for plotting
long_list_percentage_opposite_df <- list_percentage_opposite_df %>%
  pivot_longer(cols = starts_with("n") , names_to = "probs", values_to = "percentages")
dim(long_list_percentage_opposite_df)

long_list_percentage_opposite_df <- data.frame(long_list_percentage_opposite_df, samp_size = rep(c("n_10", "n_30", "n_50", "n_100", 
  "n_150", "n_300", "n_500", "n_1000", "n_5000"),each = 7))


long_list_percentage_opposite_df %>% 
  
