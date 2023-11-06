library(tidyverse)

set.seed(1974)
n_all = 100
mfq_min = 0
mfq_max = 26
mean_mfq = 5
sd_mfq = 4

set.seed(1974)
all_values_1 <- data.frame(values = truncnorm::rtruncnorm(n_all, mfq_min, mfq_max, mean_mfq, sd_mfq)) # generate a truncated normal 
                                                #distribution emulating the short mfq values
library(RColorBrewer)

all_values_1 %>% 
  ggplot(aes(x= values)) +
  geom_histogram(bins =20, fill = brewer.pal(8, "Blues")[4])+
  ggtitle("distribution of depression scores")+
  geom_vline(xintercept = 10, linetype = "dashed", colour = "red") +
  annotate("text", x = 11.5, y = 8, label = "Depression Caseness \n Threshold", color = "red", size = 4, angle = 90, vjust = -0.5) +
  theme_minimal()+
  ylab("")
  
dichot_values <- ifelse(all_values_1>=10, 1, 0) # dichotomise values using 10 as a threshold

df_all_values <- data.frame(values = all_values_1$values, dichot_values, x_var = rep(0,n_all)) # create dataset that 
                                                                            # has a dummy variable coded zero for plotting
num_colors <- 8
brewer_colors <- brewer.pal(num_colors, "Blues")

p_all_values <- df_all_values %>%       # plot all the values with a threshold 
ggplot(aes(y = values, x = factor(x_var)))+
  geom_point(position = position_jitter(width = 0.2), color = brewer_colors[4]) +
geom_hline(yintercept = 10, linetype = "dashed", colour = "red") + 
  
  annotate ("text", x = 0.54, y = 10.7, label = "depression\n threshold") +
  
  ggtitle("depression scores at time_1", 
          subtitle = "scores above red line indicate depression") +
  
  xlab("") +
  ylab("depression score")  +
  scale_color_manual(values = brewer_colors[4]) + # Set color for data points+
  theme_minimal()
  
  
p_all_values



#### now you take the "high scorers" and you plot them at Time point 1 and then at Time point 2

big_values_1 <- all_values_1[all_values_1>=10] # these are the high scorers

big_values_2 <- truncnorm::rtruncnorm(length(big_values_1), mfq_min, mfq_max, mean_mfq, sd_mfq)  # here you use 
                                                  # the same process as in Time point 1 to 
                                                  # generate random values for Time point 2. 

library(lexicon)

df_big_values_history <- data.frame(ids = seq(1:length(big_values_1)),  # now plot the values
                                                                        # you will see 
                                                                        # big and probably significant
                                                                        # differences
                                   values =  c(big_values_1, big_values_2) , 
                                    time_point = rep(1:2, each = length(big_values_1)), 
                                   first_names = rep(sample(freq_first_names$Name, length(big_values_1), FALSE),2))


test <- t.test(df_big_values_history$values[df_big_values_history$time_point==1], # this is the t-test of the difference
               df_big_values_history$values[df_big_values_history$time_point==2])
test$statistic

df_big_values_history %>% 
  ggplot(aes(x = factor(time_point), y = values, label = first_names))+
  geom_point()+
  geom_line(aes(group = ids)) +
  xlab("Time Point") +
  ylab("depression score") + 
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    size = 6,
    shape = 22,
    fill = "red", alpha = 0.3
  )+
  annotate ("text", x = 1, y = 5, label = paste0("stats of difference between time points: \n"  , "t = ",
                                                 round(test$statistic,2),
                                                 ", df = ", round(test$parameter,2), 
                                                 ", p = ", signif(test$p.value, digits = 2))) +
  
  ggtitle("what happens to high depression scores", subtitle = "red squares are group means")  +
  
theme_minimal()



# Set seed for reproducibility
set.seed(1974)

# Generate data as you described
n_all = 100
mfq_min = 0
mfq_max = 26
mean_mfq = 5
sd_mfq = 4
all_values_1 <- data.frame(values = truncnorm::rtruncnorm(n_all, mfq_min, mfq_max, mean_mfq, sd_mfq))

dichot_values <- ifelse(all_values_1$values >= 10, 1, 0)

# Create dataframe
df_all_values <- data.frame(values = all_values_1$values, dichot_values, x_var = rep(0, n_all))

# Define common names
names_list <- c("John", "Mary", "Michael", "Jennifer", "David", "Linda", "William", "Patricia", "James", "Elizabeth")

# Shuffle both values and names
shuffled_indices <- sample(n_all)
shuffled_values <- df_all_values$values[shuffled_indices]
shuffled_names <- sample(names_list, n_all, replace = TRUE)

# Create shuffled dataframe
df_all_values_shuffled <- data.frame(values = shuffled_values, dichot_values, x_var = rep(0, n_all), names = shuffled_names)

# Plot shuffled data
p_shuffled <- ggplot(df_all_values_shuffled, aes(y = values, x = factor(x_var), label = names)) +
  geom_point(aes(color = factor(dichot_values)), position = position_jitter(width = 0.2), size = 3) +
  geom_hline(yintercept = 10, linetype = "dashed", colour = "red") +
  ggtitle("Shuffled Depression Scores", subtitle = "Dots above red line indicate depression") +
  xlab("") +
  ylab("Depression Score") +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "top") +
  geom_text(nudge_x = 0.2, nudge_y = 0.2, size = 3)

# View the plot
p_shuffled
