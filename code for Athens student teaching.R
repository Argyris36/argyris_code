library(tidyverse)
adhd_mean <- 10
adhd_sd <- 2
n_adhd <- 100000

a_norm_dist <- data.frame(values = rnorm(n_adhd, adhd_mean, adhd_sd))


a_norm_dist %>% 
  ggplot(aes(values))+
  geom_histogram(aes(y = ..density..), bins = 30) +
  ggtitle("Distribution of ADHD symptoms")+
  ylab("Number of children") +
  xlab("ADHD scores")+
  stat_function(fun = dnorm, args = list(mean = mean(a_norm_dist$values), 
                                         sd = sd(a_norm_dist$values)), col = "blue")



iq_mean <- 100
iq_sd <- 15
n_iq <- 100000

iq_norm_dist <- data.frame(values = rnorm(n_adhd, iq_mean, iq_sd))







iq_norm_dist %>% 
  ggplot(aes(values))+
  geom_histogram(aes(y = ..density..), bins = 30) +
  ggtitle("Distribution of IQ scores")+
  ylab("Number of children") +
  xlab("IQ scores")+
  stat_function(fun = dnorm, args = list(mean = mean(iq_norm_dist$values), 
                                         sd = sd(iq_norm_dist$values)), col = "blue")




heights <- c(169.0, 165.0, 166.9, 168.1, 171.6, 172.7, 174.2,  176.7, 176.8)
year <- c(1920, 1928, 1931, 1942, 1963, 1968, 1981, 1988, 2000)

plot(year,heights)

df_heights_greece <- data.frame (heights = heights, year = year)

df_heights_greece %>% 
  ggplot(aes(x = year, y = heights)) + 
  geom_point()+
  ylim(160, 180)+
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth")+
  ggtitle("Change in Heights in Greece over time",subtitle = "data from Papadimitriou et al 2002") +
  ylab("height in cm") +
  xlab("year")
  
