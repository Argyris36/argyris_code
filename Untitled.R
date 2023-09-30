library(ggplot2)
library(gganimate)

all_values_1 <- truncnorm::rtruncnorm(100, 0, 26, 5, 4) 
dichot_values <- ifelse(all_values_1>=10, 1, 0)
df_all_values <- data.frame(all_values_1, dichot_values)

df_all_values %>% 
ggplot(aes(all_values_1, colour = dichot_values))+
  geom_


big_values_1 <- all_values[all_values>=10]

big_values_2 <- truncnorm::rtruncnorm(length(big_values_1), 0, 26, 5, 4) 



df_big_values_history <- data.frame(ids = seq(1:length(big_values_1)), big_values_1, big_values_2)


ggplot()


sample(100
       
       
truncnorm::rtruncnorm()