library("tidyverse")
library("corrr")  # install.packages("corrr") in console if missing
weight.height %>%
  select(Height, Weight) %>%
  correlate(method = "spearman") %>%
  shave() %>%
  fashion() %>%
  knitr::kable()


weight.height %>%
  group_by(Gender) %>%
  summarize(cor=cor(Height, Weight, method = "spearman"))%>%
  knitr::kable()

weight.height %>%
 # group_by(Gender) %>%
  summarize(mean_height = mean(Height), mean_weight = mean(Weight), sd_height = sd(Height), 
            sd_weight = sd(Weight))%>%
  knitr::kable()


my_Sigma <- matrix(c((sd(weight.height$Height))^2, .93*sd(weight.height$Height)*sd(weight.height$Weight), 
                     .93*sd(weight.height$Height)*sd(weight.height$Weight),
                     sd((weight.height$Weight))^2), ncol = 2)
my_Sigma


sim_weight.height <- MASS::mvrnorm(10000, 
                      c(Height = mean(weight.height$Height), Weight = mean(weight.height$Weight)), 
                      my_Sigma) %>% 
                      as.tibble() %>% 
                      mutate(type = "simulated")
sim_weight.height


alldata <- bind_rows(weight.height %>% mutate(type = "real"), 
                     sim_weight.height)


ggplot(alldata, aes(Height, Weight)) +
  geom_point(aes(colour = type), alpha = .1)


sim_weight.height <- MASS::mvrnorm(10000, 
                                   c(Height = mean(weight.height$Height), Weight = mean(weight.height$Weight)), 
                                   my_Sigma) %>% 
                                  as.tibble() %>% 
                                  mutate(type = "simulated")

p_value_sim <- cor.test(sim_weight.height$Height, sim_weight.height$Weight)$p.value

# take the above, create a function, and repeat several times over different sample sizes
# then count and plot number of p_values below a criterion


##### test predictions 
#### https://intjem.biomedcentral.com/articles/10.1186/s12245-018-0212-9 

test_weight.height <- weight.height

test_weight.height <- test_weight.height %>% 
  mutate(e_Weight = ((Height)*2.54)-100)


cor.test(test_weight.height$Weight/2.25, test_weight.height$e_Weight)

differences <- test_weight.height$Weight/2.25 - test_weight.height$e_Weight
mean(differences)
percentage_dif <- (differences/test_weight.height$Weight/2.25)
mean(mean(percentage_dif ))

                   