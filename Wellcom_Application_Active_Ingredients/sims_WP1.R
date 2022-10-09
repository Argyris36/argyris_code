
library(TOSTER)
powerTOSTtwo(alpha = 0.05,
             statistical_power = 0.80,
             low_eqbound_d = -0.40,
             high_eqbound_d = 0.40)


library(TOSTER)
powerTOSTtwo(alpha = 0.15,
             statistical_power = 0.80,
             low_eqbound_d = -0.30,
             high_eqbound_d = 0.30)


####WP1 simulation

library(lme4)
library(lmerTest)
library(broom.mixed)
library(broom)
library(emmeans)

avg_surp_self <- 54.5
avg_surp_NO_self <- 50
avg_NO_surp_self <- 50
avg_NO_surp_NO_self <- 50

sd <- 15

n <-80

trials <- 15

within_corel <- 0.5

n_sims <-10

p_vals <- c()


power_func_WP1 <- function( n){
for (i in 1:n_sims){
# surprise AND self
dat_surp_self <- rnorm_multi(n = n, # create two correlated variables
                             mu = rep(avg_surp_self, trials),
                             sd = rep(sd, trials),
                             r = c(within_corel), 
                             varnames = paste("T_", seq(1:trials)),
                             empirical = FALSE)


dat_surp_self_long <- dat_surp_self %>%
  pivot_longer(
    cols = starts_with("T"),
    names_to = "time", 
    #names_prefix = "Condd",
    values_to = "score")
id <- rep(1:n, each=trials)
cond <- rep("surp_self", n*trials)
time<- rep(1:trials, n)
dat_surp_self_long <- data.frame(cbind(id,cond, time, dat_surp_self_long))

dim(dat_surp_self_long)
colMeans(dat_surp_self)

# surprise but NO self
dat_surp_NO_self <- rnorm_multi(n = n, # create two correlated variables
                             mu = rep(avg_surp_NO_self, trials),
                             sd = rep(sd, trials),
                             r = c(within_corel), 
                             varnames = paste("T_", seq(1:trials)),
                             empirical = FALSE)


dat_surp_NO_self_long <- dat_surp_NO_self %>%
  pivot_longer(
    cols = starts_with("T"),
    names_to = "time", 
    #names_prefix = "Condd",
    values_to = "score")
id <- rep(1:n, each=trials)
cond <- rep("surp_NO_self", n*trials)
time<- rep(1:trials, n)
dat_surp_NO_self_long <- data.frame(cbind(id,cond, time, dat_surp_NO_self_long))

dim(dat_surp_NO_self_long)
colMeans(dat_surp_NO_self)

# No surprise but  self
dat_NO_surp_self <- rnorm_multi(n = n, # create two correlated variables
                                mu = rep(avg_NO_surp_self, trials),
                                sd = rep(sd, trials),
                                r = c(within_corel), 
                                varnames = paste("T_", seq(1:trials)),
                                empirical = FALSE)


dat_NO_surp_self_long <- dat_NO_surp_self %>%
  pivot_longer(
    cols = starts_with("T"),
    names_to = "time", 
    #names_prefix = "Condd",
    values_to = "score")
id <- rep(1:n, each=trials)
cond <- rep("NO_surp_self", n*trials)
time<- rep(1:trials, n)
dat_NO_surp_self_long <- data.frame(cbind(id,cond,trials, dat_NO_surp_self_long))



dim(dat_NO_surp_self_long )
colMeans(dat_NO_surp_self)

# NO surprise and NO  self
dat_NO_surp_NO_self <- rnorm_multi(n = n, # create two correlated variables
                                mu = rep(avg_NO_surp_NO_self, trials),
                                sd = rep(15, trials),
                                r = c(0.5), 
                                varnames = paste("T_", seq(1:trials)),
                                empirical = FALSE)


dat_NO_surp_NO_self_long <- dat_NO_surp_NO_self %>%
  pivot_longer(
    cols = starts_with("T"),
    names_to = "time", 
    #names_prefix = "Condd",
    values_to = "score")
id <- rep(1:n, each=trials)
cond <- rep("NO_surp_NO_self", n*trials)
time<- rep(1:trials, n)
dat_NO_surp_NO_self_long <- data.frame(cbind(id, cond, trials, dat_NO_surp_NO_self_long))

dim(dat_NO_surp_NO_self_long )
colMeans(dat_NO_surp_NO_self)


df_for_sim_WP1 <-  data.frame(id = c(dat_surp_self_long$id, dat_surp_NO_self_long$id, dat_NO_surp_self_long$id, 
                              dat_NO_surp_NO_self_long$id), 
                  cond = c(dat_surp_self_long$cond, dat_surp_NO_self_long$cond,dat_NO_surp_self_long$cond,
                           dat_NO_surp_NO_self_long$cond),
                  time = c(dat_surp_self_long$time, dat_surp_NO_self_long$time, dat_NO_surp_self_long$time,
                           dat_NO_surp_NO_self_long$time),
                  mood = c(dat_surp_self_long$score, dat_surp_NO_self_long$score, dat_NO_surp_self_long$score,
                           dat_NO_surp_NO_self_long$score))

df_for_sim_WP1 %>% 
  group_by(cond) %>% 
  summarise(avg_score = mean(mood))

#WP1_mod <- lmer(mood ~ factor(cond) + (1|id), data = df_for_sim_WP1)
p_vals[i] <-coef(summary(WP1_mod <- lmer(mood ~ factor(cond) + (cond|id), data = df_for_sim_WP1)))[4,5]

#lmer_int <- lmer(mood ~ 1 + cond + time + cond:time + (1|subject), data = df_for_sim, REML = FALSE,) # fit the model with the interaction (score~time*cond + time*surprise , data = df_for_sim )
#lmer_null <- lmer(mood ~ 1 + cond + time  + (1|subject), data = df_for_sim, REML = FALSE,) # fit the model without the interaction  lm_null <- lm(mood ~ 1 + cond + time, data = df_for_sim)
#p_vals[i] <- anova(lmer_int, lmer_null)$`Pr(>Chisq)`[2] # put the p-values in a list

}
sum(p_vals<0.05)/length(p_vals)
}

prop_abo_thresh_p_value_WP1 <- lapply(seq(20, 200, by = 20),power_func_WP1)


power_data_WP1<- data.frame(cbind(x = seq(20, 200, by = 20), y = prop_abo_thresh_p_value_WP1))
#plot(seq(50, 250, by = 10),prop_abo_thresh_p_value)

power_WP1 <- power_data_WP1 %>% 
  ggplot(aes(x= as.numeric(x), y = as.numeric(y))) + 
  geom_point() +
  geom_hline(yintercept = 0.9, colour = "red")

power_WP1 + 
  labs(x= "Sample Size", y="Power") +
  annotate("text",x= 120,y=0.6,label="simulations = 500, \n alpha = 0.05, \n d = 0.3")+ #+ coord_cartesian(ylim=c(-0,26),clip="off")+
  ggtitle("Power Simulation for WP1")

emmeans(WP1_mod, list(pairwise ~ cond), adjust = "tukey")$`pairwise differences of cond`[5][1]
library(lmerTest)
anova(WP1_mod)
# m.emm.df <-
#   m.emm %>%
#   broom::tidy()
# 
# contrast(m.emm, 'tukey') %>%
#   broom::tidy() %>%
#   head(6)
