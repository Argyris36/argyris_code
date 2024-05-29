# simulate data and get EMMs
library(tidyverse)


set.seed(1974)

# total n
n <- 10^6


gender <- sample(c("Male", "Female"), size = n, replace = TRUE)


age <- sample(16:65, size = n, replace = TRUE)

# I 've put here 5 locations, kept it simple, but the point is that we have all sorts of other covariates and teh EMM
# will keep it each at the stipulated level so that we can draw appropriate inferences, e.g. 18 yo girl in London receiving CBT 
# vs a boy etc.
location <- sample(paste("Location", 1:5), size = n, replace = TRUE)

# a generous mean to avoid having to truncate
depression_t1 <- rnorm(n, mean = 50, sd = 10)

# Here I simulate change in depression score considering for a small correlation with age and gender interaction
# random error
epsilon <- rnorm(n)

# age dichotomised to make things simpler for the sim
younger <- age <= 25
older <- age > 25

# Simulate the change in depression scores
# - older improve more 
# - girls do worse than boys and women better than men.
# I played around with this to get the plot to look like what we have
depression_change <- -0.2 * age + 2* (gender == "Female" & younger) - 0.6 * (gender == "Female" & older) + epsilon

# this now is the dep score at t2
depression_t2 <- depression_t1 + depression_change

# Create the data frame
synthetic_data <- data.frame(
  gender = gender,
  age = age,
  location = location,
  depression_t1 = depression_t1,
  depression_t2 = depression_t2
)



# Calculate the change in depression score
synthetic_data <- synthetic_data %>%
  mutate(depression_change = depression_t2 - depression_t1)

synthetic_data %>% 
  group_by(gender, age) %>% 
  summarise(avg_dep = mean(depression_change))


head(synthetic_data)


# the ancova type lm 
ancova_model <- lm(depression_t2 ~ age * gender + depression_t1 + location, data = synthetic_data)


# just for completeness the ancova without interaction terms
ancova_model_no_interaction <- lm(depression_t2 ~ age + gender + depression_t1 + location, data = synthetic_data)


# summary(ancova_model)
# summary(ancova_model_no_interaction)

# Compare the aics
aic_interaction <- AIC(ancova_model)
aic_no_interaction <- AIC(ancova_model_no_interaction)

# choose on basis of AIC
aic_difference <- aic_no_interaction - aic_interaction
  ifelse(aic_difference <-10, 
print ("prefer interaction"), print("ditch interaction"))



#  a point plot
# plot_1 <-  ggplot(synthetic_data, aes(x = age, y = depression_change, color = gender)) +
#   geom_point(alpha = 0.3, size = 0.5) + 
#   #geom_smooth(method = "lm", se = FALSE) +
#   labs(
#     title = "Change in Depression Score by Age and Gender",
#     x = "Age",
#     y = "Change in Depression Score",
#     color = "Gender"
#   ) +
#   theme_minimal()
# 
# plot_1

# Here is a line plot, could do the same above, but on my local machine the geom_point takes ages.
plot_2 <- ggplot(synthetic_data, aes(x = age, y = depression_change, color = gender)) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Change in Depression Score by Age and Gender",
    x = "Age",
    y = "Change in Depression Score",
    color = "Gender"
  ) +
  theme_minimal()
plot_2

library(emmeans)
# This is how to get the overall Estimated marginal means
emm <- emmeans(ancova_model, ~ gender | age)

# Pairwise comparisons
pairs(emm)

# Summary of estimated marginal means which gives you the average age difference, not of interest to us
summary(emm)

# the emm at 18
emm_age_18 <- emmeans(ancova_model, ~ gender | age, at = list(age = 18))


emm_age_18

# the pairwise difference at 18
pairs(emm_age_18)





# this to get the EMM across the age range
emm_by_age <- emmeans(ancova_model, ~ gender | age, at = list(age = 16:65))

# get data for plotting
emm_by_age_df <- as.data.frame(emm_by_age)

# Create the plot
ggplot(emm_by_age_df, aes(x = age, y = emmean, color = gender)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = 0.2) +
  labs(
    title = "Estimated Marginal Means of Depression Scores by Age and Gender",
    x = "Age",
    y = "Estimated Marginal Mean Depression Score",
    color = "Gender"
  ) +
  theme_minimal()




# but the beauty of this is that you can obtain differences for any level(s) of the covariates. 
# suppose you wanted to find out about 18 year old girls vs boys in location 1 at the mean level of depression
# at t1
# obviously, we could estimate it for anyone with a high score or a low score etc. 

emm_loc_1_dep_avg <- emmeans(ancova_model, ~ gender, 
                        at = list(age = 18, location = "Location 1", depression_t1 = mean(synthetic_data$depression_t1)))



emm_loc_1_dep_avg 


# to address the note from teh package see here

emm_interaction <- emmeans(ancova_model, ~ age * gender | location, at = list(age = 18, location = "Location 1", depression_t1 = mean(synthetic_data$depression_t1)))

# Print the estimated marginal means for the interaction of age and gender at location 1
emm_interaction


# ie identical because we had specified this in the model.

library(ggplot2)

# Convert to data frame for plotting
emm_interaction_df <- data.frame(emm_interaction)

# Here is another way to visualise the interaction effect, or rather see what happens at each age point and location
plot_interaction_at_point <- ggplot(emm_interaction_df, aes(x = factor(age), y = emmean, color = gender)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, position = position_dodge(width = 0.5)) +
  labs(
    title = "Estimated Marginal Means of Depression Scores by Age and Gender",
    x = "Age",
    y = "Estimated Marginal Mean Depression Score",
    color = "Gender"
  ) +
  theme_minimal()

plot_interaction_at_point

