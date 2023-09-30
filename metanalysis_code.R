#### A simplified simulation of metanalysis####

n = 40 # for simplicity, made all sample sizes the same

psychotherapy_active <- rnorm (n, 50, 5) # these are all normal distributions of the response rates
psychotherapy_control <- rnorm(n, 20, 5 )

medication_active <- rnorm(n, 50, 5)
medication_control <- rnorm(n, 40, 5)

outcome_values <- c(psychotherapy_active, psychotherapy_control,
                    medication_active, medication_control)


active_vs_control <- rep(c("active", "control", "active", "control"), each = 40)

psychotherapy_vs_meds <- c(rep(c("psychotherapy", "medication"), each = 80))
psychotherapy_vs_meds

#this is a dataset with all the necessary values
df_metanalysis_simulated <- data.frame(outcome_values = outcome_values, 
           active_vs_control <- active_vs_control, 
           psychotherapy_vs_meds)


# this is a plot with the values
df_metanalysis_simulated  %>% 
  ggplot(aes(x = factor(active_vs_control), y = outcome_values, colour = psychotherapy_vs_meds))+
  geom_point(alpha = 0.5)+
  geom_line(aes(group = psychotherapy_vs_meds)) +
  xlab("Active vs Control") +
  ylab("Response Rate") + 
  ggtitle("psychotherapy vs meds, simulated") 




lm_metanalysis_simulated <- summary(lm(outcome_values ~ psychotherapy_vs_meds + active_vs_control + 
     psychotherapy_vs_meds*active_vs_control, data = test))
lm_metanalysis_simulated
