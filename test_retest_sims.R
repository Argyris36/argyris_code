
 
 
 
 
 install.packages("faux")
 
 
 library(faux)
 library(ggplot2)
 library(ggrepel)
 library(patchwork)
 
 # set seed for reproducibility
 set.seed(1974)
 n <- 10
 dat_perfect <- rnorm_multi(n = n, 
                    mu = c(15, 15),
                    sd = c(5, 5),
                    r = c(0.99), 
                    varnames = c("measurement_1", "measurement_2"),
                    empirical = FALSE)
 

 df_perfect <- data.frame(measurement_1 = dat_perfect$measurement_1, measurement_2 =  dat_perfect$measurement_2, patient = paste0("patient_", 1:n))
 
 # create scatterplot of the two variables with jittered patient labels
 p_perfect<- ggplot(df_perfect, aes(x = measurement_1, y = measurement_2, label = patient)) +
   geom_point(shape = 1, color = "black") +
   xlab("Measurement Occasion 1") +
   ylab("Measurement Occasion 2") +
   ggtitle("Perfect Agreement") +
   geom_text_repel(point.padding = 1, size = 3, nudge_x = 0.5, nudge_y = 0.5) + # add labels with more jitter
   geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey70") + # add diagonal line
   geom_smooth(method='lm', linetype = "dashed", se = FALSE) +
   annotate("text", x = (max(range(measurement_1)) - 0.25 * max(range(measurement_1))), y = (max(range(measurement_2)) - 0.75 *max(range(measurement_2))), 
            hjust = 1, vjust = 1, # add correlation coefficient to top right
            label = paste0("Correlation = ", round(cor(measurement_1, measurement_2), 2))) +
   scale_x_continuous(limits = c(0, max(range(measurement_1)))) + # set x-axis limits
   scale_y_continuous(limits = c(0, max(range(measurement_2))))
 
 p_perfect
 
############
 dat_perfect_shift_5 <- rnorm_multi(n = 10, 
                            mu = c(15, 20),
                            sd = c(5, 5),
                            r = c(0.99), 
                            varnames = c("measurement_1", "measurement_2"),
                            empirical = FALSE)
 
 
df_perfect_shift_5 <- data.frame(measurement_1 = dat_perfect_shift_5$measurement_1, measurement_2 = dat_perfect_shift_5$measurement_2, patient = paste0("patient_", 1:n))
 
 

p_perfect_shift_5 <- ggplot(df_perfect_shift_5, aes(x = measurement_1, y = measurement_2, label = patient)) +
  geom_point(shape = 1, color = "black") +
  xlab("Measurement Occasion 1") +
  ylab("Measurement Occasion 2") +
  ggtitle("Perfect Consistency") +
  geom_text_repel(point.padding = 1, size = 3, nudge_x = 0.5, nudge_y = 0.5) + # add labels with more jitter
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey70") + # add diagonal line
  geom_smooth(method='lm', linetype = "dashed", se = FALSE) +
  annotate("text", x = (max(range(measurement_1)) - 0.25 * max(range(measurement_1))), y = (max(range(measurement_2)) - 0.75 * max(range(measurement_2))), 
           hjust = 1, vjust = 1, # add correlation coefficient to top right
           label = paste0("Correlation = ", round(cor(measurement_1, measurement_2), 2))) +
  scale_x_continuous(limits = c(0, max(range(measurement_1)+5))) + # set x-axis limits
  scale_y_continuous(limits = c(0, max(range(measurement_2)+5))) # set y-axis limits 

p_perfect_shift_5



############

 dat_random <- rnorm_multi(n = 10, 
                                    mu = c(15, 15),
                                    sd = c(5, 5),
                                    r = c(0.01), 
                                    varnames = c("measurement_1", "measurement_2"),
                           empirical = FALSE)

                           
                           
                           df_random <- data.frame(measurement_1 = dat_random$measurement_1, measurement_2 = dat_random$measurement_2, patient = paste0("patient_", 1:n))
                           
                           
                           
                           p_random <- ggplot(df_random, aes(x = measurement_1, y = measurement_2, label = patient)) +
                             geom_point(shape = 1, color = "black") +
                             xlab("Measurement Occasion 1") +
                             ylab("Measurement Occasion 2") +
                             ggtitle("Very low agreement/consistency") +
                             geom_text_repel(point.padding = 1, size = 3, nudge_x = 0.5, nudge_y = 0.5) + # add labels with more jitter
                             geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey70") + # add diagonal line
                             geom_smooth(method='lm', linetype = "dashed", se = F) +
                             annotate("text", x = (max(range(measurement_1)) - 0.25 * max(range(measurement_1))), y = (max(range(measurement_2)) - 0.75 * max(range(measurement_2))), 
                                      hjust = 1, vjust = 1, # add correlation coefficient to top right
                                      label = paste0("Correlation = ", round(cor(df_random$measurement_1, df_random$measurement_2), 2))) +
                             scale_x_continuous(limits = c(0, max(range(measurement_1)))) + # set x-axis limits
                             scale_y_continuous(limits = c(0, max(range(measurement_2))))
                           p_random
                           
#####                           
                           
                                                               
dat_at_point_5  <- rnorm_multi(n = 10, 
                            mu = c(15, 15),
                            sd = c(5, 5),
                            r = c(0.5), 
                            varnames = c("measurement_1", "measurement_2"),
                            empirical = FALSE)

df_at_point_5 <- data.frame(measurement_1 = dat_at_point_5$measurement_1, measurement_2 = dat_at_point_5$measurement_2, patient = paste0("patient_", 1:n))

p_at_point_5 <- ggplot(df_at_point_5, aes(x = measurement_1, y = measurement_2, label = patient)) +
  geom_point(shape = 1, color = "black") +
  xlab("Measurement Occasion 1") +
  ylab("Measurement Occasion 2") +
  ggtitle("Medium agreement/consistency") +
  geom_text_repel(point.padding = 1, size = 3, nudge_x = 0.5, nudge_y = 0.5) + # add labels with more jitter
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey70") + # add diagonal line
  geom_smooth(method='lm', linetype = "dashed", se = F) +
  annotate("text", x = (max(range(measurement_1)) - 0.25 * max(range(measurement_1))), y = (max(range(measurement_2)) - 0.75 * max(range(measurement_2))), 
           hjust = 1, vjust = 1, # add correlation coefficient to top right
           label = paste0("Correlation = ", round(cor(df_at_point_5$measurement_1, df_at_point_5$measurement_2), 2))) +
  scale_x_continuous(limits = c(0, max(range(measurement_1)))) + # set x-axis limits
  scale_y_continuous(limits = c(0, max(range(measurement_2))))

p_at_point_5

(p_perfect + p_perfect_shift_5) / (p_at_point_5 + p_random )
