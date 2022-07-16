#### WP3 Power Calculations ####
library(ggplot2)
library(tidyr)
library(pwr)
library(patchwork)

# part I: demonstrate power for: Q1a: to differentiate between a random sequence and the true ADEPT rankings (= ranking 
# of items based on their model utilities);
# Q1b: to rank correctly as 1,2,3 the first, middle and last of their statements in their ADEPT ranking;
# Q3: to have on average a 90% rate of questions/items responded to by adolescets in the ADEPT

# All three problems can be approached with the same binomial power simulation approach. 

prob_correct <- 0.9 # expected probability

prob_random <- 0.5 # chance

binomial_power <- 0
binomial_sample_sizes <- 1:1000
for (i in seq(binomial_sample_sizes)){
binomial_power[i] <- pwr.p.test(ES.h(prob_random, prob_correct ),n = i ,alternative = "less",sig.level = 0.01)[[4]]
}
binomial_power_by_sample <- cbind(binomial_sample_sizes[1:100], binomial_power[1:100])

binomial_power_by_sample <- data.frame(binomial_power_by_sample)

colnames(binomial_power_by_sample) <- c("sample_size", "power")
                                  
                                  
                                  
binomial_power_by_sample_plot <- ggplot () + 
  
  geom_point(data = binomial_power_by_sample, aes( x = sample_size, y = power)) +
  xlab('Sample Size') +
  ylab('Power')+
  ggtitle("Power over sample Size") 

binomial_power_by_sample_plot <- binomial_power_by_sample_plot + geom_segment(aes(x=binomial_power_by_sample$sample_size[1],
                                                 
                                                 xend=tail(binomial_power_by_sample$sample_size,1),y= 0.9, yend = 0.9, colour = "red", 
                                                 linetype = "dashed"))



binomial_power_by_sample_plot <- binomial_power_by_sample_plot + theme_classic() + theme(legend.position="none")

binomial_power_by_sample_plot 


# part II: demonstrate confidence intervals around .9 estimate of success for recovery of personalised statements 
# using each of the two methods. This seems a clearer way of approaching this as it does not have to rely on our
# expectation of a base rate, i.e. whether the alternative is random. 

successes <- 9*c(1, 5, 10, 50, 100) # to create the numerator of the number of successes/trial

trials <- 10*c(1, 5, 10, 50, 100) # denominator of the number of successes/trial

confidence_intervals <-matrix(NA, length(successes), 2)

for (i in 1:(length(successes))){

confidence_intervals[i,] <- binom.test(successes[i],trials[i], conf.level = 0.99)$conf.int[1:2]

confidence_intervals

}
confidence_intervals <- cbind(trials, confidence_intervals)
confidence_intervals <- data.frame(confidence_intervals)

confidence_interval_plot <- ggplot() + 
  geom_line(data = confidence_intervals, aes(x = trials, y = confidence_intervals[,2])) +
  geom_line(data = confidence_intervals, aes(x = trials, y = confidence_intervals[,3])) +
  xlab('Sample Size') +
  ylab('Correct guess with 99% CIs')+
  c +
  scale_x_continuous(breaks = seq(0, length(binomial_sample_sizes), 100)) +
  ylim(.4, 1) 

confidence_interval_plot <- confidence_interval_plot + geom_segment(aes(x=trials[1],
                                                  
                                                        xend=tail(trials,1),y= 0.9, yend = 0.9, colour = "red"
                                                        ))
#confidence_interval_plot <- confidence_interval_plot + theme(legend.position="none")


# add the shaded area
confidence_interval_plot <- confidence_interval_plot + theme_classic() + theme(legend.position="none") +

geom_ribbon(data = confidence_intervals, aes(x = trials, y = 0.9 , ymin = confidence_intervals[,2], 
                                             ymax = confidence_intervals[,3], xmin = 10, xmax = 1000), 
                                              fill = "grey70", alpha = 0.3) 
                                              





binomial_power_by_sample_plot + inset_element(confidence_interval_plot, left = 0.3, bottom = 0.1, right = 1, top = .8) 




