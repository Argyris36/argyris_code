p1 = .44 # proportion getting better on drug
p2 = .25 # proportion getting better on placebo
n1 = 90 - 17 # number of people on drug
n2 = 90 - 29 # number of people on placebo

effect_sizes <- list(.05, .10, .15, p1-p2, .25, .30) # a list of possible effect sizes ranging from 
                                                      # 5% and moving through the observed difference (p1-p2)
                                                      # up to an unlikely 30%

se <- sqrt((p1*(1-p1))/n1 + (p2*(1-p2))/n2) # a standard formula to obtain the standard error of 
                                            # differences in proportions

library(retrodesign) 
retro_design(effect_sizes, alpha = 0.01, se) # power,type M and type S errors for any given effect 
                                              # size


