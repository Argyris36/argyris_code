library(segmented)

n = 10
set.seed(1974)

# some clunky vectors to simulate the non-linearity
x =  1:(2*n)
y <- 0 + 0.5*x
y_2 <- 0 + 1.2*(n+1):(2*n)

# put in a dataframe
df_pwise_reg <- data.frame(
  y = c(y,y_2), 
  x = 1:length(c(y,y_2)
  ))

# check the plot
df_pwise_reg %>% 
  ggplot(aes(x, y))+
  geom_point()

# now create a variable that gives you the points around the  kink you see.
poss_kinks <- df_pwise_reg$x[df_pwise_reg$x>15 & df_pwise_reg$x<25] 

#run regression models that 
lm_pw <- list()
aics <- 0
for(i in 1: length(poss_kinks)){
 
lm_pw[[i]] <- lm(y ~ x*(x < poss_kinks[i]) + x*(x>=poss_kinks[i]))
aics[i] <- AIC(lm_pw[[i]])

}
poss_kinks[which(aics==min(aics))]

plot(poss_kinks, aics) # visualise it

lm(y ~ x*(x < 22) + x*(x>=22))

simple_model <- lm(y ~ x, df_pwise_reg)

segmented(simple_model, seg.Z=~x)



lm_pw[[1]]
  
 