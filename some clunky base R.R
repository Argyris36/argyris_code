#compare three group by functions: a base one, my own loop solution, and the tidyverse one
df <- data.frame(
  grp = sample(2, 100000, replace = TRUE),
  a = rnorm(100000),
  b = rnorm(100000),
  c = rnorm(100000),
  d = rnorm(100000)
)
df

# standard base solution
st_time_std_base <- Sys.time()

agg <-lst ()
the_names <- colnames(df[, 2:ncol(df)])
for(i in 1: ncol(df-1)){
agg[[i]] <- aggregate(as.formula(paste(the_names[i], paste("grp"), sep = "~")), 
                      df, function(x) c( n = length(x), mean = mean(x), sd = sd(x), min = min(x), max = max(x)))

}
agg

end_time_std_base <- Sys.time()

time.taken_std_base <- round(end_time_std_base - st_time_std_base,2)

# my solution
st_time_my <- Sys.time()

tst <- lst () 
lst_dfs <- list()
multiple.func <- function(x) {
  c( mean = mean(x), sd = sd(x), min = min(x), max = max(x))
}
for(i in 1:length(unique(df$grp))){
  
lst_dfs[[i]] <-   df[which(df$grp==i),,drop= FALSE ]
  
tst[[i]]<-  c(
              i,
              nrow((lst_dfs[[i]])), 
              sapply(lst_dfs[[i]][,2:ncol(df)], multiple.func )
          
            
)

  names(tst[[i]])<- c("grp", "n",paste0(rep(c("mean_", "sd_", "min_", "max_"), 4), 
                                                   rep(colnames(df[2:ncol(df)]), each = 4) ))
}
tst


df_tst <- cbind(data.frame(tst[[1]]), data.frame(tst[[2]]))

end_time_my <- Sys.time()

time.taken_my <- round(end_time_my - st_time_my,2)
# tidyverse solution

start_time_tidy <- Sys.time()

df %>% 
  group_by(grp) %>% 
  summarize(
    n = n(),
    across(a:d, list(avg = mean, sd =sd, min = min, max = max))
  )

end_time_tidy <- Sys.time()

time.taken_tidy <- round(end_time_tidy- start_time_tidy,2)


time.taken_std_base
time.taken_my
time.taken_tidy


#### a problem of plotting

# let there be a dataframe with several variables, many  rows per person and n people


n_subj <- 10
n_trials <- 30

# I use faux to create coreelated variables and transform dataset to long to plot


# here I create a df per person which I replicate over the n_subjects
library(faux) 
simulated_data <- replicate(n_subj, rnorm_multi(n = 30,  # trials
                                       mu = c(0, 1, 2), # let all the means be the same
                                       sd = c(1, 2, 3), # let all sds be the same
                                       r = c(0.5, 0.2, 0.1), # these are the correlations
                                       #A with each B, C, D; B with C, D, C with D
                                       
                                       
                                       varnames = c("var_a", "var_b", "var_c"),# just giving it names
                                       empirical = FALSE))

# in this loop I turn the above into a dataframe
simulated_df_all_subjects <- data.frame()
for(i in 1: n_subj){
  simulated_df_all_subjects<- rbind(simulated_df_all_subjects, data.frame(simulated_data[,i]))
}
dim(simulated_df_all_subjects)

simulated_df_all_subjects$id <- rep(1:10, each =30)
simulated_df_all_subjects$trials <- rep(1:30, 10)
dim(simulated_df_all_subjects)


head(simulated_df_all_subjects, 40)


# here I get correlations across each subject
correlations_a_b <- 0
for(i in 1: length(unique(simulated_df_all_subjects$id))){
  
  correlations_a_b[i] <-  cor(simulated_df_all_subjects[simulated_df_all_subjects$id == i, ]$var_a, simulated_df_all_subjects[simulated_df_all_subjects$id == i, ]$var_b) 
  
}
correlations_a_b
mean(correlations_a_b )

# here are the correlations depicted
library(tidyverse)
simulated_df_all_subjects %>% 
  ggplot(aes(x = var_a, y = var_b)) +
  geom_point() +
  facet_wrap(~id, ncol = 2)


# or in base R 
par(mfrow=c(2,5))

ids <- unique(simulated_df_all_subjects$id)

for(i in 1: length(ids)){
  
plot(simulated_df_all_subjects[simulated_df_all_subjects$id==i, ]$var_a, 
     
     simulated_df_all_subjects[simulated_df_all_subjects$id==i,]$var_b , 
     
  main = paste("id = ", ids[i]), adj = 0,  col.main="black", xlim = c(-3,3.5),xlab = "var_a", ylab= "var_b",  ylim = c(-7,8) , 
  
  col = "#2E9FDF", frame = "FALSE",
  
  abline(lm(simulated_df_all_subjects[simulated_df_all_subjects$id==i, ]$var_a~ 
            simulated_df_all_subjects[simulated_df_all_subjects$id==i,]$var_b ), col="red"))
  
  text(x=0, y=-5.5, labels = paste("r =",  round(cor(simulated_df_all_subjects[simulated_df_all_subjects$id == i, ]$var_a, simulated_df_all_subjects[simulated_df_all_subjects$id == i, ]$var_b),2)))

  mtext("correlations between the two variables per individual", side = 3, line = - 1.5, outer = TRUE)
  
  }

# now let's reshape the dataframe to long 
simulated_df_all_subjects_long <- simulated_df_all_subjects %>% 
  pivot_longer(!c(id, trials), names_to = "var_type", values_to = "values")


simulated_df_all_subjects_long %>% 
  ggplot(aes(x = trials, y = values))+
  geom_point()+
  facet_wrap(~var_type, ncol = 1)

# using base

par(mfrow=c(3,1))
#pdf("rplot.pdf") 
variables <- unique(simulated_df_all_subjects_long$var_type)
for(i in 1: length(variables)){
  
 plot( simulated_df_all_subjects_long[variables=i]$trials, simulated_df_all_subjects_long[variables=i]$values,
       main = variables[i],
       xlab = "trials", ylab = "value", ylim = c(-8,11), col = "#2E9FDF", frame = "FALSE"
       )
  
}
#dev.off()

# if you need to correlate variables using the long dataset, it is very simple
cor(simulated_df_all_subjects_long[simulated_df_all_subjects_long$var_type == "var_a", ]$values, 
    simulated_df_all_subjects_long[simulated_df_all_subjects_long$var_type == "var_b", ]$values)


# and it is also simple if you want to do this by subject
cors_by_id <- 0
for (i in 1: length(unique(simulated_df_all_subjects$id))){
  
cors_by_id[i] <-  cor(simulated_df_all_subjects_long[simulated_df_all_subjects_long$var_type == "var_a" & 
                                       simulated_df_all_subjects_long$id == i  , ]$values, 
      simulated_df_all_subjects_long[simulated_df_all_subjects_long$var_type == "var_b" &
                                       simulated_df_all_subjects_long$id == i , ]$values)
  
}

cors_by_id
