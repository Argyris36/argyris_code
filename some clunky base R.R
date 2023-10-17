df <- data.frame(
  grp = sample(2, 1000000, replace = TRUE),
  a = rnorm(1000000),
  b = rnorm(1000000),
  c = rnorm(1000000),
  d = rnorm(1000000)
)
df




# my solution -------------------------------------------------------------

st_time_my <- Sys.time()
# write a quick function that does whatever you want to summarise
multiple.func <- function(x) {
  c( mean = mean(x), sd = sd(x), min = min(x), max = max(x))
  
}

#two empty vectors
test1 <- list()
test2 <- list()
#correlations_by_grp <- list()
#the loop
for(i in 1:length(unique(df$grp))){
  
  test1[[i]] <-  df[df[1]==i, ]
  
  test2[[i]]<- sapply(test1[[i]][,2:ncol(df)], multiple.func)
  
  #correlations_by_grp[[i]] <- cor(test1[[i]][,2:ncol(df)])
  
}

test2
#correlations_by_grp

end_time_my <- Sys.time()

time.taken_my <- round(end_time_my - st_time_my,2)



# correlation amongst all





# the tidyverse solution --------------------------------------------------
start_time_tidy <- Sys.time()
library(tidyverse)
df %>% 
  group_by(grp) %>% 
  summarize(
    n = n(),
    across(a:d, list(avg = mean, sd =sd, min = min, max = max))
  )

end_time_tidy <- Sys.time()

time.taken_tidy <- round(end_time_tidy- start_time_tidy,2)








# The tapply solution -----------------------------------------------------
st_time_tapply <- Sys.time()

grouped_results <- list()
for (i in 1: ncol(df) ){
 
grouped_results[[i]] <-  with (df, tapply(df[,i], list(grp), FUN=multiple.func))

}

names(grouped_results) <- colnames(df[1:ncol(df)])

st_end_tapply <- Sys.time()
time.taken_tapply <- round(st_end_tapply - st_time_tapply,2)


####another tapply solution!!!!!
beginning <- Sys.time()
lets_see <- lapply(df, function(x) with (df, tapply(x, list(grp), FUN=multiple.func))) # a fantastic one liner
ending <- Sys.time()
ending - beginning

lets_see 

time.taken_my
time.taken_tidy
time.taken_tapply


###understand these
take_it_up <- list()
for(i in 1:5){
  
  take_it_up [[i]] <- data.frame(with(df, tapply(df[,i], grp, multiple.func)) )

}
reduce(full_join , take_it_up)



xx <- data.frame(group = rep(1:4, 100), a = rnorm(400) , b = rnorm(400) )
head(xx)


DataCov <- do.call( rbind, lapply( split(xx, xx$group),
                                   function(x) data.frame(group=x$group[1], mCov=cov(x$a, x$b)) ) )

DataCov
######

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
