
# -------------------------------------------------------------------------

# doing stats by groups in r ----------------------------------------------

# -------------------------------------------------------------------------




# example dataframe -------------------------------------------------------


df <- data.frame(
  grp = sample(2, 100, replace = TRUE),
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100),
  d = rnorm(100)
)
df




# create a function with lots of parameters -------------------------------

# write a quick function that does whatever you want to summarise
multiple.func <- function(x) {
  c( mean = mean(x), sd = sd(x), min = min(x), max = max(x))
  
}




# my original solution -------------------------------------------------------------
st_time_my <- Sys.time()

#two empty vectors
test1 <- list()
test2 <- list()
correlations_by_grp <- list() # in case you also wanted to do correlations by group
#the loop
for(i in 1:length(unique(df$grp))){
  
  test1[[i]] <-  df[df[1]==i, ]
  
  test2[[i]]<- data.frame(sapply(test1[[i]][,2:ncol(df)], multiple.func))
  
  correlations_by_grp[[i]] <- cor(test1[[i]][,2:ncol(df)]) # in case you also wanted to do correlations by group
  
}

test2 # this is a very elegant solution; only downside is that it would be cumbersome to do more than one by group
correlations_by_grp

end_time_my <- Sys.time()

time.taken_my <- round(end_time_my - st_time_my,2)

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

time.taken_tidy


# this is a good solution but not flexible in terms of functions that can be used.


# my first tapply solution -----------------------------------------------------
st_time_tapply <- Sys.time()

grouped_results <- list()
for (i in 1: ncol(df) ){
 
grouped_results[[i]] <-  data.frame(rep(paste0("grp_", colnames(df[,i, drop=F])),2),
                                        with (df, tapply(df[,i], list(grp), FUN=multiple.func)))

}
grouped_results
df_of_grouped_data <- do.call("rbind",grouped_results)


df_of_grouped_data 

names(grouped_results) <- colnames(df[1:ncol(df)])

st_end_tapply <- Sys.time()
time.taken_tapply <- round(st_end_tapply - st_time_tapply,2)
time.taken_tapply

df_for_tapply_output <- data.frame(with(df,tapply(a, list(grp), multiple.func)))

# comment: quite lengthy but works well and can handle more than one group



####most elegant tapply solution!!!!!
beginning <- Sys.time()
lets_see <- do.call("rbind",lapply(df[2:ncol(df)], function(x) data.frame(with (df, tapply(x, list(grp), FUN=multiple.func))))) # a fantastic one liner
lets_see

# this is by far the most elegant and most versatile solution
# only downside is that it comes out as a list.


ending <- Sys.time()
ending - beginning




# now try some solutions with two groups ----------------------------------



df_two_grp <- data.frame(
  grp_1 = rep(c("boy", "girl"), 50),
  grp_2 =  sample(c("dep", "non_dep"), 100, replace = TRUE),
  a = rnorm(100),
  b = rnorm(100),
  c = rnorm(100),
  d = rnorm(100)
)
head(df_two_grp)

#### using the elegant tapply solution above. Notice I have turned it into a dataframe
lets_see_2 <- do.call("rbind", lapply(df_two_grp[3:ncol(df_two_grp)], function(x) data.frame(with (df_two_grp, tapply(x, list(grp_1, grp_2), FUN=multiple.func)) )))# a fantastic one liner

# this does work well



# correlations by groups --------------------------------------------------


# correlation by two groups
do.call("rbind",lapply(split(df_two_grp, list(df_two_grp$grp_1, df_two_grp$grp_2)), function(x) data.frame(cor(df_two_grp$a, df_two_grp$b))))


###understand these
take_it_up <- list()
for(i in 1:5){
  
  take_it_up [[i]] <- data.frame(with(df, tapply(df[,i], grp, multiple.func)) )

}
reduce(full_join , take_it_up)
take_it_up 


xx <- data.frame(group = rep(1:4, 100), a = rnorm(400) , b = rnorm(400) )
head(xx)


DataCov <- do.call( rbind, lapply( split(xx, xx$group),
                                   function(x) data.frame(group=x$group[1], mCov=cov(x$a, x$b)) ) )

DataCov



lapply(split(df, df$grp), function(x) cor(df$a, df$b)) 

lm_test <- lapply(split(df, df$grp), function(x) lm(x$a~x$b))

lm_test$"1"[[1]][2]

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
