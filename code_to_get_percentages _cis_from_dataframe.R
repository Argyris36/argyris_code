#### This is some code to estimate proportions with CIs from a dataframe using loops
library(tidyverse)
df_test <- data.frame(var_a = c(rep(0,20), rep(1, 10), rep(NA, 5)), var_b = c(rep(0,10), rep(1, 20), rep(NA, 5))) 
df_test # I just created something that emulates your dataframe, e.g. the "test_df" we created together. 

var_names<- colnames(df_test) # this just creates the varnames, it pinches them off the column names of your df


list_dfs <-list () # this is the empty list that you will need


# now follows the simple loop 
for(i in 1: length(df_test)){
  list_dfs[[i]] <-df_test %>% 
  drop_na() %>% 
  count(eval(parse(text=var_names[i]))) %>%   # the gibberish  "eval(parseetc) here is simply your way of telling the computer
                                                  # to not treat this as simply a name, but as a variable. To see the difference use it with and without the gibberish
  mutate(perc = (n / sum(n, na.rm = TRUE))) # this gives you the percentage
}
list_dfs       # this is the output. 

# and now add the confidence intervals using the prop.test command below. I have estimated the 
# CIs for the "1" and therefore the "0" have been assigned NAs, if that makes sense. 
for(i in 1: length(list_dfs)){
list_dfs[[i]]$lower_ci[2] <- prop.test(list_dfs[[i]]$n[2], sum(list_dfs[[i]]$n))$conf.int[[1]]
list_dfs[[i]]$upper_ci[2] <- prop.test(list_dfs[[i]]$n[2], sum(list_dfs[[i]]$n))$conf.int[[2]]
}

list_dfs