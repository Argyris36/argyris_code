as.formula(paste("y ~ x1 + x2", "x3", sep = "+"))
as.formula(paste("y ~ x1+" , "x*24", "x3", sep = "+"))
factors <- c("x2", "x3")
facs2 <- c("x2*x3", "x4", "x5")
as.formula(paste0("y~", paste(facs2, collapse="+")))

df_test <- data.frame(cbind(y = rnorm(100, 10,1), x1=rnorm(100,0,1), x2 = rnorm(100, 1,2), 
                                         x3 = rnorm(100, 2,3), x4 = rnorm(100, 3,4)))

                                         

pred_vars_1 <- c("(x1)", "(x2*x3)", "x4")
test_form_1 <- as.formula(paste("y~", paste(pred_vars_1, collapse = "+")))

test_1 <- lm(test_form_1, data = df_test)

test_1 <- eval(bquote(lm(.(test_form_1), data = df_test)))

test_1

pred_vars_2 <- c("(x1)", "x2", "x3", "x4")
test_form_2 <- as.formula(paste("y~", paste(pred_vars_2, collapse = "+")))

test_2 <- lm(test_form_2, data = df_test)

test_2 <- eval(bquote(lm(.(test_form_2), data = df_test)))

test_2



pred_vars_3 <- c("(x1)", "x2", "x4")
test_form_3 <- as.formula(paste("y~", paste(pred_vars_3, collapse = "+")))

test_3<- lm(test_form_3, data = df_test)

test_3 <- eval(bquote(lm(.(test_form_3), data = df_test)))

test_3



pred_vars_4 <- c("(x1)", "x2")
test_form_4 <- as.formula(paste("y~", paste(pred_vars_4, collapse = "+")))

test_4<- lm(test_form_4, data = df_test)

test_4 <- eval(bquote(lm(.(test_form_4), data = df_test)))

test_4



anova(test_1, test_2)

listing_it <- list()
name_it <- list()
listing_it[[1]] <- test_1
listing_it[[2]] <- test_2
listing_it[[3]] <- test_3
listing_it[[4]] <- test_4

deposit<- list()
for(i in 2:length(listing_it)){
  
  deposit [[i]] <- anova(listing_it[[i-1]], listing_it[[i]])
  
  name_it[i] <- paste(listing_it[[i-1]]$call, ";", listing_it[[i]]$call)
  
}



#to get the names.
paste(paste0(listing_it[[i]]$call[[2]], paste(" VS "), paste0( listing_it[[i-1]]$call[[2]]))[c(3,6)])

##########
# put each formula in lists
# loop over them with your glm formula. Put the results into list.
# use i 2: length(...) to run anovas between all models. 
# label them 






  