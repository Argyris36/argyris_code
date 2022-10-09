test_df <- data.frame(a = rnorm(10,0,1), b = rnorm(10, 1, 2), c = c(rep("male", 5), rep("female",5)), 
                      d = rep(c("yes", "no"),5) )
test_df
                      

class(test_df$c)


turn_to_factor <- function(df, var1, var2){  
  if (is.null(df[[var1]])==TRUE)  {
    print("nothing to do for var1")
  } else {
    if(is.factor(paste0(df,"$",var1))==FALSE){ 
      df[[var1]] = factor(df[[var1]])
    } else{
      df[[var1]]
    }  
  }
  if (is.null(df[[var2]])==TRUE) {
    print("nothing to do for var2")
  } else {
    if(is.factor(df[[var2]])==FALSE){ 
      df[[var2]] = factor(df[[var2]])
    } else{
      df[[var2]]
    }  
  }
  return(df)
}

result_2 <- turn_to_factor(df = test_df , var1 = "c", var2 = "d")

class(c(result_2$c, result_2$d))

