rolls<-function() #function
{
  x<-sample(1:6,1) #roll a die once
  i=1 #increase the counter
  while(x!=6) #until the roll is not a six
  {
    x<-sample(1:6,1) #continue rolling
    i=i+1 #and increasing the counter
  }
  return(i) #return the counter after the 6
}


roll_func <- function() {
x<- c(1, sample (1:6, 1))
i = 1
while ((x[i] != x[i+1]) & (x[i] != 6 ))
{
  x <- c(1, sample (1:6, 1))
  i = i+1
}
  return(i)
}


mean(replicate(1000, roll_func()))

mean(replicate(5,mean(replicate(10000,rolls()))))

mean(replicate(100,rolls()))

rolls()


rolls<-function() #function
{
  j=0 #counter to check if the current roll is 6
  x<-sample(1:6,1) #roll once
  i=1 #increase the counter
  if(x==6) #if the roll is a six
  {
    j=1 #set this counter to 1
    x<-sample(1:6,1) #roll again
    i=i+1 #increase the counter
    if(x!=6) j=0 #if the roll is not 6, set j back to 0
  }
  while(j!=1) #while j is 0
  {
    x<-sample(1:6,1) #roll once
    i=i+1 #increase the roll counter
    if(x==6) #if the roll is a six
    {
      j=1 #set this counter to 1
      x<-sample(1:6,1) #roll again
      i=i+1 #increase the counter
      if(x!=6) j=0 #if the roll is not 6, set j back to 0
    }
  }
  return(i) #return the roll counter
}

start_time <- Sys.time()
mean(replicate(10000,rolls()))
stop_time <- Sys.time()
  
stop_time -start_time



 

  
test_func <- function(){

  x <- replicate(1000, sample(1:6, 1))
  z <- seq(1:length(x))
  df_dice <- data.frame(x = x, z = z)


position <- 0
test <- NA
for (i in 1:(nrow(df_dice)-1)){

  
  if ((df_dice$x[i] == df_dice$x[i+1]) & (df_dice$x[i] == 6 )){

  test[i] = df_dice$z[i]}

}

position <- min(which(!is.na(test)))
position
  return(position)
  }


test_func()
start_time <- Sys.time() 
mean(replicate(1000, test_func()))
stop_time <- Sys.time()
stop_time -start_time
