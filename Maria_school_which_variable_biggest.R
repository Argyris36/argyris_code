# Solution to One of Maria's school problems with code
# two numbers p and q such that 0<p<q<1
# which is the largest of these expressions?
# I am demonstrating this by using an range of examples. 
 

q <- seq(0.1 , 0.9, by = 0.1) 
p = q-.05
p

A = q-p
B = p-q
C = (p+q)/2
D = p/q
E = q/p

df_ranks <- data.frame(A = A, B= B, C= C, D= D, E =E)
largest <- colnames(df_ranks)
largest <- largest[apply(df_ranks,1, which.max)]

df_ranks <- cbind(df_ranks, largest)
df_ranks
###############

test <- data.frame(combn(1:9, 3))
apply(test, 2, prod)
test <- rbind(test, apply(test, 2, prod))
test_expanded <- data.frame(
cbind(
row_1 = test[,test[4, ] == 18],
row_2 = test[,test[4, ] == 105],
row_3 = test[,test[4, ] == 192],
column_1 = test[,test[4, ] == 56],
column_2 = test[,test[4, ] == 180],
column_3 = test[,test[4, ] == 36]
)
)



new_lst <- list()
  for (i in 1:ncol(test_expanded)){

new_lst[[i]] <- data.frame(permn(test_expanded[1:3, i]))
}

#names(new_lst) <- paste("prod_eq_",test_expanded[4,] )
names(new_lst) <- c("first_18", "second_18", "only_105", "only_192", "first_56", "first_56", "only_180", "first_36", "second_36")

test_it<-0
test_it<-matrix(NA, nrow= 6, ncol = 6)
for(i in 1:6){
  for(j in 1:6){

test_it[i, j] <- intersect(new_lst$first_18[3,i], new_lst$second_36[1,j])

  }
}
test_it


for(i in 1:9){
  for(j in 1:9){
  
  test_it[i] <- which(new_lst$first_18[3,i] == new_lst$second_36[1,j])
 
} 
}
test_it



