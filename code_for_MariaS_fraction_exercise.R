### this solves an exercise for Maria's school
# the teacher asked her to consider the following numbers 3,4,5,6 and find all combinations
# that satisfy fraction_1 > fraction_2
# this is a tough problem that requires some good combinatorics
# or some computer code.
# I am doing both below.
vec_a<- c(3,4,5,6) # the numbers
df_fractions <- data.frame(expand.grid(vec_a, vec_a)) # the dataframe with the possible combinations
# math explanation: all pairwise comparisons of two lists, sucha s vec_a with itself
# are given by n*(n-1)//2 (which is the Bernoulli/binomial for pairs); however, here we must also consider a) the fact that each number
# with itself can build a fraction
# each pair can build the reverse fraction, e.g. 3/4 as well as 4/3
# therefore, we have (4*(4-1)/2) + the four items with themselves + the 6 reverse ones = 16

# now we create the column were the divided fractions appear as decimals (R coerces it this way)
df_fractions$fractions <- df_fractions$Var1/df_fractions$Var2 

# and for illustration, this loop also recreates the possible fractions
for(i in 1:nrow (df_fractions )){
  
  df_fractions$fraction[i] <-  paste0(df_fractions$Var1[i],"/", df_fractions$Var2[i])
  
}

# the code below gives the possible combinations with the fractions. 
# here we use the Bernoulli (binomial) formula or the equivalent n*(n-1)/2 
# because we don't want comparisons with self (the question is who wins)
# or duplicate comparisons, e.g. having both comparison: 3/4 vs 1/2 and 1/2 vs 3/4

df_all_comparisons <- as.data.frame(t(combn(df_fractions$fractions,2))) # the decimals
df_all_comparisons <- data.frame(df_all_comparisons, as.data.frame(t(combn(df_fractions$fraction,2)))) # the fractions
colnames(df_all_comparisons) <- c("numbers_1", "numbers_2", "fraction_1", "fraction_2")

# this now contains all the comparisons
df_all_comparisons

# but also has some duplicates because we have the comparison of wholes, e.g. the 1 that occurs 
# due to 3/3 with that due to 4/4.
# I will remove these too.

final_comparisons <- df_all_comparisons %>% 
  filter(!(numbers_1 == numbers_2))

# to also give the winning fraction, I created the following column
for(i in 1:nrow(final_comparisons))
if((final_comparisons$numbers_1[i]>final_comparisons$numbers_2[i])  == TRUE){
  final_comparisons$winner[i] <- final_comparisons$fraction_1[i]
} else { 
  final_comparisons$winner[i] <- final_comparisons$fraction_2[i]
}
  
  
paste("The number of comparisons are", nrow(final_comparisons))

