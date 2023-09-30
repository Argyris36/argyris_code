library(openxlsx)

exercise_df <- read_excel("~/Downloads/exercise_df.xlsx")

# exercise_df <- data.frame(mon_sets = 5, mon_reps = 5, mon_weight = 130,
#                           
#                           wed_sets = NA, wed_reps = NA, wed_weight = NA,
#                           
#                           fri_sets = 1, fri_reps = 5, fri_weight = 140,
#                           
#                           start_date = as.Date("2023-03-10"),
#                           
#                           n_week = NA,
#                           
#                           mon_sets_ought = 5, mon_reps_ought = 5, mon_weight_ought = NA,
#                           
#                           wed_sets_ought = NA, wed_reps_ought = NA, wed_weight_ought = NA,
#                           
#                           fri_sets_ought = 1, fri_reps_ought = 5, fri_weight_ought = 140 ,
#                           
#                           Notes = NA
#                           
# )
# 
# exercise_df
# 
# 



if  ((exercise_df$fri_reps[length(exercise_df$fri_reps)]  == exercise_df$fri_reps_ought[length(exercise_df$fri_reps_ought)] &
      
      exercise_df$fri_sets[length(exercise_df$fri_sets)]  == exercise_df$fri_sets_ought[length(exercise_df$fri_sets_ought)] &
      
      exercise_df$fri_weight[length(exercise_df$fri_weight)]   == exercise_df$fri_weight_ought[length(exercise_df$fri_weight_ought)])  == TRUE){
  
  
  
  fri_weight_ought <- c(exercise_df$fri_weight_ought, (exercise_df$fri_weight[length(exercise_df$fri_weight)]+2.5))
  
  mon_weight_ought <- c(exercise_df$mon_weight_ought, (exercise_df$mon_weight[length(exercise_df$mon_weight)]+2.5))
  
  

  
  #print("hello")
  
  
  
} else {
  
  
  
  fri_weight_ought <- c(exercise_df$fri_weight_ought, (exercise_df$fri_weight[length(exercise_df$fri_weight)]+2.5))
  
  mon_weight_ought <- c(exercise_df$mon_weight_ought, (exercise_df$mon_weight[length(exercise_df$mon_weight)]+2.5))
  
  
  
}


exercise_df[nrow(exercise_df)+1, ] <-NA
exercise_df$fri_weight_ought <- fri_weight_ought
exercise_df$mon_weight_ought <- mon_weight_ought

exercise_df



write.xlsx(exercise_df,'/Users/stringarisa/Downloads/exercise_df.xlsx',colNames = TRUE, replace = TRUE)



