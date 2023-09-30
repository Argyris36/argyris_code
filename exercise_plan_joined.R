


weight_names <- paste0(c("_squat_", "_bench" , "_dl"), "test_", "weight")

# Enter test date
training_date <-seq(from = as.Date("2023-03-03"), to = as.Date("2023-03-03"), by = 'day')

# Enter weights and reps of your test day
# 1. weights in squat, bench, dl seq
# 2. reps in squat, bench, dl seq
test_weights <- list(c(140, 100, 180))
test_reps <- list(c(5,5,2))


# one_rm_weight_reps <- list(c(squat_test_weight,squat_test_reps), c(bench_test_weight, bench_test_reps), c(dl_test_weight, dl_test_reps))
# names(one_rm_weight_reps) <- one_rm_names


#Eppley formula for 1 rep max
eppley_one_rep_max_formula <- function (weight, reps){
  one_rms <- weight*(1 + reps/30)
  return(one_rms)
}

one_rm_names <- paste0("est_one_rep_max_", c("_squat", "_bench" , "_dl"))


df_one_rep_max  <- matrix(NA, nrow = length(training_date), ncol =  length(one_rm_names) )
for(i in 1: length(one_rm_names)){
  
  df_one_rep_max[[1, i]]  <- round(eppley_one_rep_max_formula(weight = 
                                                          test_weights[[length(test_weights)]][i] , 
                                                          reps = test_reps[[length(test_reps)]][i]))
  
  colnames(df_one_rep_max) <- one_rm_names
  df_one_rep_max <- data.frame(df_one_rep_max)

  
   
}


df_one_rep_max$date <- training_date[[1]]
df_one_rep_max 



test_df <- data.frame(squat = c(140, 135), deadlift = c(150, 155), bench = c(100, 102.5), date = seq(from = as.Date("2023-03-03"), to = as.Date("2023-03-03"), by = 'day'))
right_join(test_df, df_one_rep_max, by = "date")

test_df_1 <- data.frame(squat = c(140, 135), deadlift = c(150, 155), bench = c(100, 102.5), date = seq(from = as.Date("2023-03-04"), to = as.Date("2023-03-04"), by = 'day'))

rbind(test_df, test_df_1)
# library(openxlsx)
# 
# exercise_df <- read_excel("~/Downloads/exercise_df.xlsx")
# 
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
# # I then created squat variables.
# colnames(exercise_df) <- str_replace(colnames(exercise_df), "mon", "squat_mon")
# colnames(exercise_df) <- str_replace(colnames(exercise_df), "wed", "squat_wed")
# colnames(exercise_df) <- str_replace(colnames(exercise_df), "fri", "squat_fri")
# squat_exercise_df <- exercise_df
# squat_exercise_df  <- subset(squat_exercise_df, select = -c(start_date, n_week, Notes))
# squat_exercise_df 
# 
# bench_exercise_df <- squat_exercise_df 
# colnames(bench_exercise_df) <- str_replace(colnames(bench_exercise_df), "squat", "bench")
# bench_exercise_df 
# 
# dl_exercise_df <- squat_exercise_df 
# colnames(dl_exercise_df) <- str_replace(colnames(dl_exercise_df), "squat", "dl")
# dl_exercise_df 
# 
# press_exercise_df <- squat_exercise_df 
# colnames(press_exercise_df) <- str_replace(colnames(press_exercise_df), "squat", "press")
# press_exercise_df 
# 
# dbrow_exercise_df <- squat_exercise_df 
# colnames(dbrow_exercise_df) <- str_replace(colnames(dbrow_exercise_df), "squat", "dbrow")
# dbrow_exercise_df[,-which(str_detect(colnames(dbrow_exercise_df),"_ought"))]  # getting rid of "ought" for accessories 
#                            
# rdl_exercise_df <- squat_exercise_df 
# colnames(rdl_exercise_df) <- str_replace(colnames(rdl_exercise_df), "squat", "rdl")
# rdl_exercise_df 
# rdl_exercise_df[,-which(str_detect(colnames(rdl_exercise_df),"_ought"))]
# 
# pflies_exercise_df <- squat_exercise_df 
# colnames(pflies_exercise_df) <- str_replace(colnames(pflies_exercise_df), "squat", "pflies")
# pflies_exercise_df 
# pflies_exercise_df[,-which(str_detect(colnames(pflies_exercise_df),"_ought"))]
# 
# 
# lat_exercise_df <- squat_exercise_df 
# colnames(lat_exercise_df) <- str_replace(colnames(lat_exercise_df), "squat", "lat")
# lat_exercise_df 
# lat_exercise_df[,-which(str_detect(colnames(lat_exercise_df),"_ought"))]
# 
# 
# exercise_df <- data.frame(cbind(squat_exercise_df, bench_exercise_df, dl_exercise_df, press_exercise_df, dbrow_exercise_df, rdl_exercise_df, 
#       pflies_exercise_df, lat_exercise_df))
# 
# 
# # now create an if statement for each of the lifts
# 
# if  ((exercise_df$fri_reps[length(exercise_df$fri_reps)]  == exercise_df$fri_reps_ought[length(exercise_df$fri_reps_ought)] &
#       
#       exercise_df$fri_sets[length(exercise_df$fri_sets)]  == exercise_df$fri_sets_ought[length(exercise_df$fri_sets_ought)] &
#       
#       exercise_df$fri_weight[length(exercise_df$fri_weight)]   == exercise_df$fri_weight_ought[length(exercise_df$fri_weight_ought)])  == TRUE){
#   
#   
#   
#   fri_weight_ought <- c(exercise_df$fri_weight_ought, (exercise_df$fri_weight[length(exercise_df$fri_weight)]+2.5))
#   
#   mon_weight_ought <- c(exercise_df$mon_weight_ought, (exercise_df$mon_weight[length(exercise_df$mon_weight)]+2.5))
#   
#   
#   
#   
#   #print("hello")
#   
#   
#   
# } else {
#   
#   
#   
#   fri_weight_ought <- c(exercise_df$fri_weight_ought, (exercise_df$fri_weight[length(exercise_df$fri_weight)]+2.5))
#   
#   mon_weight_ought <- c(exercise_df$mon_weight_ought, (exercise_df$mon_weight[length(exercise_df$300mon_weight)]+2.5))
#   
#   
#   
# }
# 
# 
# exercise_df[nrow(exercise_df)+1, ] <-NA
# exercise_df$fri_weight_ought <- fri_weight_ought
# exercise_df$mon_weight_ought <- mon_weight_ought
# 
# exercise_df
# 
# 
# 
# write.xlsx(exercise_df,'/Users/stringarisa/Downloads/exercise_df.xlsx',colNames = TRUE, replace = TRUE)
# 
# 

