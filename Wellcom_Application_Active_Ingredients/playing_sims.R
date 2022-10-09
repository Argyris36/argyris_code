generate_design <- function(n_participants, n_genres, n_songs, prop_instrument = .20, genre_names = c()){
  # we have a new genre_name argument here to assign factor levels dynamically.
  design_matrix <- expand.grid(participant = 1:n_participants, genre = 1:n_genres, song = 1:n_songs)
  design_matrix$song <- paste0(design_matrix$genre, "_", design_matrix$song) 
  # NOTE THAT I DELETED THE LINE THAT ASSIGNED ROCK POP HERE TO MAKE THE FUNCTION MORE FLEXIBLE
  instrument_players <- sample(c("yes", "no"), n_participants, prob = c(prop_instrument, (1-prop_instrument)), replace = T) # sample whether people play instrument
  for(i in 1:nrow(design_matrix)){ # grant people the ability to play instrument
    design_matrix$instrument[i] <- instrument_players[design_matrix$participant[i]]
    design_matrix$genre[i] <- genre_names[as.integer(design_matrix$genre[i])]
  }
  
  return(design_matrix)
}


set.seed(1)
data4 <- generate_design(n_participants = 10, n_genres = 3, n_songs = 20, prop_instrument = .20, genre_names = c("pop", "rock", "classic")) # we add the classic genre

# assign levels to to original variables
data4$genre <- factor(data4$genre, levels = c("pop", "rock", "classic"))
data4$instrument <- factor(data4$instrument, levels = c("yes", "no")) 

pp_song_mat <- expand.grid(song = seq(10, 100, by = 10), participant = seq(10, 500, by = 10)) # matrix containing all the different combinations of song number and participants

pp_song_mat <- data.frame(rbind(c(-999,-999), pp_song_mat)) # extra row for first iteration

pp_song_mat$power <- rep(-999) 
pp_song_mat$p_singular <- rep(-999) 
pp_song_mat$p_nonconv <- rep(-999) 
pp_song_mat$skips_at_n <- rep(-999) 
pp_song_mat$control_row <- rep(-999) 

n_sims <- 10 # we want 100 simulations here which will already be 50,000 models to fit if power does not reach .90 before that as we have 500 combinations of songs-number and participant number
p_vals <- c()
# power_at_n <- c(0) # this vector will contain the power for each sample-size (it needs the initial 0 for the while-loop to work)
n <- 1
power_crit <- .90
alpha <- .005 


b0 <- 75
b1 <- 0
b2 <- 2.5 
sd_u0 <- sqrt(11.25)
epsilon <- sqrt(56.25-11.25)
sd_u1 <- 5
corr_u01 <- -.20
sigma_u01 <- matrix(c(sd_u0^2, sd_u0*sd_u1*corr_u01, sd_u0*sd_u1*corr_u01, sd_u1^2), ncol = 2)

sd_w0 <- sqrt(11.25)/2
sd_w1 <- sd_w0/4
corr_w01 <- .10
sigma_w01 <- matrix(c(sd_w0^2, sd_w0*sd_w1*corr_w01, sd_u0*sd_w1*corr_u01, sd_w1^2), ncol = 2)



# some additional stuff we need
is_singular <- c() # this will be used to check how many models are singular fits
is_nonconv <- c() # this will be used to check how many models did not converge




start_time <- Sys.time()

#### increasing sample size ####
set.seed(987654321)
while(pp_song_mat$power[n] < power_crit){
  n <- n+1 # increase n for next iteration
  skips_at_n <- 0 # initialize counter for how many trials were skipped
  
  #### increasing simulation number ####
  for(sim in 1:n_sims){
    
    
    # make design-matrix
    tmp_dat <- generate_design(n_participants = pp_song_mat$participant[n], n_genres = 2, 
                               n_songs = pp_song_mat$song[n], prop_instrument = .20)
    tmp_dat$genre_pop <- ifelse(tmp_dat$genre == "pop", 1, -1)
    tmp_dat$instrument_yes <- ifelse(tmp_dat$instrument == "yes", 1, -1)
    
    #### making sure that there is at least 1 instrument player in the data (otherwise the model does cannot be fit) ####
    if (mean(tmp_dat$instrument_yes) != -1){
      
      unique_songs <- unique(tmp_dat$song)
      tmp_dat$genre <- factor(tmp_dat$genre)
      tmp_dat$instrument <- factor(tmp_dat$instrument, levels = c("yes", "no"))
      
      # make random effect matrices
      U01 <- mvrnorm(length(unique(tmp_dat$participant)),c(0,0),sigma_u01)
      W01 <- mvrnorm(length(unique(tmp_dat$song)),c(0,0),sigma_w01)
      
      #### create the DV ####  
      for(i in 1:nrow(tmp_dat)){
        tmp_dat$liking[i] <- rnorm(1, 
                                   b0+ # fixed intercept (average liking)
                                     U01[tmp_dat$participant[i], 1] + # random intercept for participants
                                     W01[which(unique_songs == tmp_dat$song[i]), 1] + # random intercept term for song
                                     (b1+ # fixed effect of genre (which is 0)
                                        U01[tmp_dat$participant[i], 2]) # random slope for genre across participants
                                   *tmp_dat$genre_pop[i] # for each row whether its pop or rock
                                   +(b2+ # fixed effect of instrument 
                                       W01[which(unique_songs == tmp_dat$song[i]), 2])
                                   *tmp_dat$instrument_yes[i]# random slope for instrument across songs
                                   , epsilon) # residual SD
        
      }
      
      #### fit model #### 
      tmp_lmem <- suppressMessages(mixed(liking ~ genre + instrument + (1 + genre | participant) + (1 + instrument | song), tmp_dat, method = "S", control = lmerControl(optimizer = "bobyqa"), progress = F))
      
      p_vals[sim] <- tmp_lmem$anova_table$`Pr(>F)`[2] # extract p-value for instrument
      
      
      #### check for model convergence and singular fits ####
      is_singular[sim] <- ifelse(max(grepl("singular", tmp_lmem[["full_model"]]@optinfo[["conv"]][["lme4"]][["messages"]], fixed = T)) > 0, 1, 0)
      is_nonconv[sim] <- ifelse(max(grepl("failed", tmp_lmem[["full_model"]]@optinfo[["conv"]][["lme4"]][["messages"]], fixed = T)) > 0, 1, 0)
      
    } else { # this happens if there is not a single instrument player in our sample
      
      p_vals[sim] <- 1 # set p-value to 1 manually
      
      skips_at_n <- skips_at_n+1 # update number of skipped simulations because of 0 instrument players
      # print("skipping sim because no single instrument player")
      
    }
    
    
  } # end of single-n simulation for-loop
  
  #### calculate power and print a message to see our progress    
  pp_song_mat$power[n] <- mean(p_vals < alpha)
  pp_song_mat$p_singular[n] <- mean(na.omit(is_singular)) # save proportion of singular models to pp_song_mat
  pp_song_mat$p_nonconv[n] <- mean(na.omit(is_nonconv)) # save proportion of non-converging models to pp_song_mat
  pp_song_mat$skips_at_n[n] <- skips_at_n # add this number to pp_song_mat so we can have a look at it later
  
  print("################################################")
  print(paste0("current sample-size: ", pp_song_mat$participant[n], " with ", pp_song_mat$song[n], " songs"))
  print(paste0("current power: ", mean(pp_song_mat$power[n])))
  print(Sys.time() - start_time)
}
  
  
  
  