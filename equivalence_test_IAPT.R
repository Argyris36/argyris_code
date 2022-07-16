####notes for equivalence test analysis#####


install.packages("TOSTER")
library("TOSTER")



# for PHQ

dif_mean_young_phq <- 4.02 
dif_sd_young_phq <- 6.90
dif_mean_old_phq = 4.15 
dif_sd_old_phq = 6.94
n_young = 2177
n_old = 11273

# first get difference for effect size = 0.15
es <- (diff_old_young_phq)/ sqrt((dif_sd_young_phq^2+ dif_sd_old_phq^2) / 2)
diff_old_young_phq <- es*sqrt((dif_sd_young_phq^2+ dif_sd_old_phq^2) / 2)
diff_old_young_phq <- 0.15*sqrt((dif_sd_young_phq^2+ dif_sd_old_phq^2) / 2)
diff_old_young_phq

# then calculate by hand
t_lower_phq <- (dif_mean_young_phq - dif_mean_old_phq - (-diff_old_young_phq))/sqrt(( dif_sd_young_phq^2 /n_young) + (dif_sd_old^2 /n_old))
t_lower_phq

t_upper_phq <- (dif_mean_young_phq - dif_mean_old_phq - diff_old_young_phq )/sqrt(( dif_sd_young_phq^2 /n_young) + (dif_sd_old^2 /n_old))
t_upper_phq

  
# then estimate with TOSTER
tsum_TOST(m1=dif_mean_young, m2=dif_mean_old, sd1=dif_sd_young, sd2=dif_sd_old, n1= n_young, n2=n_old, low_eqbound = -0.15, high_eqboun=0.15, eqbound_type = "SMD", alpha = 0.05, var.equal = TRUE)



# for GAD
dif_mean_young_gad <- 3.70 
dif_sd_young_gad <- 6.24
dif_mean_old_gad = 3.79 
dif_sd_old_gad = 6.39
n_young = 2177
n_old = 11273

es <- (diff_old_young_gad)/ sqrt((dif_sd_young_gad^2+ dif_sd_old_gad^2) / 2)
diff_old_young_gad <- es*sqrt((dif_sd_young_gad^2+ dif_sd_old_gad^2) / 2)
diff_old_young_gad <- 0.15*sqrt((dif_sd_young_gad^2+ dif_sd_old_gad^2) / 2)
diff_old_young_gad

# then calculate by hand
t_lower_gad <- (dif_mean_young_gad - dif_mean_old_gad - (-diff_old_young_gad))/sqrt(( dif_sd_young_gad^2 /n_young) + (dif_sd_old^2 /n_old))
t_lower_gad

t_upper_gad <- (dif_mean_young_gad - dif_mean_old_gad - diff_old_young_gad )/sqrt(( dif_sd_young_gad^2 /n_young) + (dif_sd_old^2 /n_old))
t_upper_gad


# then estimate with TOSTER
tsum_TOST(m1=dif_mean_young_gad, m2=dif_mean_old_gad, sd1=dif_sd_young_gad, sd2=dif_sd_old_gad, n1= n_young, n2=n_old, low_eqbound = -0.15, high_eqboun=0.15, eqbound_type = "SMD", alpha = 0.05, var.equal = TRUE)



library(TOSTER)
powerTOSTtwo(alpha = 0.05,
             statistical_power = 0.90,
             low_eqbound_d = -0.15,
             high_eqbound_d = 0.15)

n_sim = 1000
dif_mean_young = 4.02 
dif_sd_young = 6.90
sim_difs_young <- rnorm(n_sim, dif_mean_young, dif_sd_young)

dif_mean_old = 4.15 
dif_sd_old = 6.94
sim_difs_old <- rnorm(n_sim, dif_mean_old, dif_sd_old)


mean(sim_difs_young - sim_difs_old)
sd(sim_difs_young - sim_difs_old)
sum(((sim_difs_young - sim_difs_old)/sd(sim_difs_young-sim_difs_old))>=0.1)/length(sim_difs_old)





library(negligible)
neg.twoindmeans(
  v1 = population_young,
  v2 = population_old,
  dv = NULL,
  iv = NULL,
  eil = -0.69,
  eiu = 0.69,
  varequiv = FALSE,
  normality = TRUE,
  tr = 0.2,
  nboot = 500,
  alpha = 0.05,
  plot = TRUE,
  saveplot = FALSE,
  data = NULL
)


population_young <- rnorm(n = 2177 , dif_mean_young, dif_sd_young)
population_old <- rnorm(n = 11273 , dif_mean_old, dif_sd_old)
df_young_vs_old <- data.frame(cbind(population_young, population_old))

