library(deSolve)
library(ggplot2)

## Parameters
r <- 0.1  # growth rate of the affected population
K <- 0.6  # Carrying capacity fraction (of the total population)
alpha <- 0.8  # Base diagnosis rate (constant)
Se <- 0.85  # Sensitivity of the diagnostic procedures
Sp <- 0.75  # Specificity of the diagnostic procedures
N <- 1000000  # Total population
sigma <- 1  # constant (constant number of individuals that are "exposed" to the disease for each mentally ill patient)
beta <- 0.25  # minimum percentage of the population that represent a critical mass for mental illness awareness
P0 <- N * 0.10 #initially affected population
D0 <- P0 * 0.8  # Initial diagnosed individuals
prevalence0 <- P0 / N
PPV0 <- (Se * prevalence0) / (Se * prevalence0 + (1 - Sp) * (1 - prevalence0))

## Differential equations models
model <- function(time, state, parameters) {
  P <- state[1]
  D <- state[2]
  PPV <- state[3]
  prevalence <- P / N
  PPV <- (Se * prevalence) / (Se * prevalence + (1 - Sp) * (1 - prevalence))
  NPV <- (Sp * (1 - prevalence)) / (Sp * (1 - prevalence) + (1 - Se) * prevalence)
  alpha_adj <- alpha * (PPV + (1 - NPV)) * (1 + (sigma * D) / (beta * N))
  dPdt <- r * P * (1 - P / (N * K))
  dDdt <- alpha_adj * P
  
  list(c(dPdt, dDdt, PPV))
}

## Initial conditions
initial_conditions <- c(P = P0, D = D0, PPV= PPV0)

## Time points (months)
times <- seq(0, 48, length.out = 12)

## Solve the system of differential equations
solution <- ode(y = initial_conditions, times = times, func = model, parms = NULL)

solution_df <- as.data.frame(solution)
nnt <- 8  #number needed to treat
nnh <- 13 #number needed to harm

responders_op_rate <- 1/nnt #percentage of responders over_placebo
harmed_op_rate <- 1/nnh #percentage of harmed patients over_placebo

## a loop to build responders and non-responders
responders_pop <- 0
harmed_pop <- 0
for (i in 1: length(solution_df)){
  TP <- solution_df[i,4] * solution_df[i,3]
  
  FP <- solution_df[i,2] - TP
  responders_pop[i] <- TP*responders_op_rate
  harmed_pop[i] <- (TP+FP)*harmed_op_rate
}

df_response_ratio <- data.frame(responders = responders_pop, 
                                harmed = harmed_pop) # ,  error = percent_diagnostic_error)






