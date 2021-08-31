# simulate fish abundance data and calculate power to detect a change in population size
# also calculate bias of the estimated magnitude of change from the simulated 'truth'

# cabuelow
# 2021-09-01

#TODO: get some real data to estimate alpha and variance parameters from a time series
# include variance for random effects 

library(tidyverse)

# example change in fish abundance over time

x <-  seq(0, 1, by = 0.1) # time covariate
alpha <- 12 # intercept, mean pop. size at time 0
gamma <- log(2) # expect pop. size to double (factor of 2) - we log to estimate the additive effect
var <- 0.1 # stochasticity/variance due to environmental variability
yexp <- alpha*exp(gamma*x) # expected true trend - use log-linear equation for count data (can't have counts less than zero)
yobs_pois <- rpois(length(x), lambda = alpha*exp(gamma*x + rnorm(x, sd = var))) # add stochasticity due to environmental variability from a normal distribution

# plot true trend (yexp) and observed trend (yobs_pois) with extra environmental stochasticity

ggplot() + 
  geom_line(aes(x = x, y = yexp)) +
  geom_point(aes(x = x, y = yobs_pois)) +
  ylab('Fish abundance (counts)') +
  xlab('Time step') +
  theme_classic() 

# simulate scenarios

set.seed(123) # set random number generator to get reproducible results

# make simulation scenario factors (i.e. number of cameras, expected pop change, etc)

nreps <- c(3:10) # number of cameras per site 
change <- c(1.5, 2, 3) # expected change in fish pop abundance (multiplicative factor of increase)
variance <- c(0, 0.5, 1) # degree of environmental stochasticity (sd)
alpha <- 12 # intercept, mean pop. size at time 0
factors <- expand.grid(reps = nreps, change = change, var = variance) # make all combinations of factors for simulating
datsim <- data.frame(Reps = NA, Variance = NA, Effect_size = NA, Power = NA, Bias = NA) # dataframe for storing results of simulations

# loop through each simulation scenario (i.e. combination of simulation factors)

system.time( # approx. 3 mins to run
for(j in 1:nrow(factors)){

nsims <- 500 # number of simulations
pval <- numeric(nsims) # for storing pvalues and calculating power to detect change in population size
bias <- numeric(nsims) # for storing difference between simulated pop. size ('truth') and pop. size estimated from simulated datasets

reps <- factors[j,1] # get number of camera reps
b <- factors[j,2] # get expected change in pop. size
var <- factors[j,3] # get degree of enviro. stochasticity 

# make 500 simulated datasets under selected scenario

for(i in 1:nsims){

# simulate fish observations from poisson distribution, depending on number of camera reps

x <-  seq(0, 1, by = 0.1) # time covariate
gamma <- log(b) # additive change in population size
  
ls <- list() # list for storing results
for(d in 1:reps){
yobs_pois <- rpois(length(x), lambda = alpha*exp(gamma*x + rnorm(x, sd = var))) # add some extra stochasticity from a normal distribution, with variable sd
ls[[d]] <- data.frame(rep = rep(reps, length(yobs_pois)), day = x, yobs = yobs_pois)
}
df <- do.call(rbind, ls)

# fit glm to the simulated data and calculate whether able to detect an effect

m1 <- glm(df$yobs ~ df$day, family = poisson(link = "log")) # glm with poisson distribution for count data
pval[i] <- coef(summary(m1))[,'Pr(>|z|)'][2] # p-value - do we detect a statistically significant change in pop. size
bias[i] <- abs(gamma - coef(m1)[1]) # bias of estimated trend in pop. abundance over time

}

# calculate power and accuracy under each simuation scenario and store in data frame for plotting later

datsim[j,1] <- reps
datsim[j,2] <- var
datsim[j,3] <- b
datsim[j,4] <- sum(pval < 0.05)/nsims
datsim[j,5] <- median(bias)

})

# plot 

datsim$Variance <- as.factor(datsim$Variance)

ggplot(datsim) +
  geom_point(aes(x = Reps, y = Power, col = Variance)) +
  facet_wrap(~Effect_size) +
  theme_classic()

ggplot(datsim) +
  geom_point(aes(x = Reps, y = Bias, col = Variance)) +
  facet_wrap(~Effect_size) +
  theme_classic()

  

