# simulate data on fish abundance (n = 2 cameras per site, deployment time one to 12 hours)
# force variance in estimate of fish abundance to decrease the longer cameras are left out
# use bootstrap resampling to estimate 95% confidence interval width (precision) of abundance estimate

# cabuelow
# 2021-09-01

library(tidyverse)

set.seed(123) # set random number generator to obtain reproducible results

# factors

i_rep <- 2 # camera reps/site
i_hours <- c(1:12) # number of deployment hours

# simulate abundance data

ls <- list() # list for storing results

# loop through number of camera hours and simulate fish counts (yobs)
# expected average count of fish across camera reps is 12

for(i in seq_along(i_hours)){
  hour <- rep(i, i_rep)
  rep <- c(1:i_rep)
  y <- round(abs(rnorm(n = i_rep, mean = 12, sd = 5/i)), 0) # use normal distribution to force variance to decrease with increased number camera hours
  dat <- data.frame(d_hours = hour, c_rep = rep, yobs = y)
  ls[[i]] <- dat
}

dat <- do.call(rbind, ls) # bind simulated data into a dataframe for plotting

# plot data

ggplot(dat) +
  geom_point(aes(x = as.factor(d_hours), y = yobs)) +
  geom_smooth(aes(x = d_hours, y = yobs)) +
  theme_classic()

# bootstrap resample precision (95% confidence interval width) around mean abundance from camera reps

dat.CI <- data.frame(cams = NA, hour = NA, mean_obs = NA, upp.95CI = NA, lwr.95CI = NA) # dataframe for storing results

# loop through number of camera deployment hours and calculate precision

for(j in 1:max(i_hours)){
  
  d2 <- filter(dat, d_hours == paste(j)) # subset data for number of hours
  nboots <- 10000 # number of bootstrap samples
  boot.MSE.result <- numeric(nboots) # for storing results   
  
  for(i in 1:nboots){
     boot.samp <- sample(d2$yobs, nrow(d2), replace=TRUE) # get bootstrap sample 
     boot.MSE.result[i] <- (mean(boot.samp) - mean(d2$yobs))^2 # calculate mean squared error between bootstrap sample and observed/simulated data
  }
  
  SE <- sqrt(mean(boot.MSE.result)) # calculate standard error of the mean across all bootstrapped samples
  
  dat.CI[j,1] <- i_rep
  dat.CI[j,2] <- d2[1,1]
  dat.CI[j,3] <- mean(d2$yobs)
  dat.CI[j,4] <- upper95.limit <- mean(d2$yobs) + qnorm(0.975)*SE # upper 95 percent confidence interval
  dat.CI[j,5] <- lower95.limit <- mean(d2$yobs) - qnorm(0.975)*SE # lower 95 percent confidence interval
}

# plot

ggplot(dat.CI) +
  geom_point(aes(x = as.factor(hour), y = mean_obs)) +
  geom_errorbar(aes(x = as.factor(hour), ymin=lwr.95CI, ymax=upp.95CI), colour="black", width=.1) +
  geom_hline(yintercept = 12, lty = 'dashed') +
  ylab('Precision and Accuracy of Abundance estimate') +
  xlab('Camera deployment time') +
  theme_classic()

