# Simulating trends in fish population abundance to estimate power and bias

This repository provides code to simulate fish population abundance under different monitoring scenarios.

1. data-sim-cam-deploy.R
  - This script simulates fish counts from video cameras (camera deployment time varies from 1 to 12 hours)
  - Bootstrap resampling is used to estimate precision of fish abundance estimates given camera deployment time

2. data-sim-trends.R
  - This script simulates trends in fish population abundance under different monitoring scenarios
  - Power to detect a trend and bias of the estimated trend under each scenario is calculated


