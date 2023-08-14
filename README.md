
# Introduction
- This directory contains the raw data and R scripts used by  Pieter Pel for his Bachelor's thesis at the Erasmus University of Rotterdam for the Econometrics and Economics bachelor programme. 
- For this thesis the forecasting performance of several machine learning models was tested with four datasets. The models are random forest, XGBoost, BART and HBART. The predictions were done for used car prices, alcohol use, number of fish caught and hourly change in the stock market. 

# Installation
- R version 4.1.0 was used and can be downloaded from https://www.r-project.org/
- The following CRAN R packages were used and can be installed using the install.packages() command in R:
-- alphavantage
-- coda
-- caret
-- dplyr
-- e1071
-- energy
-- forecast 
-- ggplot2 
-- gridExtra 
-- lubridate 
-- Metrics 
-- randomForest 
-- rbart 
-- scales 
-- tseries 
-- xgboost 
-- xtable 
-- zoo


# Data sources
- The used cars dataset is available in the rbart package and can be extracted using data("ucarprice").
- The alcohol and fishery datasets are available in the supplementary materials of Pratola et. al, (2019).
- The stock market dataset uses data from the Alpha Vantage API, which can be extracted using the alphavantage package.

# Code structure
- All R code used is available under /src:
-- alcohol.R 
-- e_stats.R 
-- fishery.R 
-- loading_data.R 
-- predict_with_xgboost.R 
-- qinsamp.R 
-- simulate.R 
-- stock_market.R 
-- used_cars.R
- qinsamp.R is directly used from Pratola et. al, (2019), alcohol.R and fishery.R were adapted from the same source, the other functions are written by myself.
- How to use the code:
-- alcohol.R does the prediction and analysis of the alcohol dataset
-- fishery.R does the prediction and analysis of the fishery dataset
-- stock_market.R does the prediction and analysis of the stock market dataset
- An elaborate explanation of the code is available in the README.md file under /src. 

# Results
- For an exhaustive discussion and conclusion of the results I refer to my thesis. The main conclusions are that:
-- Machine learning models can adequately predict the 1-hour change in a stock price
-- By adding heteroscedasticity to BART, HBART can make better predictions
-- The predictions are only profitable with a basic trading algorithm for low transaction costs

# Contact information
Pieter Pel: 525573pp@student.eur.nl

# License
Creative Commons
