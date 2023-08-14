# Analysis of the Stock Market dataset

#|=== Directory setup
path = dirname(rstudioapi::getSourceEditorContext()$path)
path <- substr(path, 1, nchar(path) - 4)
setwd(path)

### Source scripts and load packages
source('src/loading_data.R')
source("src/qinsamp.R")
source("src/predict_with_LSTM.R")
source("src/predict_with_XGBoost.R")
source("src/e_stats.R")
source("src/simulate_trading.R")

chooseCRANmirror(graphics = FALSE)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(Metrics,
               tseries,
               energy,
               rbart,
               randomForest,
               caret,
               xtable,
               forecast,
               xgboost,
               scales,
               e1071)


###############################################################################
##### Loading data and predicting

# 'Standard' settings
tickers <- c("AAPL", "MSFT", "AMZN", "NVDA", "GOOGL", "META", "TSLA", "UNH", "XOM", "SPY")
intervals <- c("60min")
years <- c("1", "2")
months <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
num_lags <- c(5)
steps_ahead <- c(1)
methods <- c('RW', "RF", 'BART', 'HBART', 'XGBoost')
data_already_available <- FALSE

### Function that performs the predictions and loads the data
run_stock_market <- function(tickers,
                             intervals = c("60min"),
                             years = c("1", "2"),
                             months = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                             num_lags = c(5),
                             steps_ahead = c(1),
                             methods,
                             data_already_available = FALSE) {
  # Cycle over all tickers
  for (ticker in tickers) {
    
    # Initialize list that is returned
    models <- list()
    
    # Cycle over all intervals
    for (interval in intervals) {
      
      for (step in steps_ahead) {
        
        step_string <- toString(step)
        
        # Cycle over all number of lags
        for (num_lag in num_lags) {
          
          num_lag_string <- toString(num_lag)
          
          # Load and save data for RF, BART and HBART (first difference and with lags in data)
          if (any( c("RF", "BART", "HBART", "XGBoost") %in% methods) ) {
            file_name <- paste(ticker, interval, step, num_lag, sep='_')
            file_path <- paste('data/raw/stockprices/first_difference/', file_name, '.csv', sep='')
            if (!data_already_available) {
              data <- load_data(ticker=ticker, interval=interval, years=years, months=months, num_lag=num_lag, steps_ahead = step, first_difference = TRUE)
              write.csv(data, file_path)
            } else {
              data <- read.csv(file_path)[,-c(1)] # First column gets strange when loading the data

            }
          }
          
          # Load and save data for LSTM and RW (level and no lags)
          if (any( c("RW", "LSTM") %in% methods) ) {
            file_name <- paste(ticker, interval, step, sep='_')
            file_path <- paste('data/raw/stockprices/level/', file_name, '.csv', sep='')
            if (!data_already_available) {
              data_level <- load_data(ticker=ticker,interval = interval, first_difference=FALSE, num_lags = 0, steps_ahead = step)
              write.csv(data_level, file_path)
            } else {
              data_level <- read.csv(file_path)[,-c(1)] # First column gets strange when loading the data
            }
          } 
          
          # Split in training and test data for RF and (H)BART
          n <- floor( nrow(data) / 2 ) 
          train <- na.omit(data[(1+num_lag):n,])
          test <- na.omit(data[(n+1):nrow(data),])
          
          # Save in and out-of-sample x and y
          models[[interval]][[step_string]][[num_lag_string]][['in_sample_x']] <- train[,-c(1, 7, 8)]
          models[[interval]][[step_string]][[num_lag_string]][['out_of_sample_x']] <- test[,-c(1, 7, 8)]
          models[[interval]][[step_string]][[num_lag_string]][['in_sample_y']] <- train$Target_Close
          models[[interval]][[step_string]][[num_lag_string]][['out_of_sample_y']] <- test$Target_Close
          
          # Cycle over all methods
          for (method in methods) {
            
            # Print status
            print( paste(ticker, interval, step, num_lag, method), sep=' ' )
            
            # Store start time
            start_time <- Sys.time()
            
            # Random forest
            if (method == "RF") {
              y <- train$Target_Close
              yp <- test$Target_Close
              
              # Run random forest method
              model <- randomForest(Target_Close ~ ., data=train[,-c(1,7)], importance=TRUE, ntree=500) # Don't consider Time and originalClose
              
              # Make predictions using the model
              predictions <- predict(model, test)
              
              # Make fitted values for the training sample
              fitted_values <- predict(model, train)
            }
            
            if (method == "XGBoost") {
              # Run model
              xgboost_run <- predict_with_xgboost(train_data=train[,-c(1,7)], test_data=test[,-c(1,7)], target_col='Target_Close')
              
              # Extract output
              model <- xgboost_run$model
              
              # Predict and fit
              predictions <- xgboost_run$predictions
              fitted_values <- xgboost_run$fitted_values
              
              # Actual values
              y <- xgboost_run$y
              yp <- xgboost_run$yp
              
            }
            
            # (H)BART
            if ( method %in% c("HBART", "BART") ) {
              # Parameters
              burn <- 1000
              nd <- 2000
              
              # Train data
              x <- train[,-c(1, 7, 8)] # Remove Time, original_Close and Target_Close
              y <- train$Target_Close
              
              # Test data
              xp <- test[,-c(1, 7, 8)] # Remove Time, original_Close and Target_Close
              yp <- test$Target_Close
              np <- nrow(xp)
              
              # Variance prior
              nu=3
              sigq=0.90
              qchi=qchisq(1.0-sigq,nu)
              sighat=sd(y)
              lambda=(sighat*sighat*qchi)/nu
              
              ##run rbart MCMC
              # nskip: burn in draws,
              # ndpost:kept draws,
              # nadapt: initial draws to tune MCMC,
              # numcut: number of cutpoints used for each x
              # k: bigger k gives smoother f 
              set.seed(19)
              if (method == "HBART") { # HBART has ntreeh > 1
                res = rbart(x,y,nskip=burn,ndpost=nd,ntreeh = 40,numcut=1000,k=0.5,tc=5, overallsd=sqrt(lambda), overallnu=nu, printevery = TRUE)
              }
              if (method == "BART") { # BART has ntreeh = 1 and different probabilities of birth and death
                res = rbart(x,y,ntree=200,ntreeh=1,pbd=c(0.7,0.0),nskip=burn,ndpost=nd,numcut=1000,k=2,tc=5,overallsd=sqrt(lambda),overallnu=nu, printevery=TRUE)
              }
              
              ## now predict and fit to get inference
              predictions_raw = predict(res,x.test=xp)
              fitted_values_raw = predict(res,x.test=x)
              
              model <- res
              models[[interval]][[step_string]][[num_lag_string]][[method]][['predictions_raw']] <- predictions_raw
              models[[interval]][[step_string]][[num_lag_string]][[method]][['fitted_values_raw']] <- fitted_values_raw
              
              predictions = predictions_raw$mmean
              fitted_values = fitted_values_raw$mmean
              
              # e-statistic
              pdraw = predictions_raw$mdraws + predictions_raw$sdraws * matrix(rnorm(nd*np),nrow=nd)
              qvec = qsamp(yp,pdraw)
              
              estat <- edist(matrix(c(qvec,runif(10000)),ncol=1),c(np,10000))[1]
              models[[interval]][[step_string]][[num_lag_string]][[method]][['estat']] <- estat
            }
            
            # RW
            if (method == "RW") {
              y <- train$Target_Close
              yp <- test$Target_Close
              
              # Predict and fit
              predictions <- rep(0, length(yp))
              fitted_values <- rep(0, length(y))
              
              model <- NULL
            }
            
            # Store end time
            end_time <- Sys.time()
            
            ## Save results
            models[[interval]][[step_string]][[num_lag_string]][[method]][['model']] <- model
            models[[interval]][[step_string]][[num_lag_string]][[method]][['predictions']] <- predictions
            models[[interval]][[step_string]][[num_lag_string]][[method]][['fitted_values']] <- fitted_values
            models[[interval]][[step_string]][[num_lag_string]][[method]][['y']] <- y
            models[[interval]][[step_string]][[num_lag_string]][[method]][['yp']] <- yp
            
            models[[interval]][[step_string]][[num_lag_string]][[method]][['cor']] <- cor(predictions, yp)
            models[[interval]][[step_string]][[num_lag_string]][[method]][['rmse']] <- rmse(predictions, yp)
            models[[interval]][[step_string]][[num_lag_string]][[method]][['mae']] <- mae(predictions, yp)
            
            models[[interval]][[step_string]][[num_lag_string]][[method]][['computation_time']] <- end_time - start_time # Not exact since more stuff is done than just getting the models
            
            folder_path <- paste("output/stock_market/", ticker, sep='')
            
            if (!file.exists(folder_path)) {
              dir.create(folder_path)
            } 
            
            file_path <- paste(folder_path, '/models_xgboost.RData', sep='')
            
            saveRDS(models, file=file_path) # Saved at every step to ensure that progress is saved during the run
          }
        }
      }
    }
  }
  
  # Return
  return(models)
}

###############################################################################
##### Analysis

### Figure 9: The hourly closing price of $MSFT over the whole sample between June 7th 2021 and May 26th 2023.
data_level_msft <- read.csv("data/raw/stockprices/level/MSFT_60min_1.csv")
data_fd_msft <- read.csv("data/raw/stockprices/first_difference/MSFT_60min_1_5.csv")

# Convert 'Time' column to character format
data_level_msft$Time <- as.character(data_level_msft$Time)

# Convert 'Time' column to POSIXct
data_level_msft$Time <- as.POSIXct(data_level_msft$Time, format = "%Y-%m-%d %H:%M:%S")

# Adjusting plot theme and aesthetics
pdf('output/stock_market/MSFT/plots/closeprice.pdf', width=7, height=6)
ggplot(data_level_msft, aes(x = Time, y = Close)) +
  geom_line(aes(group = 1), color = "blue") +  # Adjust line color as needed
  labs(x = "Time", y = "Closing price") +
  
  # Modifying plot theme
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add border
    axis.line = element_line(size = 0.5),  # Adjust axis line thickness
    axis.text = element_text(size = 10),  # Adjust axis text size
    axis.title = element_text(size = 12, face = "bold"),  # Adjust axis title size and style
    plot.title = element_text(size = 14, face = "bold"),  # Adjust plot title size and style
    legend.position = "none"  # Remove legend
  ) +
  
  # Adjusting axes ticks and labels
  scale_x_datetime(
    breaks = "3 months",
    date_labels = "%b %y",
    limits = c(min(data_level_msft$Time), max(data_level_msft$Time))
  ) +
  scale_y_continuous(
    breaks = seq(floor(min(data_level_msft$Close)), ceiling(max(data_level_msft$Close)), by = 20)
  ) +
  
  # Adding vertical red line
  geom_vline(xintercept = as.numeric(data_level_msft$Time[3965]), color = "red", linetype = 1)

dev.off()

# Figure 10: histogram.
pdf('output/stock_market/MSFT/plots/histogram.pdf', width=12, height=8)
ggplot(data_fd_msft, aes(x = Close)) +
  geom_histogram(binwidth = 0.5, color = "transparent", fill = "grey") +
  labs(x = ("1-hour change in closing price"), y = "Frequency") +
  
  # Modifying plot theme
  scale_y_sqrt(breaks = seq(0, max(hist(data_fd_msft$Close, plot = FALSE)$counts), by = 200)) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Add border
    axis.line = element_line(size = 0.5),  # Adjust axis line thickness
    axis.text = element_text(size = 20),  # Adjust axis text size
    axis.title = element_text(size = 24),  # Adjust axis title size and style
    plot.title = element_text(size = 28),  # Adjust plot title size and style
    legend.position = "none"  # Remove legend
  )   

dev.off()

### Table 8: statistics of $MSFT data.
stats <- c("mean", "median","standard deviation", "skewness", "kurtosis", "JB")
xs <- c("Open", "Close", "Low", "High", "Volume")

table.summary <- as.data.frame(matrix(ncol=length(stats), nrow=length(xs)))
rownames(table.summary) <- xs
colnames(table.summary) <- stats

df <- data_fd_msft

for (x in xs) {
  
  # Extract observations
  obs <- df[[x]]
  
  for (stat in stats) {
    
    # Compute value of statistic
    if(stat == "mean") {
      value <- mean(obs)
    } else if (stat == "median") {
      value <- median(obs)
    } else if (stat == "standard deviation") {
      value <- sd(obs)
    } else if (stat == "skewness") {
      value <- skewness(obs)
    } else if (stat == "kurtosis") {
      value <- kurtosis(obs)
    } else if (stat == "JB") {
      value <- jarque.bera.test(obs)$statistic
    }
    
    # Store value
    table.summary[x, stat] <- value
    
  }
}

xtable(table.summary)

### Table 9: RMSEs with different number of lags.
ticker= 'MSFT'
num_lags <- c(5,10,20,30,40)
methods <- c('RW', "RF", 'XGBoost', 'BART', 'HBART')

models <- run_stock_market(ticker=ticker, num_lags=num_lags, methods=methods, data_already_available = TRUE)

models <- readRDS('output/stock_market/MSFT/models_lags.RData') # Remove later

rmse.table.lags <- as.data.frame( matrix(ncol=length(num_lags), nrow=length(methods)) )
mae.table.lags <- as.data.frame( matrix(ncol=length(num_lags), nrow=length(methods)) )
colnames(rmse.table.lags) <- colnames(mae.table.lags) <- num_lags
rownames(rmse.table.lags) <- rownames(mae.table.lags) <- methods


make_percentage_table <- function(df, digits=2) {
  # Initialize data frame that is returned
  df_pct <- df
  
  # Cycle over all rows (methods in our case)
  for (row in rownames(df)[-1])
    
    for (col in colnames(df)) {
      
      # Extract values
      base_val <- as.numeric(df[rownames(df)[1], col])
      val <- as.numeric(df[row, col])
      
      # Calculate percentage
      pct <- 100 * round(val / base_val, digits=digits)
      
      # Store value
      df_pct[row, col] <- paste(pct, '%', sep='')
    }
  
  # Round values of first row
  for (col in colnames(df)) {
    df_pct[rownames(df)[1], col] <- round(as.numeric(df[rownames(df)[1], col]), digits=digits)
  }
  
  return(df_pct)
}

# Cycle over all methods
for (method in methods) {
  
  # Cycle over all all lags
  for (num_lag in num_lags) {
    num_lag_string <- toString(num_lag)
    
    # Store value
    rmse.table.lags[method, num_lag_string] <- models[['60min']][['1']][[num_lag_string]][[method]][['rmse']]
    mae.table.lags[method, num_lag_string] <- models[['60min']][['1']][[num_lag_string]][[method]][['mae']]
  }
}

xtable(make_percentage_table(rmse.table.lags))
xtable(make_percentage_table(mae.table.lags))

### Table 10: cross-validation for BART and HBART.
## Get e-statistics
set.seed(27) #just for reproducibility
k <- 5
kappas <- c(0.25, 0.5, 1, 2, 5, 10, 20)

data <- load_data(ticker, "60min", TRUE, 5, 1)

n <- floor( nrow(data) / 2 ) 
train <- na.omit(data[(1+num_lag):n,])
test <- na.omit(data[(n+1):nrow(data),])

# Train data
x <- train[,-c(1, 7, 8)] # Remove Time, original_Close and Target_Close
y <- train$Target_Close

# Compute e-statistics
y <- as.matrix(y)
e_stats <- e_stats_cv(x,y,k,kappas,method="HBART")
e_stats.homo <- e_stats_cv(x,y,k,kappas,method="BART")
cv_estats <- list('hetero'=e_stats, 'homo'=e_stats.homo)

saveRDS(cv_estats, file="output/stock_market/MSFT/e_stats_best.RData")

## Create table

# Set up table
e_stats_table <- as.data.frame(matrix(ncol=(length(kappas)), nrow=2))
colnames(e_stats_table) <- paste(kappas)
rownames(e_stats_table) <- c("HBART", "BART")

# Compute averages
averages <- apply(e_stats, 2, mean)
averages.homo <- apply(e_stats.homo, 2, mean)

# Fill table
e_stats_table['HBART', ] <- round(averages, digits=2)
e_stats_table['BART', ] <- round(averages.homo, digits=2)

# Return LaTeX table
xtable(e_stats_table)

### Table 11: RMSEs for different step-ahead forecasts.

## Create data
tickers <- c("MSFT")
intervals <- c("60min")
years <- c("1", "2")
months <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
num_lags <- c(5)
steps_ahead <- c(3, 6, 9, 12)
methods <- c('RW', "RF",'XGBoost', 'BART', 'HBART' )
data_already_available <- TRUE

models_teststeps_fixed <- run_stock_market(tickers=tickers,num_lags=num_lags, intervals=intervals, steps_ahead = steps_ahead, data_already_available = data_already_available, methods=methods)
models_teststeps <- readRDS(file='output/stock_market/MSFT/models_teststeps.RData')

## Create RMSE (And DM), MAE and e-stat tables

# Initalize tables
table.rmse.steps <- as.data.frame( matrix(ncol=length(steps_ahead), nrow=length(methods)) )
colnames(table.rmse.steps) <- steps_ahead
rownames(table.rmse.steps) <- methods

table.mae.steps <- as.data.frame( matrix(ncol=length(steps_ahead), nrow=length(methods)) )
colnames(table.mae.steps) <- steps_ahead
rownames(table.mae.steps) <- methods

table.estat.steps <- as.data.frame( matrix(nrow=2, ncol=length(steps_ahead)) )
rownames(table.estat.steps) <- c("HBART", "BART")
colnames(table.estat.steps) <- steps_ahead

# Cycle over all steps
for (step in steps_ahead) {
  
  step_string <- toString(step)
  
  # Cycle over all methods
  for (method in methods) {
    
    table.rmse.steps[method, step_string] <- models_teststeps[['60min']][[step_string]][["5"]][[method]][['rmse']]
    table.mae.steps[method, step_string] <- models_teststeps[['60min']][[step_string]][["5"]][[method]][['mae']]
    
    if (method %in% rownames(table.estat.steps)) {
      table.estat.steps[method, step_string] <- models_teststeps[['60min']][[step_string]][["5"]][[method]][['estat']]
    }
    
  }
}

xtable(make_percentage_table(table.rmse.steps))
xtable(table.mae.steps)
xtable(table.estat.steps)

### Figure 11: predictive qq-plots of posterior draws for the 3959 out-of-sample 1-hour ahead changes in the closing price of $MSFT calibrated to the uniform distribution.

resp <- models_teststeps[['60min']][['1']][['5']][['HBART']]$predictions_raw
resp.homo <- models_teststeps[['60min']][['1']][['5']][['BART']]$predictions_raw
yp <- models_teststeps[['60min']][['1']][['5']][['HBART']]$yp
nd <- 2000

np <- length(resp[['mmean']])
pdraw = resp$mdraws + resp$sdraws * matrix(rnorm(nd*np),nrow=nd)
qvec = qsamp(yp,pdraw)

np <- length(resp.homo[['mmean']])
pdraw.homo = resp.homo$mdraws + resp.homo$sdraws * matrix(rnorm(nd*np),nrow=nd)
qvec.homo = qsamp(yp,pdraw.homo)

# Plot
pdf('output/stock_market/MSFT/plots/msft-qqplot.pdf', width=14, height=6)
par(mfrow=c(1,2))
qqplot(qvec,runif(10000),col="grey",cex.lab=1.5,xlab="sample quantile",ylab="uniform",cex.axis=1.5)
abline(0,1,col="black",lwd=3)
#title(main="(a) predictive qqplots, heteroscedastic model",cex.main=1.2)

qqplot(qvec.homo,runif(10000),col="grey",cex.lab=1.5,xlab="sample quantile",ylab="uniform",cex.axis=1.5)
abline(0,1,col="black",lwd=3)
#title(main="(b) predictive qqplots, homoscedastic model",cex.main=1.2)

dev.off()

### Figure 12:  the median posterior draw of the standard deviation from HBART vs the time of the day
pdf('output/stock_market/MSFT/plots/time_sd.pdf', width=7,height=6)
time <- models_teststeps[['60min']][['1']][['5']][['out_of_sample_x']]$Time_in_minutes
volume <- models_teststeps[['60min']][['1']][['5']][['out_of_sample_x']]$Volume
sd_median <- resp$s.5

# Convert time from minutes to hours
time_hours <- time / 60

# Set the desired x-axis limits
x_min <- min(time_hours)
x_max <- max(time_hours)
x_range <- x_max - x_min

# Increase the number of hours on the x-axis
num_hours <- 6  # Adjust the number of hours as needed
x_ticks <- seq(x_min, x_max, length.out = num_hours + 1)

plot(time_hours, sd_median, xlim = c(x_min, x_max), xaxt = "n", # Set xlim and xaxt
     xlab='Time in hours',
     ylab='Median posterior sd')  

# Customize x-axis tick labels
axis(1, at = x_ticks, labels = format(as.POSIXct(x_ticks * 60 * 60, origin = "1970-01-01"), "%H:%M"))

dev.off()

### Table 12: RMSEs for forecasting change in the closing price of all symbols.
### Table 13: trading performance with 0.5% transaction costs for all symbols.

## Create data
tickers <- c("AAPL", "MSFT", "AMZN", "NVDA", "GOOGL", "META", "TSLA", "UNH", "XOM", "SPY")
methods <- c('RW', "RF",'XGBoost', 'BART', 'HBART' )
models_testtickers <- run_stock_market(tickers=tickers, methods=methods, data_already_available = TRUE) # Only returns the model for the last ticker, the rest is stored in files

## Create tables

table.rmse.tickers <- as.data.frame( matrix(ncol=length(tickers), nrow=length(methods)) )
colnames(table.rmse.tickers) <- tickers
rownames(table.rmse.tickers) <- methods

table.mae.tickers <- as.data.frame( matrix(ncol=length(tickers), nrow=length(methods)) )
colnames(table.mae.tickers) <- tickers
rownames(table.mae.tickers) <- methods

table.estat.tickers <- as.data.frame( matrix(nrow=2, ncol=(length(tickers))) )
rownames(table.estat.tickers) <- c("HBART", "BART")
colnames(table.estat.tickers) <- tickers

table.trading_performance <- as.data.frame( matrix(nrow=length(methods), ncol=length(tickers)) )
colnames(table.trading_performance) <- tickers
rownames(table.trading_performance) <- methods

# Cycle over all tickers
for (ticker in tickers) {
  
  # Load models
  file_path <- paste('output/stock_market/', ticker, '/models_testtickers.RData', sep='')
  models_for_ticker <- readRDS(file=file_path)
  
  # Cycle over all methods
  for (method in methods) {
    
    # RMSE
    table.rmse.tickers[method, ticker] <- models_for_ticker[['60min']][["1"]][["5"]][[method]][['rmse']]
    
    # MAE
    table.mae.tickers[method, ticker] <- models_for_ticker[['60min']][["1"]][["5"]][[method]][['mae']]
    
    # e-stat
    if (method %in% rownames(table.estat.tickers)) {
      table.estat.steps[method, step_string] <- models_for_ticker[['60min']][["1"]][["5"]][[method]][['estat']]
    }
    
    # Trading performance
    predictions <- models_for_ticker[['60min']][["1"]][["5"]][[method]][['predictions']]
    actual <- models_for_ticker[['60min']][["1"]][["5"]][[method]][['yp']]
    
    file_path= paste('data/raw/stockprices/first_difference/', ticker, '_60min_1_5.csv', sep='')
    df <- read.csv(file_path)
    
    level_closes <-  df$originalClose
    
    initial_price <- tail(level_closes, length(actual))[1]
    
    simulation <- simulate_trading(predictions=predictions, actual=actual, transaction_cost=0.005, initial_price=initial_price )
    return_val <- 100 * ( tail(simulation$portfolio_values, 1) / head(simulation$portfolio_values, 1) - 1 )
    
    table.trading_performance[method, ticker] <- return_val
    
  }
}

xtable(make_percentage_table(table.rmse.tickers))
xtable(table.mae.tickers)
xtable(table.estat.tickers)

# Box plot

# Transpose the data frame
e <- as.data.frame(t(table.trading_performance))

# Set up the plotting area
par(mfrow = c(1, ncol(e) ))  # Adjust the layout based on the number of columns

# Loop through each column and create a box plot
for (col in names(e[1:ncol(e)])) {
  boxplot(e[[col]], main = col, ylim=c(0,300), ylab = "Value", col = "lightblue", border = "black")
}

### Table 14: trading performance of HBART for different levels of transaction costs.

# Initialize 
shares <- c(0, 0.0001, 0.001, 0.002, 0.005, 0.0075, 0.01, 0.02)
table.trading_performance_hbart <- as.data.frame( matrix(nrow=length(tickers), ncol=length(shares)) )
colnames(table.trading_performance_hbart) <- shares
rownames(table.trading_performance_hbart) <- tickers

# Cycle over all tickers
for (ticker in tickers) {
  
  # Load models
  file_path <- paste('output/stock_market/', ticker, '/models_testtickers.RData', sep='')
  models_for_ticker <- readRDS(file=file_path)
  
  # Load initial price
  predictions <- models_for_ticker[['60min']][["1"]][["5"]][["HBART"]][['predictions']]
  actual <- models_for_ticker[['60min']][["1"]][["5"]][["HBART"]][['yp']]
  
  file_path= paste('data/raw/stockprices/first_difference/', ticker, '_60min_1_5.csv', sep='')
  df <- read.csv(file_path)
  
  level_closes <-  df$originalClose
  initial_price <- tail(level_closes, length(actual))[1]
  
  # Cycle over all shares
  for (share in shares) {
    
    share_string <- toString(share)
    
    simulation <- simulate_trading(predictions=predictions, actual=actual, transaction_cost=share, initial_price =initial_price)
    
    # Calculate excess return
    return_val <- 100 * ( ( tail(simulation$portfolio_values, 1)/ head(simulation$portfolio_values, 1) ) - 1 ) - 100 * (100*tail(simulation$stock_prices, 1)/head(simulation$portfolio_values, 1) - 1) 
    
    table.trading_performance_hbart[ticker, share_string] <- return_val
    
    }
}

xtable(table.trading_performance)
xtable(table.trading_performance_hbart)

## 'Manually' added XGBoost to everything myself
models_lags <- readRDS(file='output/stock_market/MSFT/models_lags.RData')

for (lag in c(1,5,10,20,30,40)) {
  lag_string <- toString(lag)
  
  models_lags[['60min']][['1']][[lag_string]][["XGBoost"]] <- models[['60min']][['1']][[lag_string]][['XGBoost']]
}

models_steps <- readRDS(file='output/stock_market/MSFT/models_teststeps.RData')

for (step in c(1, 3, 6, 9, 12)) {
  
  step_string <- toString(step)
  
  models_steps[['60min']][[step_string]][['5']][["XGBoost"]] <- models_teststeps_x[['60min']][[step_string]][['5']][["XGBoost"]]
}

for (ticker in tickers) {
  
  base_path <- paste('output/stock_market/', ticker, sep='')
  
  model_xgboost <- readRDS(paste(base_path, '/models_xgboost.RData', sep=''))
  model_others <- readRDS(paste(base_path, '/models_testtickers.RData', sep=''))
  
  model_others[['60min']][['1']][['5']][['XGBoost']] <- model_xgboost[['60min']][['1']][['5']][['XGBoost']]
    
  saveRDS(model_others, paste(base_path, '/models_testtickers.RData', sep=''))
  
}




  