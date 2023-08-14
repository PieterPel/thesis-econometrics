# Script that contains function that can load the stock market data

### Load packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(lubridate,
               alphavantager)


### Set API key

key = c("INSERT YOUR API KEY HERE")

### Function that loads the data
load_data <- function(ticker,
                      interval,
                      first_difference = FALSE,
                      num_lags = 0,
                      steps_ahead = 1,
                      years = c("1", "2"),
                      months = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12") ) {
  
  ### Get data from Alpha Vantage API
  
  # Set API key
  av_api_key(key)
  
  # Extract data
  return_df <- data.frame( matrix(nrow=0,ncol=6) )
  for (year in years) {
    for (month in months) {
      
      slice_name <- paste("year", year, "month", month, sep='')
      df <- av_get(symbol=ticker, av_fun="TIME_SERIES_INTRADAY_EXTENDED", interval=interval, slice=slice_name, adjusted=TRUE)
      return_df <- rbind(return_df, df)
      Sys.sleep(13) # If you do not have the premium version of the API you are limited to 5 pulls per minute
    }
  }
  
  # Set the column names
  colnames(return_df) <- c("Time", "Open", "Low", "Close", "High", "Volume")
  
  # Reverse order 
  return_df <- return_df[nrow(return_df):1, ]
  
  ### Take first differences if desired
  if(first_difference) {
    
    # Change columns
    return_df$Open <- append( NA, diff(return_df$Open) )
    return_df$originalClose <- return_df$Close
    return_df$Close <- append( NA, diff(return_df$Close) )
    return_df$Low <- append( NA, diff(return_df$Low) )
    return_df$High <- append( NA, diff(return_df$High) )
    return_df <- return_df[-1,] # Remove first row since it has no information on first differences
  }
  
  ### Add target closing price column
  
  # Get close value at t
  close_at_t <- return_df$originalClose
  
  # Get close value at t+h
  h_ahead_close <- append( tail(close_at_t, length(close_at_t) - steps_ahead), rep(NA, steps_ahead) )
  
  # Target value is h-step ahead difference
  return_df$Target_Close <- h_ahead_close - close_at_t
  
  # return_df$Target_Close <- append(return_df$Close[-(1:steps_ahead)], rep(NA, steps_ahead))
  
  ### Add how many minutes the market has been open that day
  time_to_df <- c()
  for (i in 1:nrow(return_df)) {
    time <- return_df[i, 1]$Time
    time_in_minutes <- hour(time) * 60 + minute(time)
    time_to_df <- append(time_to_df, time_in_minutes)
  }
  
  return_df$Time_in_minutes <- time_to_df
  
  ### Add lags (if num_lags == 0, this won't do anything)
  data <- return_df # Data frame that you want to add lags to
  
  # loop over the columns and create the lags
  for (col in c("Open", "High", "Low", "Close", "Volume")) {
    
    # Do not add any lags if the number of lags is 0
    if (num_lags == 0) {
      break
    }
    
    for (lag in 1:num_lags) {
      # create lagged column with appropriate name
      lagged_col <- paste0(col, "_lag", lag)
      
      # create a new column with NAs for the first lag rows
      data[[lagged_col]] <- c(rep(NA, lag), data[[col]][1:(nrow(data)-lag)])
    }
  }
  
  ### Store data
  return_df_with_lags <- head(data, -steps_ahead) # Remove last steps_ahead rows because those targets are unknown
  
  ### Return data
  return_df_with_lags
}






