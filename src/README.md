# Source Code
## alcohol.R, fishery.R
These scripts have the same general procedure
1. The dataset is loaded
2. The dataset is divided in an in-sample and out-of-sample
3. The BART, HBART, RF and XGBoost models are obtained using the in-sample data
4. The models are saved
5. An evaluation table is obtained using the create_eval_table() from used_cars.R
6. A predictive qq-plot is made
7. An H-evidence plot is made

## e_stats.R
This script contains two functions:

### 'get_estat'
- Input: 
-- resp: output from rbart function of the rbart package
-- yp: actual values
- Procedure:
1. he function obtains draws of the distribution implied by (H)BART
2. the quantiles of the draws are obtained with the qsamp function in qinsamp.R
3. he e-statistic is calculated of the quantiles compared to the uniform distribution using the edist function of the energy package
- Output
-- e-statistic

### 'e_stats_cv'
- Input:
-- x: x variables 
-- y: y variable
-- k: number of folds (default is 5)
-- kappas: the values for kappa that you want to cross-validate on
-- method: either 'BART' or 'HBART'
- Procedure
1. The k folds are created
2. An (H)BART model is obtained for the first in-fold and first kappa
3. Predictions are made out-of-fold
4. The e-statistic is calculated using the e_stats function
5. The status is printed
6. Step 2-5 are repeated for every fold and kappa
- Output
-- The e-statistic for every fold for every value of kappa

## loading_data.R
The script contains one function and a place to insert the Alpha Vantage API key

### 'loading_data'
- Input: 
-- ticker: the ticker that you want to get the data of
-- interval: the intraday interval (1min, 5min, 10min, 15min, 30min or 60min)
-- first_difference: makes the data first differenced if true (TRUE or FALSE)
-- num_lagsL the number of lags that are added for the prices and volume
-- steps_ahead: the numer of steps ahead that the target variable is
-- years: of which years data is extracted (subset of 1 and 2)
-- months: of which months data is extracted (subset of 1 through 12)
- Procedure:
1. The data of all months is obtained using the Alphavantage API and put in one dataframe
2. If first_difference is true, the first difference is taken of certain columns
3. The target column is added, which depends on the steps_ahead
4. A column is added that contains the minutes of the day
5. The lags are added, the number is decided by num_lags
- Output:
-- The full dataframe that can be used in stock_market.R

## qinsamp.R
This script contains two functions

### 'qinsamp'
- Input:
-- y: value for y
-- ysamp: sample of y
- Procedure:
-- Calculate quantile
- Output:
-- Quantile of y in ysamp

### 'qsamp'
- Input:
-- y: vector
-- yd: matrix of samples
- Procedure:
-- For each element in y the quantile is calculated for the corresponding sample in yd using the qinsamp function
-- The quantiles are stored
- Output:
-- Quantiles of vector y in matrix of samples yd

## predict_with_xgboost.R
This script contains one function:

### 'predict_with_xgboost'
- Input:
-- train_data: the train data frame
-- test_data: the test data frame
-- target_col: the place of the target variable in train_data and test_data
-- model: a previously made XGBoost model (default is null)
- Procedure:
1. The target variable is extracted and removed from train_data and test_data
2. The train and test data are converted to DMatrix format
3. If model is null, skip to step 6
4. A grid search is performed for hyperparameter tuning
5. The XGBoost model is trained using the optimal hyperparameters
6. With the XGBoost model values are predicted for the test data and fitted for the train data
- Output:
-- list with the model, predictions, fitted values, in-sample target variable and out-of-sample target variable

## simulate_trading.R
This script contains one function:

### 'simulate_trading'
- Input:
-- predictions: vector of predicted changes in price
-- actual: vector of actual changes in price
-- transaction_cost: transaction cost as a share of the price
-- kelly_share: share of stocks that are bought and sold of maximum used (not used, since only a value of 1 is used)
-- initial_num_stocks: inital number of stocks (standard is 100)
-- initial_price: initial price of the stock
-- initial_cash: initial amount of cash (default is 0)
- Procedure:
1. The lists that are returned are initialized
2. If the predicted change in price is larger than the predicted level of transaction costs and there is cash available, stocks are bought
3. If the predicted decrease in price is larger than the predicted level of transaction costs and there are stocks in the portfolio, stocks are sold
4. The price is updated with the actual change
6. Steps 2 - 4 are repeated for every prediction
- Output:
-- List containing at every time the portfolio value, price of the stock, the predicted price and the number of stocks held 

## stock_market.R
This script contains two functions and a general procedure:

### 'run_stock_market'
- Input:
-- tickers: the tickers that are studied
-- intervals: the intraday intervals that are studied (1min, 5min, 10min, 15min, 30min or 60min)
-- num_lags: the number of lags that are studied for the prices and volume
-- steps_ahead: the numer of steps ahead that are studied for the target variable
-- years: of which years data is extracted (subset of 1 and 2)
-- months: of which months data is extracted (subset of 1 through 12)
-- methods: the methods that are used to create models (subset of RW, RF, BART, HBART and XGBoost)
-- data_already_available: boolean that says if the data is already stored in the depository (default is false)
- Procedure:
1. If the data is not available, the data is loaded
2. The data is split in an in and out-of-sample dataset
3. Every model studied is made on the in-sample dataset, then fitted on the in-sample dataset and predicts out-of-sample
4. The models, predictions, fitted values and evaluation metrics are stored
5. Steps 2-4 are repeated for every ticker, interval, number of lags and steps ahead
- Output:
-- list that has the models studied for all tickers, intervals, number of lags and steps ahead

### 'make_percentage_table' 
- Input:
-- df: dataframe that is changed
-- digits: number of digits that the percentage displays
- Procedure:
1. The dataframe that is returned is initialized as a copy of df
2. The value at the first row of df is extracted
3. The value at another row of df is extracted
4. The percentage is calculated and stored
5. This is repeated for every column and row, except the top row
- Output:
-- dataframe where the top row stays the same, but the others are as a percentage of the value at the top row for every column
### Procedure
1. Put you Alphavantage API key in the loading_data.R script
2. A figure is made that shows a graph of the closing price of $MSFT over the whole sample
3. A histogram of the 1-hour ahead change in closing price of $MSFT over the whole sample is made
4. A table is made that shows summary statistics of the dataset used for $MSFT
5. For all models, the run_stock_market function is run for the 1-hour ahead change in $MSFT using a different number of lags
6. Tables are made that show the out-of-sample forecasting performance for the 1-hour ahead change of $MSFT using different number of lags
7. An in-sample 5-fold cross-validation is performed for BART and HBART for the 1-hour ahead forecasts of the change of $MSFT
8. A table is made that shows the in-sample cross-validation result for BART and HBART
9. For all models, the run_stock_market function is run for different hour-ahead forecasts of $MSFT
10. Tables are made that show the out-of-sample forecasting performance for the different hour ahead forecasts for the change of $MSFT.
11. A figure is made that shows the predictive qq-plots for BART and HBART for out-of-sample forecasting the 1-hour ahead change in closing price of $MSFT
12. A figure is made that shows the median posterior draw of the standard deviation from HBART vs the time of the day for the out-of-sample 1-hour ahead forecast of the change in closing price of $MSFT
13. For all models, the run_stock_market function is run for the 1-hour ahead forecasts for all ticker symbols
14. A table is made that shows the RMSE for forecasting the 1-hour ahead change in closing price of all ticker symbols
15. Simultaneously with 9, a table is made that shows the trading performance, obtained using the simulate_trading function in simulate.R with 0.5% transaction costs, using the forecasts and actual values of the 1-hour ahead change in closing price of all tickers 
16. A table is made that shows the excess returns compared to a buy-and-hold strategy using the predictions made by HBART and the actual values using the simulate_trading function in the simulate_trading script for different levels of transaction costs.
17. Note: for Step 4, 6 and 9 the models are saved in the depository. If you want to save them with a different name, you must change the file name in the run_stock_market function before running that function


## used_cars.R
This script contains one function and a general procedure

### 'create_eval_table'
- Input: nothing directly, but it uses variables that are loaded in the environment
- Procedure
1. The table that is returned is intialised
2. A metric is put in to the table
3. Step 2 is repeated for every method and set of predictions
- Output
-- LaTeX table that sumarizes evaluation metrics for the models

### Procedure
1. The dataset is loaded
2. The dataset is divided in an in-sample and out-of-sample
3. A 5-fold cross-validation is performed for BART and HBART using the in-sample data
4. The BART, HBART, RF and XGBoost models are obtained using the in-sample data
5. The models are saved
6. A figure is made that summarizes the continous variables conditional on the trim
7. A table is made that sumarizes the results of the cross-validation by giving the average over the 5 folds for every kappa for BART and HBART
8. An evaluation table is obtained using the create_eval_table() function
9. A predictive qq-plot is made
10. An H-evidence plot is made
11. A figure is made that plots the posterior median price for out-of-sample data plotted as a function of year and mileage, where the size of the dot is the posterior median of the standard deviation


