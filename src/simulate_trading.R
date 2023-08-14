# Function that simulates trading algorithm

simulate_trading <- function(predictions, # Vector that contains the predictions for the change in price
                             actual, # Vector that contains the actual change in price
                             transaction_cost, # Transaction costs as a share of the price
                             kelly_share=1, # Not used, share of stocks possible that are bought or sold
                             initial_num_stocks=100, # Initial number of stocks in the portfolio
                             initial_price=test$originalClose[1], # Initial price of stock
                             initial_cash=0) {# Initial level of cash in the portfolio
                            
  # Initialize 
  return_list <- list()
  portfolio_value <- initial_num_stocks * initial_price + initial_cash
  portfolio_values <- c()
  num_stocks <- initial_num_stocks
  num_stocks_list <- c()
  stock_price <- initial_price # At end of first period
  stock_prices <- c()
  cash <- initial_cash
  transaction_cost <- transaction_cost # As share of the stock price
  kelly_share <- kelly_share
  predicted_prices <- c()
  
  # Loop over all predictions
  for (i in 1:length(predictions)) {
    
    # Store current portfolio value and predicted price
    portfolio_values <- append(portfolio_values, portfolio_value)
    predicted_prices <- append(predicted_prices, stock_price + predictions[i])
    
    ### At end of period i
    
    # Buy stocks if the predicted increase in price is greater than the transaction cost
    if (predictions[i] > transaction_cost * stock_price && cash > 0) {
      # Calculate how many stocks to buy
      num_buy <- cash / (stock_price * (1+transaction_cost)) * kelly_share
      
      # Adjust cash
      cash <- cash - num_buy * stock_price * (1+transaction_cost) 
      
      # Adjust number of stocks
      num_stocks <- num_stocks + num_buy  
    }
    
    # Sell stocks if the predicted decrease is greater than the transaction cost
    if (predictions[i] < -1 * transaction_cost * stock_price && num_stocks > 0) {
      # Calculate how many stocks to sell
      num_sell <- num_stocks * kelly_share 
      
      # Adjust number of stocks
      num_stocks <- num_stocks - num_sell 
      
      # Adjust cash
      cash <- cash + num_sell * stock_price * (1-transaction_cost) 
    }
    
    ### During period i+1
    
    stock_prices <- append(stock_prices, stock_price) # Store current stock price
    num_stocks_list <- append(num_stocks_list, num_stocks) # Store current number of stocks
    stock_price <- stock_price + actual[i] # Calculate next stock price
    portfolio_value <- num_stocks * stock_price + cash # Calculate new portfolio value
  }
  
  # Plot portfolio value development
  plot(1:length(portfolio_values) , portfolio_values)
  
  # Create returning list:
  return_list[['portfolio_values']] <- portfolio_values
  return_list[['stock_prices']] <- stock_prices
  return_list[['predicted_prices']] <- predicted_prices
  return_list[['number_of_stocks']] <- num_stocks_list
  
  # Return
  return_list
}