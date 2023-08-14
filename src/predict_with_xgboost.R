# Script that can create an XGBoost model and use it to predict

### Load packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(xgboost,
               caret)

predict_with_xgboost <- function(train_data, test_data, target_col, model=NULL) {
  
  # Initialize return list
  return_list = list()
  
  # Extract y data
  train_labels <- train_data[[target_col]]
  test_labels <- test_data[[target_col]]
  
  # Remove the target variable from the training data
  train_data <- train_data[, !(colnames(train_data) %in% target_col)]
  test_data <- test_data[, !(colnames(test_data) %in% target_col)]
  
  # Convert the training and test data to DMatrix format
  dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_labels)
  dtest <- xgb.DMatrix(data = as.matrix(test_data))
  
  # Create a model if desired
  if (is.null(model)) {
  
    # Set up the parameter grid for grid search
    param_grid <- expand.grid(
      nrounds = c(5, 10, 50, 100, 150),
      max_depth = c(2, 5, 10, 15),
      eta = c(0.01, 0.05, 0.1, 0.2),
      gamma = c(0, 0.1, 0.2),
      colsample_bytree = 1,
      min_child_weight = 1,
      subsample = 1
    )
    
    # Define the training control with cross-validation
    train_control <- trainControl(
      method = "cv",
      number = 5,  # Number of cross-validation folds
      search = "grid"
    )
    
    # Perform grid search for hyperparameter tuning
    grid_search <- caret::train(
      x = as.matrix(train_data),
      y = train_labels,
      trControl = train_control,
      method = "xgbTree",
      tuneGrid = param_grid
    )
    
    # Get the best hyperparameters
    best_params <- grid_search$bestTune
    best_params <- as.list(best_params)
    
    # Train the xgboost model with the best hyperparameters
    xgboost_model <- xgboost(data = dtrain, params = best_params, nrounds = best_params$nrounds)
  }
  
  # Predict on the test data using the trained model
  predictions <- predict(xgboost_model, newdata = dtest)
  fitted_values <- predict(xgboost_model, newdata = dtrain)
  
  # Return
  return_list[['model']] <- xgboost_model
  return_list[['predictions']] <- predictions
  return_list[['fitted_values']] <- fitted_values
  return_list[['y']] <- train_labels
  return_list[['yp']] <- test_labels
  
  return_list
}
