# Analysis of the Used Cars dataset

### Source files and load packages

library(rbart)
library(caret)
library(energy)
library(Metrics)
library(forecast)
library(randomForest)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(xtable)

source("src/qinsamp.R")
source("src/e_stats.R")
source("src/predict_with_xgboost.R")

###############################################################################
###### Loading data and predicting

# Initialization of key params
burn=1000
nd=2000

# Load the dataset
data("ucarprice")
ddf <- ucarprice
p <- ncol(ddf)
y <- ddf[1]
x=rbartModelMatrix(ddf[,2:p])

## Make training/test data set
x=rbartModelMatrix(ddf[,2:p])
set.seed(19) #just for reproducibility
train.index=sample(1:1000,600)
xp=x[-train.index,]
x=x[train.index,]
yp=y[-train.index,]
y=y[train.index,]
p = ncol(x)
n = nrow(x)
np = nrow(xp)


## Cross-validation 
set.seed(27) #just for reproducibility
k <- 5
kappas <- c(0.25, 0.5, 1, 2, 5, 10, 20)

y <- as.matrix(y)
e_stats <- e_stats_cv(x,y,k,kappas,method="HBART")

e_stats.homo <- e_stats_cv(x,y,k,kappas,method="BART")
apply(e_stats, 2, mean)
apply(e_stats.homo, 2, mean)

# Save e-stats
cv_estats <- list('hetero'=e_stats, 'homo'=e_stats.homo)
saveRDS(cv_estats, file="output/used_cars/e_stats_best.RData")

# Create box plots

e <- e_stats.homo
# Set up the plotting area
par(mfrow = c(1, ncol(e[1:ncol(e)])))  # Adjust the layout based on the number of columns

# Loop through each column and create a box plot
for (col in names(e[1:ncol(e)])) {
  boxplot(e[[col]], main = col, ylim = c(0,15), ylab = "Value", col = "lightblue", border = "black")
}

dev.off()

## Getting models
# Variance prior
nu=10
sigq=0.90
qchi=qchisq(1.0-sigq,nu)
sighat=sd(y)
lambda=(sighat*sighat*qchi)/nu

# Now run the heteroscedastic model:
res = rbart(x,y,xp,ntree=200,nskip=burn,ndpost=nd,numcut=1000,k=1.5,tc=5,overallsd=sqrt(lambda),overallnu=nu)
resp = predict.rbart(res,x.test=xp)

# Run the homoscedastic model:
res.homo = rbart(x,y,xp,ntree=200,ntreeh=1,pbd=c(0.7,0.0),nskip=burn,ndpost=nd,numcut=1000,k=0.75,tc=5,overallsd=sqrt(lambda),overallnu=nu)
resp.homo = predict.rbart(res.homo,x.test=xp)

# Run the random forest
rf_model <- randomForest(y ~ ., data=(ddf[train.index,]), importance=TRUE, ntree=500)
rf_predictions <- predict(rf_model, (ddf[-train.index,]))

# Run the XGBoost
set.seed(1913)
xgboost_run <- predict_with_xgboost(as.data.frame(rbartModelMatrix(ddf))[train.index, ], as.data.frame(rbartModelMatrix(ddf))[-train.index, ], 'price')
xgboost_predictions <- xgboost_run$predictions
xgboost_model <- xgboost_run$model

# Save models
used_cars = list()
used_cars[['hetero']] <- list('res'=res, 'resp'=resp)
used_cars[['homo']] <- list('res'=res.homo, 'resp'=resp.homo)
used_cars[['rf']] <- list('model'=rf_model, 'predictions'=rf_predictions)
used_cars[['xgboost']] <- list('model'=xgboost_model, 'predictions'=xgboost_predictions)
used_cars[['y']] <- y
used_cars[['yp']] <- yp

saveRDS(used_cars, 'output/used_cars/models_final.RData')
uc_models <- readRDS('output/used_cars/models_final.RData')
resp <- uc_models$hetero$resp
resp.homo <- uc_models$homo$resp
rf_predictions <- uc_models$rf$predictions
xgboost_predictions <- uc_models$xgboost$predictions

###############################################################################
##### Analysis

### Figure 1: summary of continuous variables conditional on trim.
pdf('output/used_cars/plots/uc-trim.pdf', width = 10, height = 5)
par(mfrow = c(2, 4))
maxprice <- 80000
maxmileage <- 300000
maxyear <- 2015
minyear <- 1994

trim_430 <- ddf[ddf$trim == 430,]
trim_500 <- ddf[ddf$trim == 500,]
trim_550 <- ddf[ddf$trim == 550,]
trim_other <- ddf[ddf$trim == 'other',]

# Row 1
plot(trim_430$mileage, trim_430$price, xlim = c(0, maxmileage), ylim = c(0, maxprice), pch = 19, xlab = "Mileage", ylab = "Price", main = "trim=430", cex.axis = 1.2, cex.lab = 1.2)
plot(trim_500$mileage, trim_500$price, xlim = c(0, maxmileage), ylim = c(0, maxprice), pch = 19, xlab = "Mileage", ylab = "Price", main = "trim=500", cex.axis = 1.2, cex.lab = 1.2)
plot(trim_550$mileage, trim_550$price, xlim = c(0, maxmileage), ylim = c(0, maxprice), pch = 19, xlab = "Mileage", ylab = "Price", main = "trim=550", cex.axis = 1.2, cex.lab = 1.2)
plot(trim_other$mileage, trim_other$price, xlim = c(0, maxmileage), ylim = c(0, maxprice), pch = 19, xlab = "Mileage", ylab = "Price", main = "trim=other", cex.axis = 1.2, cex.lab = 1.2)

# Row 2
plot(trim_430$year, trim_430$price, xlim = c(minyear, maxyear), ylim = c(0, maxprice), pch = 19, xlab = "Year", ylab = "Price", main = "trim=430", cex.axis = 1.2, cex.lab = 1.2)
plot(trim_500$year, trim_500$price, xlim = c(minyear, maxyear), ylim = c(0, maxprice), pch = 19, xlab = "Year", ylab = "Price", main = "trim=500", cex.axis = 1.2, cex.lab = 1.2)
plot(trim_550$year, trim_550$price, xlim = c(minyear, maxyear), ylim = c(0, maxprice), pch = 19, xlab = "Year", ylab = "Price", main = "trim=550", cex.axis = 1.2, cex.lab = 1.2)
plot(trim_other$year, trim_other$price, xlim = c(minyear, maxyear), ylim = c(0, maxprice), pch = 19, xlab = "Year", ylab = "Price", main = "trim=other", cex.axis = 1.2, cex.lab = 1.2)

dev.off()
### Table 4: cross-validation for BART and HBART.

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

### Table 5: evaluation metrics

create_eval_table <- function() { # This way the code can be used for alcohol and fishery too
  # Initalize data frame
  
  metrics <- c("rho", "RMSE", "MAE", "e-stat.")
  methods <- c("HBART", "BART", "RF" ,"XGBoost")
  eval_table <- as.data.frame(matrix(ncol=length(metrics), nrow=length(methods)))
  colnames(eval_table) <- metrics
  rownames(eval_table) <- methods
  
  predictions.all = list('HBART'=resp$mmean, "BART"=resp.homo$mmean, "RF"=rf_predictions, "XGBoost"=xgboost_predictions)
  
  # Cycle over all methods
  for (method in methods) {
    
    # Select predictions
    pred <- predictions.all[[method]]
    
    # Cycle over all metrics
    for (metric in metrics) {
      
      Value <- NA
      
      # Get correct value
      if (metric == "rho") {
        value <- cor(pred, yp)
      } else if (metric =="RMSE") {
        value <- rmse(pred, yp)
      } else if (metric == "MAE") {
        value <- mae(pred, yp) 
      } else if(metric == "e-stat." && method %in% c("BART", "HBART")) {
        if (method=="HBART") {value <- get_estat(resp, yp)}
        if (method=="BART") {value <- get_estat(resp.homo, yp)}
      }
      
      eval_table[method, metric] <- value
    }
  }
  
  # Return LaTeX table
  xtable(eval_table)
}

create_eval_table()

### Figure 2: predictive qq-plots of posterior draws for the 400 out-of-sample predictions of price calibrated to the uniform distribution.

# Get qvec
np <- length(resp[['mmean']])
pdraw = resp$mdraws + resp$sdraws * matrix(rnorm(nd*np),nrow=nd)
qvec = qsamp(yp,pdraw)

np <- length(resp.homo[['mmean']])
pdraw.homo = resp.homo$mdraws + resp.homo$sdraws * matrix(rnorm(nd*np),nrow=nd)
qvec.homo = qsamp(yp,pdraw.homo)

# Plot
pdf('output/used_cars/plots/uc-qqplot.pdf', width=14, height=6)
par(mfrow=c(1,2))
qqplot(qvec,runif(10000),col="grey",cex.lab=1.5,xlab="sample quantile",ylab="uniform",cex.axis=1.5)
abline(0,1,col="black",lwd=3)
#title(main="(a) predictive qqplots, heteroscedastic model",cex.main=1.2)

qqplot(qvec.homo,runif(10000),col="grey",cex.lab=1.5,xlab="sample quantile",ylab="uniform",cex.axis=1.5)
abline(0,1,col="black",lwd=3)
#title(main="(b) predictive qqplots, homoscedastic model",cex.main=1.2)

dev.off()
### Figure 3: H-evidence plot.

pdf('output/used_cars/plots/uc-check-heter.pdf', width=8, height=8)
oo=order(resp$smean)
qm = apply(resp$sdraws,2,quantile,probs=c(.05,.95))
plot(c(resp$smean[oo[1]],resp$smean[oo[np]]),range(qm),type="n",xlab=expression(hat(s)(x)), ylab="s(x) posterior",cex.lab=1.4,cex.axis=1.2)
for(i in 1:np) lines(rep(resp$smean[oo[i]],2),qm[,oo[i]],col="gray")
abline(h=mean(resp.homo$smean[1]),col="black",lwd=3)
abline(h=quantile(resp.homo$sdraws,0.05),col="black",lty=2)
abline(h=quantile(resp.homo$sdraws,0.95),col="black",lty=2)

dev.off()
### Figure 4:  posterior median price for out-of-sample data plotted as a function of year and mileage.

# First figure out which column of the four corresponds to trim == other! 139 is the magic number
table(ucarprice$trim)
xp_copy <- as.data.frame(xp)
x_copy <- as.data.frame(x)

other_indices <- which(xp_copy$trim4 == 1) # Change trim4 to correct one if necessary
other_indices_reverse <- 400 - other_indices

# Divide xp
xp_other <- xp_copy[other_indices,] 
xp_not_other <-xp_copy[-other_indices,] 

# Initialize data frame that will create the plots
plot_df_other <- data.frame( matrix( ncol=4, nrow=nrow(xp_other) ) )
plot_df_not_other <- data.frame( matrix( ncol=4, nrow=nrow(xp_not_other) ) )
colnames(plot_df_other) <- c("year", "mileage", "median_price", "median_sd")
colnames(plot_df_not_other) <- c("year", "mileage", "median_price", "median_sd")

# Extract the median price and standard deviation for both groups
price_other_median <- resp$m.5[other_indices]
sd_other_median <- resp$s.5[other_indices]

price_not_other_median <- resp$m.5[-other_indices]
sd_not_other_median <- resp$s.5[-other_indices]

# Fill the columns for both groups
plot_df_other$year <- xp_other$year
plot_df_other$mileage <- xp_other$mileage
plot_df_other$median_price <- price_other_median
plot_df_other$median_sd <- sd_other_median

plot_df_not_other$year <- xp_not_other$year
plot_df_not_other$mileage <- xp_not_other$mileage
plot_df_not_other$median_price <- price_not_other_median
plot_df_not_other$median_sd <- sd_not_other_median

# Graph the plots
pdf('output/used_cars/plots/ucfigure11_test1.pdf', width=9, height=4.5)
# Find the maximum and minimum values for size and color

# Create the plots
plot1 <- ggplot(plot_df_not_other, aes(x = mileage, y = year)) +
  geom_point(aes(color = median_price, size = median_sd/sqrt(median_price)), alpha = 0.8) +
  scale_color_gradient(low = "lightgrey", high = "black") +
  scale_size(range = c(0.1, 10)) +
  xlim(0, 250000) +
  ylim(1994, 2013) +
  labs(x = "mileage", y = "year") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Turn off the grid
        panel.border = element_rect(color = "black", fill = NA, size = 1, linetype = "solid"),  # Add border
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  # Adjust plot margins
        legend.position = "none",
        aspect.ratio = 1)  # Set plot aspect ratio to make it square

plot2 <- ggplot(plot_df_other, aes(x = mileage, y = year)) +
  geom_point(aes(color = median_price, size = median_sd/sqrt(median_price)), alpha = 0.8) +
  scale_color_gradient(low = "lightgrey", high = "black") +
  scale_size(range = c(0.1, 10)) +
  xlim(0, 250000) +
  ylim(1994, 2013) +
  labs(x = "mileage", y = "year") +
  theme_minimal() +
  theme(panel.grid = element_blank(),  # Turn off the grid
        panel.border = element_rect(color = "black", fill = NA, size = 1, linetype = "solid"),  # Add border
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  # Adjust plot margins
        legend.position = "none",
        aspect.ratio = 1)  # Set plot aspect ratio to make it square

grid.arrange(plot1, plot2, ncol = 2)

dev.off()

