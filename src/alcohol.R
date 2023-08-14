# Analysis of the Alcohol dataset.

#|=== Directory setup
path = dirname(rstudioapi::getSourceEditorContext()$path)
path <- substr(path, 1, nchar(path) - 4)
setwd(path)

### Source files and load packages
chooseCRANmirror(graphics = FALSE)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(rbart,
               coda,
               randomForest,
               Metrics,
               forecast,
               xtable)

source("src/qinsamp.R")
source("src/e_stats.R")
source("src/used_car_prices")

###############################################################################
##### Loading data and predicting

# initialization of key params
burn=1000
nd=2000

# load the dataset
ddf = read.csv("data/raw/Pratola/Alcohol-Bart.csv")
p=ncol(ddf)
y=ddf[p]
x=rbartModelMatrix(ddf[,1:(p-1)])

# Make training/test sets.
set.seed(99) #just for reproducibility
train.index=sample(1:2462,1477)
xp=x[-train.index,]
x=x[train.index,]
yp=y[-train.index,]
y=y[train.index,]
p = ncol(x)
n = nrow(x)
np = nrow(xp)

## Cross-validation
set.seed(27)
k <- 5
kappas <- c(0.5,1,2,3,4,5,6,7,8,9,10,15,20, 25, 30)

e_stats <- e_stats_cv(x,y,k,kappas,method="HBART")
e_stats.homo <- e_stats_cv(x,y,k,kappas,method="BART")

e_stats_cv <- list()
e_stats_cv[['hetero']] <- e_stats
e_stats_cv[['homo']] <- e_stats.homo

saveRDS(e_stats_cv, file="output/alcohol/e_statistics_cv.Rdata")

# Create box plots
e <- e_stats.homo
# Set up the plotting area
par(mfrow = c(1, ncol(e[1:ncol(e)])))  # Adjust the layout based on the number of columns

# Loop through each column and create a box plot
for (col in names(e[1:ncol(e)])) {
  boxplot(e[[col]], main = col, ylim = c(0,15), ylab = "Value", col = "lightblue", border = "black")
}

# Variance prior
nu=3
sigq=0.90
qchi=qchisq(1.0-sigq,nu)
sighat=sd(y)
lambda=(sighat*sighat*qchi)/nu


# Now run the heteroscedastic model:
res = rbart(x,y,ntree=200,nskip=burn,ndpost=nd,numcut=1000,k=5,tc=5,overallsd=sqrt(lambda),overallnu=nu)
resp = predict.rbart(res,x.test=xp)

# Run the homoscedastic model:
res.homo = rbart(x,y,ntree=200,ntreeh=1,pbd=c(0.7,0.0),nskip=burn,ndpost=nd,numcut=1000,k=2,tc=5,overallsd=sqrt(lambda),overallnu=nu)
resp.homo = predict.rbart(res.homo,x.test=xp)

# Run the random forest
rf_model <- randomForest(y ~ ., data=ddf[train.index,], importance=TRUE, ntree=500)
rf_predictions <- predict(rf_model, ddf[-train.index,])

# Run the XGBoost
set.seed(1913)
xgboost_run <- predict_with_xgboost(as.data.frame(rbartModelMatrix(ddf))[train.index, ], as.data.frame(rbartModelMatrix(ddf))[-train.index, ], 'y')
xgboost_predictions <- xgboost_run$predictions
xgboost_model <- xgboost_run$model

# Save models
alcohol = list()
alcohol[['hetero']] <- list('res'=res, 'resp'=resp)
alcohol[['homo']] <- list('res'=res.homo, 'resp'=resp.homo)
alcohol[['rf']] <- list('model'=rf_model, 'predictions'=rf_predictions)
alcohol[['xgboost']] <- list('model'=xgboost_model, 'predictions'=xgboost_predictions)
alcohol[['y']] <- y
alcohol[['yp']] <- yp

saveRDS(alcohol, 'output/alcohol/models_final.RData')
alcohol_models <- readRDS('output/alcohol/models_better.RData')
resp <- alcohol_models$hetero$resp
res <- alcohol_models$homo$resp
rf_predictions <- alcohol_models$rf$predictions
xgboost_predictions <- alcohol_models$xgboost$predictions

###############################################################################
##### Analysis

# Table 7: evaluation metrics.
create_eval_table()

## Figure 7: predictive qq-plots of posterior draws for the 985 out-of-sample predictions of alcohol use calibrated to the uniform distribution.

# Get qvec
np <- length(resp[['mmean']])
pdraw = resp$mdraws + resp$sdraws * matrix(rnorm(nd*np),nrow=nd)
qvec = qsamp(yp,pdraw)

np <- length(resp.homo[['mmean']])
pdraw.homo = resp.homo$mdraws + resp.homo$sdraws * matrix(rnorm(nd*np),nrow=nd)
qvec.homo = qsamp(yp,pdraw.homo)

# Plot
pdf("output/alcohol/plots/alcohol-qq-plot.pdf",width=14,height=6)
par(mfrow=c(1,2))
qqplot(qvec,runif(10000),col="grey",cex.lab=1.5,xlab="sample quantile",ylab="uniform",cex.axis=1.5)
abline(0,1,col="black",lwd=3)
#title(main="(a) predictive qqplots, heteroscedastic model",cex.main=1.2)

qqplot(qvec.homo,runif(10000),col="grey",cex.lab=1.5,xlab="sample quantile",ylab="uniform",cex.axis=1.5)
abline(0,1,col="black",lwd=3)
#title(main="(b) predictive qqplots, homoscedastic model",cex.main=1.2)

dev.off()
## Figure 8: H-evidence plot.

pdf("output/alcohol/plots/alcohol-check-heter.pdf",width=8,height=8)
oo=order(resp$smean)
qm = apply(resp$sdraws,2,quantile,probs=c(.05,.95))
plot(c(resp$smean[oo[1]],resp$smean[oo[np]]),range(qm),type="n",xlab=expression(hat(s)(x)), ylab="s(x) posterior",cex.lab=1.4,cex.axis=1.2)
for(i in 1:np) lines(rep(resp$smean[oo[i]],2),qm[,oo[i]],col="gray")
abline(h=mean(resp.homo$smean[1]),col="black",lwd=3)
abline(h=quantile(resp.homo$sdraws,0.05),col="black",lty=2)
abline(h=quantile(resp.homo$sdraws,0.95),col="black",lty=2)

dev.off()








