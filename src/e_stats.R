# Script that contains various functions concerning the e-statistic

### Load packages
if(!require("pacman")) install.packages("pacman")
pacman::p_load(energy,
               caret,
               rbart)


get_estat <- function(resp, yp) {
  # Get draws
  np <- length(resp[['mmean']])
  pdraw = resp$mdraws + resp$sdraws * matrix(rnorm(nd*np),nrow=nd)
  
  # Get quantiles
  qvec = qsamp(yp,pdraw)
  
  # Calculate statistic
  edist(matrix(c(qvec,runif(10000)),ncol=1),c(np,10000))
  
}

e_stats_cv <- function(x, y, k=5, kappas, method) {
  
  # Initialize folds and output matrix
  folds <- createMultiFolds(y=y, k=k, times=1)

  e_df <- as.data.frame(matrix(ncol=length(kappas), nrow=k))
  colnames(e_df) <- paste(kappas)
  
  # Cycle over all kappas
  for (kappa in kappas) {
    e_stats <- numeric(k)
    
    # Cycle over all folds
    for (i in 1:k) {
      
      # Get the training and validation data for the current fold
      fold <- folds[[i]]

      y_in <- y[fold, ]
      yp <- y[-fold, ]
      x_in <- x[fold, ]
      xp <- x[-fold, ]
      
      # Fit the HBART model
      burn <- 1000
      nd <- 2000
      
      nu=10
      sigq=0.90
      qchi=qchisq(1.0-sigq,nu)
      sighat=sd(y_in)
      lambda=(sighat*sighat*qchi)/nu
      ##run rbart MCMC
      # nskip: burn in draws,
      # ndpost:kept draws,
      # nadapt: initial draws to tune MCMC,
      # numcut: number of cutpoints used for each x
      # k: bigger k gives smoother f
      set.seed(19)
      if (method == "HBART") { # HBART has ntreeh > 1
        res = rbart(x.train=x_in, y.train=y_in, nskip=burn,ndpost=nd,ntreeh = 40,numcut=1000,k=kappa,tc=5, overallsd=sqrt(lambda), overallnu=nu, printevery = TRUE)
      }
      else if (method == "BART") { # BART has ntreeh = 1 and different probabilities of birth and death
        res = rbart(x.train=x_in,y.train=y_in,ntree=200,ntreeh=1,pbd=c(0.7,0.0),nskip=burn,ndpost=nd,numcut=1000,k=kappa,tc=5,overallsd=sqrt(lambda),overallnu=nu)
      }
      
      # Make predictions on the validation set
      resp <- predict.rbart(res, x.test = xp)
      
      # Calculate the e-statistic
      e_stats[i] <- get_estat(resp, yp)
      
      # Print status
      print(paste('Kappa: ', toString(kappa), " Fold: ", toString(i)," e-stat: ",toString(e_stats[i]), sep=''))
    }
    
    # Store e-statistics in column
    column_name <- toString(kappa)
    e_df[[column_name]] <- e_stats
    
  }
  
  # Return e-statistics for every kappa
  e_df
}
