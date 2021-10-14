##### 
# This script will be used to simulate a univaraite 1-VAR model --
# I am not sure how moch flexibility it will require in the begging but got to start somewhere
# It'll be built heavily off of this R package: https://rdrr.io/cran/mlVAR/
# Although other helpful R functions include: https://stat.ethz.ch/R-manual/R-devel/library/stats/html/arima.sim.html

# -- load-library(s) ---------
library(mlVAR) ## Use this to simulate the VAR data (simulateVAR)
library(vars) ## Use this to train the VAR model (VAR ; ar; armia)
library(stats)

# --declare-global-----------

# --sim-VAR-1-1var-----------
## I want to sample a matrix with an eigenvalue less than 1 the same length as the number of time points
num.time <- 100
out.params <- list()
out.eigen <- NULL
for(i in 1:num.time){
  ## Generate a 2x2 matrix.. although this will change as we want to sample more variables
  ## with an eigenvalue less than or equal to 1 
  ## Generate a set of values less than 1 
  rand.samp <- matrix(rnorm(1, mean = .2, sd = .15), ncol=1, nrow=1)
  out.params[[i]] <- rand.samp
  out.eigen <- rbind(out.eigen, eigen(rand.samp)$values)
}
if (sum(range(abs(out.eigen)) < 1) == 2) {
  print("Stationary TS")
}

vals <- simulateVAR(pars=out.params, means = 0, lags = 1, Nt=num.time, residuals = .25)
all.dat <- data.frame(cbind(1:num.time, vals))
colnames(all.dat) <- c("one", "two")

# --sim-VAR-1-2var-----------
## I want to sample a matrix with an eigenvalue less than 1 the same length as the number of time points
num.time <- 100
out.params <- list()
out.eigen <- NULL
for(i in 1:num.time){
  ## Generate a 2x2 matrix.. although this will change as we want to sample more variables
  ## with an eigenvalue less than or equal to 1 
  ## Generate a set of values less than 1 
  rand.samp <- matrix(rnorm(4, mean = .2, sd = .05), ncol=2, nrow=2)
  out.params[[i]] <- rand.samp
  out.eigen <- rbind(out.eigen, eigen(rand.samp)$values)
}
if (sum(range(abs(out.eigen)) < 1) == 2) {
  print("Stationary TS")
}
  
vals <- simulateVAR(pars=out.params, means = 3, lags = 1, Nt=num.time, residuals = .5)
mod <- VAR(vals, lag.max = 5)

# --sim-VAR-1-1var--Stats-package-----------
set.seed(456)
## list description for AR(1) model with small coef
AR_sm <- list(order = c(2, 0, 0), ar = c(.2, .3))
## list description for AR(1) model with large coef
AR_lg <- list(order = c(1, 0, 0), ar = 0.9)
## simulate AR(1)
AR1_sm <- arima.sim(n = 50, model = AR_sm, sd = 0.1)
AR1_lg <- arima.sim(n = 50, model = AR_lg, sd = 0.1)
