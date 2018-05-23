
##Satellite Imagery Workshop 2018 - Practical Session 4 ##

# Start by making sure that your working directory is properly set
# If not you can set it using setwd()

(setwd("~/Documents/R"))
#To check your working directory use getwd
getwd()

## Crop yield State Space Model example - Linear Gaussian Trend Plus Seasonality          
# Code based on package dlm example (https://cran.r-project.org/web/packages/dlm/vignettes/dlm.pdf) #
# Data freely available and extracted from FAO website http://www.fao.org/faostat/en/#data/QC


library(dlm) ## Dynamic Linear Models package (SSMs)


yield.data <-read.csv("/Users/hollowj/Desktop/Satellite Imagery Course/FAO-thailand-crop-data.csv")

plot(yield.data$Value, type = "o",
     xlab = "Year",
     ylab = "Yield value (hg/ha)")

## Let's pretend the natural log of UKGas consumption represents yield over time of some crop:
yield.data <- log(UKgas) 



plot(yield.data, type = "o")

## Fit a structural Time series model to this data with local linear trend 
## plus quarterly seasonality component. See notes for model details.

## Step 1 is to build the model with the built in structures in dlm.
## Default order for dlmModPoly = 2, which represents a stochastic linear trend (mean plus slope)

yield.mod <- dlmModPoly() + dlmModSeas(4) 
yield.mod

# $FF is the observation equation matrix (column 1 is trend, column 3 is seasonal component)

# $V is observation equation error var-covar (needs to be estimated, see below)

# $GG is the state transition matrix:
# (5 states: see model details in notes and note "sum to zero" seasonality constraints)

# $W is the state equation var-covar matrix 
# (trend level has zero var, slope and seasonality have error - vars need to be estimated, see below)

# $m0 and $C0 are t = 0 starting values for Gaussian states' mean vector and var-covar matrix


## Next we must create a function involving unknown parameters to be estimated via max. likelihood.
## In this case these paramters are the unknown variances in the model:

estimFun <- function(x) {
  diag(W(yield.mod))[2:3] <- exp(x[1:2])
  V(yield.mod) <- exp(x[3])
  return(yield.mod)
}

## Note since the parameters are variances, parameterise as exp() for unconstrained optimisation.
## Now use dlmMLE to obtain MLEs. Uses R's built in 'optim' function:

fit <- dlmMLE(yield.data, parm = rep(0.1, 3), build = estimFun) # parm = rep(0.1,3) are starting vals

## Check convergence:

fit$conv # = 0, so OK.

## Now add these estimates back into SSM model specification:

yield.mod <- estimFun(fit$par)

## And check the var-covars have been updated:

yield.mod$V; yield.mod$W

## Now we can obtain smoothed estimates of the states using the fitted model, and decompose the series 
## into a smooth trend + stochastic seasonal component (plus error).

## dlmSmooth uses the Kalman Smoother algorithm to get smoothed values of the states plus var-covar matrices.
## It returns a time series of smoothed state vectors (object$s), plus a SVD representation of the var-cov matrices.
## Note that the smoothed series begins one time unit before the first data observation, so we need to remove it for graph:

yield.smooth <- dlmSmooth(yield.dat, mod = yield.mod)

## Create graph showing original series decomposed into smoothed trend and seasonal components:

x <- cbind(yield.dat, dropFirst(yield.smooth$s[,c(1,3)])) # note we drop the first row (see above)

colnames(x) <- c("Yield", "Trend", "Seasonality")

plot(x, main = "Pretend Yield Data: Smoothed Trend and Seasonal Components")


## We can also predict future values of the series based on the model.
## Let's forecast 3 years ahead (quarterly, so that's 12 time steps) to 1989 Q4.

## [[First rule of Forecasting Club: Extrapolation is dangerous! Second rule: See rule 1....]]

## dlmFilter uses the Kalman Filter to compute filtered values of the states and var-covar matrices
## via SVD. Note that the filtered series begins one time unit before the first data observation.

yield.filter <- dlmFilter(yield.dat, mod = yield.mod)

## dlmForecast calculates the expected value and variances of future observations and states:

yield.forecast <- dlmForecast(yield.filter, nAhead = 12) 

## We are going to forecast the deseasonalised series (ie just trend), and we need
## the trend std.deviations to create prediction intervals. 
## dlmForecast produces a list object of var-covar matrices, R 
## (each element corresponding to the forecast time). We can access the trend std. dev.
## via:

std.devs <- sapply(yield.forecast$R, function(x) sqrt(x[1,1])) 
std.devs
## Note these getting larger the futher into the future we are predicting, as expected.

## Create the upper and lower 95% prediction intervals (everything is assumed Gaussian):

fm <- yield.forecast$a[,1] # forecasted trend means (deseasonalised forecast)

pu <- fm + qnorm(0.975, sd = std.devs)
pl <- fm + qnorm(0.025, sd = std.devs)

## Combine original data, it's smoothed (deseasonalised) values, the forecasts, and prediction intervals 
## into a single time series object for plotting. ts.union will pad non-overlapping times with 
## NAs and window selects a subset of a time series between a specified start and end time.
## We'll start the plot of the original and smoothed series at 1980 Q1:

x <- ts.union(window(yield.dat, start = c(1980, 1)),           # Data
              window(yield.smooth$s[,1], start = c(1980, 1)),  # Smoothed trend
              yield.forecast$a[,1],                            # Forecast trend
              pu, pl)                                          # upper and lower 95% prediction intervals for trend
x

## Now plot on a single graph:

plot(x, plot.type = "single", type = "o", pch = c(1, 0, 15, 4, 4), 
     col = c("black", "black", "red", "blue", "blue"),
     ylab = "Pretend Yield")

legend("bottomleft", legend = c("Observed",
                                "Smoothed (deseasonalised)",
                                "Forecasted (deseasonalised)", 
                                "95% Forecast Interval"),
       bty = 'n', pch = c(1, 0, 15, 4, 4), lty = 1,
       col = c("black", "black", "red", "blue", "blue"))

