### Time Series - Block Bootstrapping

# packages
library(quantmod)
library(magrittr)
library(purrr)
library(lubridate)

# parameter
ticker <- "BMW.DE"

# get data 
data <- getSymbols(ticker, auto.assign=FALSE)
V <- Ad(data)
V_T <- V[length(V)]
v <- na.omit(ROC(V))

### simulation using resampling

n_resample <- 5 # number of returns to resample per draw
n_parts <- 50 # amount of resample draws
n_total <- n_resample * n_parts

future_ret <- vector()
for(i in 1:n_parts) { 
  start <- ceiling(runif(1) * (length(v)-n_resample))
  future_ret <- c(future_ret, v[start:(start+n_resample-1)]) 
}

# convert sampled returns into prices
V_forecast <- as.numeric(V_T) * cumprod(1+future_ret)
Forecast <- as.xts(V_forecast, 
                   order.by=as.Date(unlist(c(0:(n_total-1)) %>% map(function(x) today()+x))))

# plot results
op <- par(no.readonly = TRUE)
par(mfrow=c(1,2))
plot(V[(length(V)-n_total):length(V)], main=ticker)
plot(Forecast, ylab="", type="l")
par(op)
