# https://viz.datascience.arizona.edu/2021-time-series-intro/time-series-forecasting.html

# libraries:
library(forecast)
library(dplyr)
library(lubridate)
library(broom)
library(daymetr)
# consistent results from random number generator
set.seed(103) 

#Now we can simulate and plot the growth a fish:
days <- 0:9
mass <- vector(length = 10)
for( t in seq_along(days)){
  mass[t] <- 1 + 2* days[t] + rnorm(1, 0, 1)
}
linear_data <- data.frame(day = days, mass = mass)
plot(mass~day, 
    data = linear_data, 
    ylab = 'mass (g)', 
    ylim = c(1, 20),
    xlim = c(0, 15))

mod_linear <- lm(mass ~ 1 + days, data = linear_data)
tidy(mod_linear)

plot(mod_linear)

n_sims <- 1000
sim_data <- list()
fits <- list()
days
n <- length(days)

for (i in 1:n_sims){
  sim_data[[i]] <- vector(mode = 'double', length = n)
  for(t in seq_along(days)){
    mass[t] <- 1 + 2 * days[t] + rnorm(1, 0, 1)
  }
  sim_data[[i]] <- mass
  fits[[i]] <- lm(mass ~ 1 + days)
}

v <- vector(length = n_sims)
stats <- data.frame(intercept = v, slope = v, sigma = v, p_i = v, p_s = v)
for(i in 1:n_sims){
  stats[i, c('intercept', 'slope')] <- coef(fits[[i]])
  stats[i, 'sigma']                 <- sigma(fits[[i]])
  stats[i, c('p_i', 'p_s')]         <- summary(fits[[i]])$coefficients[1:2, 4]
}

data.frame(slope = c(mean(stats$slope), sd(stats$slope)), 
           intercept = c(mean(stats$intercept), sd(stats$intercept)), 
           row.names = c('mean', 'sd'))

# frequency of significant intercept:
sum(stats$p_i > 0.05)/nrow(stats)

# frequency of significant slope:
sum(stats$p_s > 0.05)/nrow(stats)

hist(stats$slope)
hist(stats$intercept)


mod_linear
class(mod_linear)

newdays <- 11:15
newdat <- data.frame(days = newdays)
preds <- predict(mod_linear, 
                 newdata = newdat)
## approx. equivalent to:
# preds <- 1 + 2 * newdays

plot(mass ~ day, 
     data = linear_data, 
     ylab = 'mass (g)', 
     ylim = c(0, 30),
     xlim = c(0, 15)) +
  points(newdays, preds, col = 'red') +
  abline(coef(mod_linear)) + 
  abline(confint(mod_linear)[,1], lty = 2) +
  abline(confint(mod_linear)[,2], lty = 2)


# Create time series data
set.seed(210)
months <- 1:240

# 1. just noise
noise <- rnorm(length(months))

# 2. lag of one step
lag   <- vector() 
for(t in 1:length(months)){
  if(t == 1){
    lag[t]  <- rnorm(1)
  } else {
    lag[t]  <- (lag[t-1] + noise[t]) / 2
  }
}

# 3. lag of one step + trend, slope of 1/48 per month
lag_trend <- lag + months / 48

# 4. seasonal patterns
seasonal <- 2*sin(2*pi*months/12) + noise

# 5. seasonal patterns + trend, slope of 1/48 per month
seasonal_trend <- seasonal + months / 48 

all <- ts(data = data.frame(noise, lag, lag_trend, seasonal, seasonal_trend), 
          start = 1982,
          end = 2001,
          frequency = 12)

plot(all)
lag.plot(all, set.lags = c(1, 3, 6, 9))

# Autocorrelation
acf(all[,'noise'], xlab = 'Lag (years), noise')
acf(all[,'lag'], xlab = 'Lag (years), lag 1')
acf(all[,'lag'], xlab = 'Lag (years), lag 1 and trend')
acf(all[,'seasonal'], xlab = 'Lag (years), seasonal')

# Time Series Decomposition
dec <- decompose(all[,'seasonal_trend'])

plot(dec)

plot(dec$trend)

# Can apply smoothing aka Seasonal Trend w/ local smoothing Loess (STL)
seasonal_stl <- stl(all[,'seasonal_trend'], s.window = 6)
plot(seasonal_stl)

## note how you can access each component of the decomposed time series 
## plot(seasonal_stl$time.series[,c('trend', 'seasonal', 'remainder')])

dec_months <- months[1:length(dec$trend)]
tidy(lm(dec$trend ~ dec_months))

plot(dec$trend)

seasonal_stl <- stl(all[,'seasonal_trend'], s.window = 6)
plot(seasonal_stl)

fit <- lm(seasonal_stl$time.series[,'trend'] ~ dec_months)
coef(fit)

# Forecasting
ts_fit <- tslm(all[,'seasonal_trend'] ~ trend + season)
tidy(ts_fit)

plot(forecast(ts_fit, h = 20))

auto_fit <- auto.arima(all[,'seasonal_trend'])
summary(auto_fit)
