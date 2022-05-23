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
