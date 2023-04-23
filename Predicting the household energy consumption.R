library(forecast)
library(zoo)
energy_daily <- read.csv("C:\\Users\\STSC\\Desktop\\Time_Series(2_1)\\project\\daily energy.csv")


#Testing predictability
energy.ts <- ts(energy_daily$Energy, start = 1, freq = 7)
energy.ts

energy.ar1<- Arima(energy.ts, order = c(1,0,0))
summary(energy.ar1)

# Apply z-test to test the null hypothesis that beta 
# coefficient of AR(1) is equal to 1.
ar1 <- 0.6194
s.e. <- 0.0290
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis and the data is predictable"
} else {
  "Accept null hypothesis and data is not predictable"
}

plot(energy.ts, 
     xlab = "Time", ylab = "Energy consumption", 
     ylim = c(0, 40), xaxt = 'n',
     main = "Energy consumption")

axis(1, at = seq(1, 730, 1), labels = format(seq(1, 730, 1)))

#Time series components
energy.stl <- stl(energy.ts, s.window = "periodic")
autoplot(energy.stl, main = "Energy Consumption Time Series Components")

autocor <- Acf(energy.ts, lag.max = 12, 
               main = "Autocorrelation for Energy Consumption")


nValid <- 210
nTrain <- length(energy.ts) - nValid 
nTrain
train.ts <- window(energy.ts, start = c(1, 1), end = c(1, nTrain))
valid.ts <- window(energy.ts, start = c(1, nTrain + 1), 
                   end = c(1, nTrain + nValid))
length(train.ts)
length(valid.ts)


## Part b
ma.trailing_4 <- rollmean(train.ts, k = 4, align = "right")
ma.trailing_7 <- rollmean(train.ts, k = 7, align = "right")
ma.trailing_10 <- rollmean(train.ts, k = 10, align = "right")

##Part c
ma.trail_4.pred <- forecast(ma.trailing_4, h = nValid, level = 0)
ma.trail_4.pred
ma.trail_7.pred <- forecast(ma.trailing_7, h = nValid, level = 0)
ma.trail_7.pred
ma.trail_10.pred <- forecast(ma.trailing_10, h = nValid, level = 0)
ma.trail_10.pred

## Part d
round(accuracy(ma.trail_4.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_7.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_10.pred$mean, valid.ts), 3)


trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)

trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred



plot(energy.ts, 
     xlab = "Time", ylab = "Energy Consumption", ylim = c(0, 40), 
     bty = "l", xlim = c(1, 120), xaxt = "n",
     main = "Regression Forecast in Training and Validation Partitions ") 
axis(1, at = seq(1, 120, 1), labels = format(seq(1, 120, 1)))
lines(trend.seas$fitted, col = "blue", lwd = 2, lty = 1)
lines(trend.seas.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(70,35, legend = c("Energy Consumption Data", 
                             "Regression Forecast, Training Partition", 
                             "Regression Forecast, Validation Partition"), 
       col = c("black", "blue", "blue"), 
       lty = c(1, 1, 2), lwd =c(1, 2, 2), bty = "n")

# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(75, 75), c(0, 40))
lines(c(105, 105), c(0, 40))
text(35, 41, "Training")
text(89, 41, "Validation")
text(107, 41, "Future")
arrows(1, 39, 74.9, 39, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(75, 39, 104.9, 39, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(105, 39, 119, 39, code = 3, length = 0.1,
       lwd = 1, angle = 30)



trend.seas.res <- trend.seas$residuals
trend.seas.res
ma.trail.res <- rollmean(trend.seas.res, k = 10, align = "right")
ma.trail.res
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred


plot(trend.seas.res, 
     xlab = "Time", ylab = "Energy Consumption", ylim = c(-20, 30), 
     bty = "l", xlim = c(1, 120), xaxt = "n",
     main = "Regression Forecast in Training and Validation Partitions ") 
axis(1, at = seq(1, 120, 1), labels = format(seq(1, 120, 1)))
lines(trend.seas.res.valid, col = "brown", lwd = 2, lty = 2)
lines(ma.trail.res, col = "blue", lwd = 2, lty = 1)
lines(ma.trail.res.pred$mean, col = "blue", lwd = 2, lty = 2)
legend(70,35, legend = c("Regression Residuals, Training Partition", 
                             "Regression Residuals, Validation Partition",
                             "MA Forecast (k=10), Training Partition", 
                             "MA forecast (k=10), Validation Partition"), 
       col = c("brown", "brown", "blue", "blue"), 
       lty = c(1, 2, 1, 2), lwd =c(2, 2, 2, 2), bty = "n")


# Plot on chart vertical lines and horizontal arrows describing
# training, validation, and future prediction intervals.
lines(c(75, 75), c(-20, 30))
lines(c(105, 105), c(-20, 30))
text(37, 24, "Training")
text(91, 24, "Validation")
text(115, 24, "Future")
arrows(1, 25, 74.9, 25, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(75, 25, 104.9, 25, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(105, 25, 119, 25, code = 3, length = 0.1,
       lwd = 1, angle = 30)

# Part c
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(valid.df) <- c("Energy  consumptions", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(ma.trail.res.pred$mean,valid.ts),3)
round(accuracy(fst.2level, valid.ts), 3)



# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(energy.ts ~ trend  + season)
summary(tot.trend.seas)

# Create regression forecast for future 8 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 8, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 10, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 8 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 8, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 8 periods by combining 
# regression forecast and trailing MA for residuals for future 8 periods
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
future12.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                                tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df


round(accuracy(tot.trend.seas.pred$fitted, energy.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, energy.ts), 3)
round(accuracy((naive(energy.ts))$fitted, energy.ts), 3)
round(accuracy((snaive(energy.ts))$fitted, energy.ts), 3)


# Create Holt-Winter's (HW) exponential smoothing for partitioned data.
# Use ets() function with model = "ZZZ", i.e., automatic selection of
# error, trend, and seasonality options.
# Use optimal alpha, beta, & gamma to fit HW over the training period.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ
# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

## HW model using automated parameters for the whole data set
HW.ZZZ <- ets(energy.ts, model = "ZZZ")
HW.ZZZ 
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 8 , level = 0)
HW.ZZZ.pred



round(accuracy(HW.ZZZ.pred$fitted, energy.ts), 3)
round(accuracy((naive(energy.ts))$fitted, energy.ts), 3)
round(accuracy((snaive(energy.ts))$fitted, energy.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted, energy.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, energy.ts), 3)

## (1) LINEAR TREND MODEL.
# Use tslm() function to create linear trend model.
train.lin <- tslm(train.ts ~ trend)

# See summary of quadratic trend model and associated parameters.
summary(train.lin)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.lin.pred <- forecast(train.lin, h = nValid, level = 0)
train.lin.pred
## (2) QUADRATIC TREND MODEL.
# Use tslm() function to create quadratic (polynomial) trend model.
train.quad <- tslm(train.ts ~ trend + I(trend^2))

# See summary of quadratic trend model and associated parameters.
summary(train.quad)

# Apply forecast() function to make predictions for ts data in
# validation set.  
train.quad.pred <- forecast(train.quad, h = nValid, level = 0)
train.quad.pred


## (3) SEASONALITY MODEL.
# Use tslm() function to create seasonal model.
train.season <- tslm(train.ts ~ season)

# See summary of seasonal model and associated parameters.
summary(train.season)

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
train.season.pred <- forecast(train.season, h = nValid, level = 0)
train.season.pred


## (4) LINEAR TREND AND SEASONALITY MODEL.
# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lin.trend.season.pred <- 
  forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred


## (5) QUADRATIC TREND AND SEASONALITY MODEL.
# Use tslm() function to create quadratic trend and seasonal model.
train.quad.trend.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.trend.season.pred <- 
  forecast(train.quad.trend.season, h = nValid, level = 0)
train.quad.trend.season.pred
# Use accuracy() function to identify common accuracy measures
# for the developed forecast in the validation period.
round(accuracy(train.lin.pred$mean, valid.ts),3)
round(accuracy(train.quad.pred$mean, valid.ts),3)
round(accuracy(train.season.pred$mean, valid.ts),3)
round(accuracy(train.lin.trend.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.trend.season.pred$mean, valid.ts),3)
# FIT REGRESSION MODEL WITH QUADRATIC TREND AND SEASONALITY, 
# REGRESSION MODEL WITH SEASONALITY,AND LINEAR TREND FOR ENTIRE DATASET. 
# FORECASTDATA, AND MEASURE ACCURACY.

##LINEAR TREND AND SEASONALITY MODEL

# Use tslm() function to create linear trend model.
lin.trend.season <- tslm(energy.ts ~ trend + season)

# See summary of linear trend equation and associated parameters.
summary(lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
lin.trend.season.pred <- forecast(lin.trend.season, h = 8, level = 0)
lin.trend.season.pred

# Apply forecast() function to make predictions for ts with 
# seasonality data in validation set.  
season.pred <- forecast(season, h = 8, level = 0)
season.pred

##LINEAR TREND
# Use tslm() function to create linear trend model.
lin.trend <- tslm(energy.ts ~ trend)

# See summary of linear trend equation and associated parameters.
summary(lin.trend)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in the future 12 months.  
lin.trend.pred <- forecast(lin.trend, h = 8, level = 0)
lin.trend.pred
##ACCURACY OF ALL 3 MODELS WITH NAIVE AND SEASONAL NAIVE.
round(accuracy(lin.trend.season.pred$fitted, energy.ts),3)
round(accuracy(lin.trend.pred$fitted, energy.ts),3)
round(accuracy((naive(energy.ts))$fitted, energy.ts), 3)
round(accuracy((snaive(energy.ts))$fitted, energy.ts), 3)

# Use tslm() function to create linear trend and seasonal model.
train.lin.trend.season <- tslm(train.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.trend.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.lin.trend.season.pred <- 
  forecast(train.lin.trend.season, h = nValid, level = 0)
train.lin.trend.season.pred

# Use Acf() function to identify autocorrealtion for the model residuals 
# (training set), and plot autocorrelation for different lags.
Acf(train.lin.trend.season.pred$residuals, lag.max = 8, 
    main = "Autocorrelation for Energy consumption Training Residuals")

# Use Arima() function to fit AR(1) model for training residuals. 
# The Arima model of order = c(1,0,0) gives an AR(1) model.
# Use summary() to identify parameters of AR(1) model. 
r.ar1 <- Arima(train.lin.trend.season$residuals, order = c(1,0,0))

# Use forecast() function to make prediction of residuals in validation set.
r.ar1.pred <- forecast(r.ar1, h = nValid, level = 0)
r.ar1.pred

# Use Acf() function to identify autocorrealtion for the training 
# residual of residuals and plot autocorrelation for different lags 
# (up to maximum of 12).
Acf(r.ar1$residuals, lag.max = 8, 
    main = 
      "Autocorrelation for Energy consumption Training Residuals of Residuals")

# Create two-level modeling results, regression + AR(1) for validation period.
# Create data table with historical validation data, regression forecast
# for validation period, AR(1) for validation, and and two-level model results. 
valid.two.level.pred <- train.lin.trend.season.pred$mean + r.ar1.pred$mean
valid.df <- data.frame(valid.ts, train.lin.trend.season.pred$mean, 
                       r.ar1.pred$mean, valid.two.level.pred)
names(valid.df) <- c("Valid.Energy", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid.df

# Use tslm() function to create linear trend and seasonal model.
linear.trend.season <- tslm(energy.ts ~ trend  + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(linear.trend.season)

#Apply forecast() function to make predictions with linear trend and seasonal model into the future 8 quarters.
linear.trend.season.pred <- 
  forecast(linear.trend.season, h = 8, level = 0)
linear.trend.season.pred

# Using Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.
# Use forecast() function to make prediction of residuals into 
# the future 5 quarters.
residuals.ar1 <- Arima(linear.trend.season$residuals, order = c(1,0,0))
residuals.ar1.pred <- forecast(residuals.ar1, h = 8, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residuals.ar1)

# Use Acf() function to identify autocorrelation for the residual of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residuals.ar1$residuals, lag.max = 8, 
    main = 
      "Autocorrelation for AR(1) Model Residuals for Entire Data Set")

# Identifying the  two-level forecast for the 8 future periods 
# as sum of linear trend and seasonal model and AR(1) model for residuals.
trend.season.ar1.pred <- linear.trend.season.pred$mean + residuals.ar1.pred$mean
trend.season.ar1.pred

# Creating a data table with linear trend and seasonal forecast for 8 future periods,
# AR(1) model for residuals for 8 future periods, and combined two-level forecast
datatables.df <- data.frame(linear.trend.season.pred$mean, 
                            residuals.ar1.pred$mean, trend.season.ar1.pred)
names(datatables.df) <- c("Reg.Forecast", "AR(1)Forecast","Combined.Forecast")
datatables.df

# Use Arima() function to fit ARIMA(1,1,1)(1,1,1) model for trend and seasonality.
# Use summary() to show ARIMA model and its parameters.
train.arima <- Arima(train.ts, order = c(1,1,1), seasonal = c(1,1,1))
summary(train.arima)
train.arima.pred <- forecast(train.arima, h = nValid, level = 0)
train.arima.pred

# Utilizing auto.arima() function to automatically identify 
# the ARIMA model structure and parameters. 
# Develop the ARIMA forecast for the validation period. 
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)
train.auto.arima.pred <- forecast(train.auto.arima, h = nValid, level = 0)
train.auto.arima.pred

# Accuracy measures for the two ARIMA models in questions 3a and 3b.
round(accuracy(train.arima.pred$mean, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)

# Using the  Arima() function to fit ARIMA(1,1,1)(1,1,1) model for the entire data set.
# Applying forecast for the 8 periods in the future. 
entire.arima <- Arima(energy.ts, order = c(1,1,1), seasonal = c(1,1,1))
summary(entire.arima)
entire.arima.pred <- forecast(entire.arima, h = 8, level = 0)
entire.arima.pred

# Using the auto.arima() function for the entire data set..
# Using the summary() to show ARIMA model and its parameters.
# Applying the forecast for the 8 periods in the future. 
auto.arima <- auto.arima(energy.ts)
summary(auto.arima)
auto.arima.pred <- forecast(auto.arima, h = 8, level = 0)
auto.arima.pred



# Using the accuracy() function to identify common accuracy measures for:
# (1) regression model with linear trend and seasonality
# (2) Two-level model (regression model + AR(1) for regression residuals)
# (3) ARIMA(1,1,1)(1,1,1)
# (4) Auto ARIMA model
# (5) Seasonal naive forecast.
round(accuracy(linear.trend.season$fitted, energy.ts), 3)
round(accuracy(linear.trend.season$fitted + residuals.ar1$fitted, energy.ts), 3)
round(accuracy(entire.arima.pred$fitted, energy.ts), 3)
round(accuracy(auto.arima.pred$fitted, energy.ts), 3)
round(accuracy((snaive(energy.ts))$fitted, energy.ts), 3)


