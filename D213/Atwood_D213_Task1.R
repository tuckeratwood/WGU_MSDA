# imports data set
df <- read.csv("C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/teleco_time_series.csv")

# imports packages to be used in analysis
library(tseries) # using tseries package to utilize adf.test function
library(astsa) # using astsa package to utilize mvspec function
library(forecast) # using forecast package to utilize auto.arima and Arima functions

# converts data into a time series
ts <- ts(df$Revenue, start = 1, frequency = 30)

# creates time series plot, no noticeable outliers, trend: upward linear (not stationary)
plot(ts, xlab = "Day", ylab = "Revenue", main = "Time Series: Day vs Revenue")

# checks for missing values (none)
sum(is.na(df))

# checks structure of time series and checks for duplicate day values
str(df)
start(df$Day)
end(df$Day)
sum(duplicated(df$Day))

# runs augmented Dickey-Fuller hypothesis test
adf.test(ts)

# creates differenced time series
diff_ts <- diff(ts)
plot(diff_ts, xlab = "Day", ylab = "Difference", main = "Difference Plot: Day vs Difference")

# re-runs ADF test on differenced data
adf.test(diff_ts)

# creates the training and testing data sets (80-20)
train <- ts(df[1:585,2])
test <- ts(df[586:731,2], start = 586)

# writes CSVs of cleaned time series data, training set, and test set
write.csv(ts, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/clean_ts.csv")
write.csv(diff_ts, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/diff_ts.csv")
write.csv(train, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/training_set.csv")
write.csv(test, "C:/Users/atwoo/OneDrive/Desktop/MSDADirectory/test_set.csv")

# decomposes original data
decomp <- decompose(ts)
plot(decomp, xlab = "Months")

# spectral density plot indicates no seasonality
mvspec(ts, main = "Spectral Density Plot")

# autocorrelation function and partial autocorrelation function of differenced data
par(mar=c(5,4,4,2) + 0.1)
acf(ts, main = "Autocorrelation of Original Data")
pacf(ts, main = "Partial Autocorrelation of Original")
acf(diff_ts, main = "Autocorrelation of Differenced Data")
pacf(diff_ts, main = "Partial Autocorrelation of Differenced Data")

# finds optimal ARIMA model
auto.arima(train)

# compares auto.arima's optimal order (1,1,0) to a few other ARIMA models
AM1 <- Arima(train, order = c(1,1,0), include.drift = TRUE)
AM2 <- Arima(train, order = c(0,1,0), include.drift = TRUE)
AM3 <- Arima(train, order = c(2,1,0), include.drift = TRUE)
AM4 <- Arima(train, order = c(1,1,1), include.drift = TRUE)
AM5 <- Arima(train, order = c(1,0,0), include.drift = TRUE)
AM6 <- Arima(train, order = c(0,1,1), include.drift = TRUE)
aic_vals <- data.frame(Arima_Order = c("1,1,0","0,1,0","2,1,0","1,1,1","1,0,0","0,1,1"), 
                       AIC_value = c(AM1$aic, AM2$aic, AM3$aic, AM4$aic, AM5$aic, AM6$aic))
aic_vals

# summary of optimal ARIMA model, including evaluation metrics
summary(AM1)

# residual plot of optimal ARIMA
res <- residuals(AM1)
plot(res, main = "Residuals of Optimal Arima Model (1,1,0) with Drift", ylab = "Residuals")

# plots training set, forecast data, forecast confidence interval, and test set
fc <- forecast(AM1, h = 146)
plot(fc, col = 2, lwd = 4, xlab = "Day", ylab = "Revenue", main = "Forecast of Revenue Using Optimal ARIMA Model")
points(test, type = "l", col = 3, lwd = 4)
legend("topleft", legend = c("Training","Test","Forecast"), col = c(2,3,4), lty = 1, lwd = 4)

# RMSE of test set
residuals <- test - fc$mean
sqrt(mean(residuals^2))
