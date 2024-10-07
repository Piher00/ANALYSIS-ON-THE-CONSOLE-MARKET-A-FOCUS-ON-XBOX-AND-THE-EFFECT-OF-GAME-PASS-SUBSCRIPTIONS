library(forecast)
library(ggplot2)
library(DIMORA)
library(readxl)
library(lmtest)
library(sm)

data <- read_excel("month_sales.xlsx")
data <- data[!is.na(data$X_tot), ]

# xbox sales data from 2005/11 to 2023/10
xbox.ts <- ts(log(data$X_tot), frequency = 12, start = c(2005,11))
basic_xbox <- ts((data$X_tot), frequency = 12, start = c(2005,11))
autoplot(basic_xbox, ylab= 'Xbox sales', main = 'Xbox console sales' ) 

autoplot(basic_xbox, ylab= 'Xbox sales', main = 'Xbox console sales' ) + 
  geom_vline(xintercept=2017.5, color='red', linetype='dashed') 

autoplot(basic_xbox, ylab= 'Xbox sales', main = 'Xbox console sales' ) + 
  geom_vline(aes(xintercept=2017.5, linetype='Game pass introduction'), color='red') +
  scale_linetype_manual(name = "", values = c("Game pass introduction" = "dashed"))

# the series is not stationary, strong seasonality
# no trend
autoplot(xbox.ts, ylab= 'log(Xbox sales)', main = 'Xbox console sales')

dummy <- c(rep(0, 140), rep(1, 76))

# --------------------TSLM-----------------------------------------------------------------

lm_dummy <- tslm(xbox.ts ~ dummy)
summary(lm_dummy)

lm_seasonal <- tslm(xbox.ts ~ dummy + season)
summary(lm_seasonal)

help <- data.frame(data=as.matrix(fitted(lm_seasonal)), date=time(fitted(lm_seasonal)))

autoplot(xbox.ts, ylab = 'log(Xbox sales)', main = 'Fitted values of Linear Regression model') + 
  geom_line(data=help, aes(date,data), col='2')

plot(fitted(lm_seasonal))
lines(fitted(lm_only), col=3)

autoplot(residuals(lm_seasonal), ylab='Residuals')
dwtest(lm_seasonal)
acf(residuals(lm_seasonal), main='ACF plot of residuals')

# ----------------ARIMAX------------------------------

tsdisplay(xbox.ts)

fit_s <- auto.arima(xbox.ts, xreg = dummy, stepwise = FALSE, approximation = FALSE)
summary(fit_s)

help_x <- data.frame(data=as.matrix(fitted(fit_s)), date=time(fitted(fit_s)))

autoplot(xbox.ts, ylab='log(Xbox sales)') + geom_line(data=help_x, aes(date,data), col='2')

checkresiduals(fit_s)

autoplot(residuals(fit_s), ylab='Residuals')
acf(residuals(fit_s), main='ACF of residuals')

# ---------------------SMOOTH SPLINE---------------------------------------

xbs <- ts((data$X_tot), frequency = 12, start = c(2005,11))

autoplot(xbs)

ss <- smooth.spline(time(xbs), xbs, lambda = 0.001)
ss_df <- data.frame(time = time(xbs), fitted = ss$y)

autoplot(xbs, ylab = 'Xbox sales') + 
  geom_line(data = ss_df, aes(x = time, y = fitted), color = "darkgreen") +
  geom_vline(aes(xintercept=2017.5, linetype='Game pass introduction'), color='red') +
  scale_linetype_manual(name = "", values = c("Game pass introduction" = "dashed"))
  
autoplot(xbs, ylab = 'Xbox sales') + 
  geom_line(data = ss_df, aes(x = time, y = fitted,linetype='Spline'), color = "darkgreen") +
  scale_linetype_manual(name = "", values = c("Spline" = "solid"))

# ---------------------FORECASTS---------------------------------------
smooth_xbox <- ts(ss$y, frequency=12, start=c(2005,11))
smooth_fit <- auto.arima(smooth_xbox, xreg = dummy, stepwise = FALSE, approximation = FALSE)
smooth_fit
plot(smooth_xbox, type='l')
lines(fitted(smooth_fit), col=2)

plot(residuals(smooth_fit))

acf(smooth_xbox)

ari <- auto.arima(smooth_xbox,  stepwise = FALSE, approximation = FALSE)
ari

plot(smooth_xbox)
lines(fitted(ari), col=2)
acf(residuals(ari))
checkresiduals(ari)
fcst <- forecast(ari)
autoplot(fcst, ylab='XBOX console sales')

a <- ses(smooth_xbox,alpha=0.2, initial="simple", h=5)
plot(smooth_xbox)
lines(fitted(a), col = 2)
af <- forecast(a)
plot(af)

arm <- auto.arima(xbs, stepwise = FALSE, approximation = FALSE)
arm
plot(xbs)
lines(fitted(arm), col=2)
checkresiduals(arm)
fc <- forecast(arm)
autoplot(fc, ylab='Xbox console sales')

