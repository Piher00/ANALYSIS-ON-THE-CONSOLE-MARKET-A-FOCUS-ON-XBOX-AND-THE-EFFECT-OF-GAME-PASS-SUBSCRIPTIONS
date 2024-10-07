library(forecast)
library(ggplot2)
library(DIMORA)
library(readxl)

gamepassdata <- read.csv("gamepassdata.csv", stringsAsFactors=TRUE)

gp.ts<- ts(gamepassdata$Subscribers.as.of.Month, frequency=12, start=c(2017,7),end=c(2019,12))

# FIRST EXPLORATORY PLOTS
autoplot(gp.ts)+
  ggtitle("Gamepass subscribers") +
  xlab("") +
  ylab("Number of subscriptions") 

tsdisplay(gp.ts)


#--------------------------------MODELS-----------------------------------------

#-----------------------LINEAR REGRESSION---------------------------------------
lm_trend<- tslm(gp.ts ~ trend )
summary(lm_trend)
res_lm_trend<- residuals(lm_trend)
tsdisplay(res_lm_trend)
for_lm_trend<- forecast(lm_trend)
autoplot(for_lm_trend) + ylab("") + xlab("Year")
plot(gp.ts)
lines(lm_trend$fitted.values,col="red")

lm_trend_season <- tslm(gp.ts~ trend + season)
summary(lm_trend_season)
res_lm_trend_season<- residuals(lm_trend_season)
tsdisplay(res_lm_trend)
for_lm_trend_season<- forecast(lm_trend_season)
plot(for_lm_trend_season)


#----------------------------- ARIMA MODEL ------------------------------------

auto<- auto.arima(gp.ts)
summary(auto)
start_date <- as.Date("2017-07-01")
end_date <- as.Date("2023-12-01")
plot(gp.ts)
for_auto<- forecast(auto, h=20)
lines(auto$fitted, col="red")
lines(for_auto$mean, col="red")
#predictions
autoplot(for_auto)
#residuals
res<- residuals(auto)
tsdisplay(res)


#-------------------- Smoothing methods-----------------



## Trend methods (Holt method)

fc<- holt(gp.ts, h=15)
fc2<- holt(gp.ts, damped=T, phi=0.9, h=15)

autoplot(gp.ts)+
  autolayer(fc, series="Holt's method", PI=F)+
  autolayer(fc2, series="Damped Holt's method", PI=F)+ ylab("") + xlab("")

### Trend and seasonality methods (Holt-Winters method)

fit1<- hw(gp.ts, seasonal="additive")
fit2<- hw(gp.ts, seasonal="multiplicative")

autoplot(gp.ts)+
  autolayer(fit2, series="", PI=F)+
  #autolayer(fit2, series="Holt-Winters with multiplicative seasonality", PI=F)+
  ylab("") + xlab("")

fit1$fitted
predict(fit1, h=24)


#------------------------ BASS MODEL----------------------------

## Simple Bass Model 
bm_gp<-BM(gp.ts,display = T)
summary(bm_gp) #R2 very high because the cumulative is almost perfect fitted

