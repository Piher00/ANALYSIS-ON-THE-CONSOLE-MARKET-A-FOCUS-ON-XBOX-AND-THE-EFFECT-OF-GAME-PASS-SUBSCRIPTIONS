library(forecast)
library(ggplot2)
library(DIMORA)
library(readxl)
library(ggplot2)


# 1 ------ Data plot -----------------------------------------------------------

# Nintendo data

sales <- read_excel('month_sales.xlsx')
dates <- sales$...1
plot(dates, sales$N_DS, type = 'l',
     main = 'Nintendo consoles sales',
     xlab = 'Year', ylab = 'Sales')
points(dates, sales$N_wii, type = 'l', col = 'blue')
points(dates, sales$N_3D, type = 'l', col = 'red')
points(dates, sales$N_U, type = 'l', col = 'green')
points(dates, sales$N_switch, type = 'l', col = 'orange')
legend(ISOdate(2020,3,1), 10200000, legend = c('DS sales', 'Wii sales', '3DS sales', 
                              'Wii U sales', 'Switch sales'),
       lty = c(1, 1, 1, 1, 1), col = c('black', 'blue', 'red', 'green', 'orange'),
       cex = 0.6)

barplot(height = c(sum(sales$N_DS, na.rm = TRUE), sum(sales$N_wii, na.rm = TRUE),
                   sum(sales$N_3D, na.rm = TRUE), sum(sales$N_U, na.rm = TRUE),
                   sum(sales$N_switch, na.rm = TRUE)), 
        names = c('DS', 'Wii', '3DS', 'Wii U', 'Switch'),
        col = c('black', 'blue', 'red', 'green', 'orange'),
        main = 'Total sales for Nintendo consoles')

# PS data

plot(dates, sales$P_4, type = 'l',
     main = 'PS consoles sales',
     xlab = 'Year', ylab = 'Sales', col = 'red')
points(dates, sales$P_3, type = 'l', col = 'blue')
points(dates, sales$P_vita, type = 'l', col = 'green')
points(dates, sales$P_PSP, type = 'l')
points(dates, sales$P_5, type = 'l', col = 'orange')
legend(ISOdate(2020,6,1), 5000000, legend = c('PSP sales', 'P3 sales', 'PS vita sales', 
                                               'P4 sales', 'P5 sales'),
       lty = c(1, 1, 1, 1, 1), col = c('black', 'blue', 'green', 'red', 'orange'),
       cex = 0.6)

barplot(height = c(sum(sales$P_PSP, na.rm = TRUE), sum(sales$P_3, na.rm = TRUE),
                   sum(sales$P_vita, na.rm = TRUE), sum(sales$P_4, na.rm = TRUE),
                   sum(sales$P_5, na.rm = TRUE)), 
        names = c('PSP', 'PS3', 'PS vita', 'PS4', 'PS5'),
        col = c('black', 'blue', 'green', 'red', 'orange'),
        main = 'Total sales for PS consoles')

# XBOX data

plot(dates, sales$X_360, type = 'l',
     main = 'XBOX consoles sales',
     xlab = 'Year', ylab = 'Sales')
points(dates, sales$X_one, type = 'l', col = 'blue')
points(dates, sales$X_S, type = 'l', col = 'red')
legend(ISOdate(2020,1,1), 4300000, legend = c('XBOX 360 sales', 
                                              'XBOX one sales', 
                                              'XBOX S sales'),
       lty = c(1, 1, 1), col = c('black', 'blue', 'red'),
       cex = 0.6)

barplot(height = c(sum(sales$X_360, na.rm = TRUE),
                   sum(sales$X_one, na.rm = TRUE),
                   sum(sales$X_S, na.rm = TRUE)), 
        names = c('XBOX 360', 'XBOX ONE', 'XBOX S'),
        col = c('black', 'blue', 'red'),
        main = 'Total sales for XBOX consoles')

# plot all the consoles, all the companies

df_sales <- data.frame(dates = dates, 
                 Nintendo_sales = sales$N_tot, 
                 PS_sales = sales$P_tot, 
                 XBOX_sales = sales$X_tot)

p_sales <- ggplot(df_sales, aes(x = dates)) +
  geom_line(aes(y = Nintendo_sales, color = 'Nintendo sales')) +
  geom_line(aes(y = PS_sales, color = 'PS sales')) +
  geom_line(aes(y = XBOX_sales, color = 'XBOX sales')) +
  scale_color_manual(values = c('Nintendo sales' = 'red', 'PS sales' = 'blue', 'XBOX sales' = 'green')) +
  labs(x = 'Year', y = 'Sales', color = 'Legend', title = 'Total sales for all consoles')

print(p_sales)

# total plot for just the home consoles

df_sales_home <- data.frame(dates = dates, 
                       Nintendo_sales = sales$N_tot_home, 
                       PS_sales = sales$P_tot_home, 
                       XBOX_sales = sales$X_tot)

p_sales_home <- ggplot(df_sales_home, aes(x = dates)) +
  geom_line(aes(y = Nintendo_sales, color = 'Nintendo sales')) +
  geom_line(aes(y = PS_sales, color = 'PS sales')) +
  geom_line(aes(y = XBOX_sales, color = 'XBOX sales')) +
  scale_color_manual(values = c('Nintendo sales' = 'red', 'PS sales' = 'blue', 'XBOX sales' = 'green')) +
  labs(x = 'Year', y = 'Sales', color = 'Legend', title = 'Total sales for home consoles')

print(p_sales_home)


# 2 ------ Home consoles competition -------------------------------------------

# our focus is on the Xbox sales with respect to the other companies

xbox_tot <- na.omit(sales$X_tot)
nin_tot <- na.omit(sales$N_tot_home)
ps_tot <- na.omit(sales$P_tot_home)

# first we try using the UCRCD ignoring the seasonality

XN_comp <- UCRCD(xbox_tot, nin_tot, display = T)
summary(XN_comp)

XP_comp <- UCRCD(xbox_tot, ps_tot, display = T)
summary(XP_comp)



# let's now try by using annual data
# we crop the data, removing the ones from years that are not complete

ps_y <- c()
for (i in seq(3, 183, by = 12)){
  new_value <- 0
  for (j in 0:11){
    new_value <- new_value + ps_tot[i + j]
  }
  ps_y <- append(ps_y, new_value)
}

nin_y <- c()
for (i in seq(3, 183, by = 12)){
  new_value <- 0
  for (j in 0:11){
    new_value <- new_value + nin_tot[i + j]
  }
  nin_y <- append(nin_y, new_value)
}

xbox_y <- c()
for (i in seq(3, 195, by = 12)){
  new_value <- 0
  for (j in 0:11){
    new_value <- new_value + xbox_tot[i + j]
  }
  xbox_y <- append(xbox_y, new_value)
}

#plot the yearly data

df_sales_y <- data.frame(Year = 2006:2022)
df_sales_y$Nintendo_sales[2:17] <- nin_y
df_sales_y$PS_sales[2:17] <- ps_y
df_sales_y$XBOX_sales <- xbox_y

p_sales_y <- ggplot(df_sales_y, aes(x = Year)) +
  geom_line(aes(y = Nintendo_sales, color = 'Nintendo sales')) +
  geom_line(aes(y = PS_sales, color = 'PS sales')) +
  geom_line(aes(y = XBOX_sales, color = 'XBOX sales')) +
  scale_color_manual(values = c('Nintendo sales' = 'red', 'PS sales' = 'blue', 'XBOX sales' = 'green')) +
  labs(x = 'Year', y = 'Sales', color = 'Legend', title = 'Annual sales for all consoles')

print(p_sales_y)

# competition models with yearly data

XN_comp_y <- UCRCD(xbox_y[2:length(xbox_y)], nin_y, display = T)
summary(XN_comp_y)

XP_comp_y <- UCRCD(xbox_y[2:length(xbox_y)], ps_y, display = T)
summary(XP_comp_y)



# finally, use smoothing to get nicer time series for the competition model
# lambda chosen by trying different ones until we got a good
# approximation of the data which did not capture too much the seasonality

smooth_xbox <- smooth.spline(dates[13:length(dates)], xbox_tot, lambda=0.001)
plot(dates[13:length(dates)], xbox_tot, xlab="time", ylab="sales", type = 'l',
     main = 'Confront XBOX values with the smoothed ones')
lines(dates[13:length(dates)], smooth_xbox$y, type = 'l', col = 'green')
legend(ISOdate(2019,1,1), 4200000, legend = c('XBOX sales', 
                                              'Smoothed XBOX sales'),
       lty = c(1, 1), col = c('black', 'green'),
       cex = 0.6)

smooth_nin <- smooth.spline(dates[25:length(dates)], nin_tot, lambda = 0.001,)
plot(dates[25:length(dates)], nin_tot, type = 'l', xlab = 'Time',
     ylab = 'Nintendo sales', ylim = c(0, max(nin_tot) + 1000000),
     main = 'Confront Nintendo values with the smoothed ones')
lines(dates[25:length(dates)], smooth_nin$y, type = 'l', col = 'red')
legend(ISOdate(2018,10,1), 8500000, legend = c('Nintendo sales', 
                                               'Smoothed Nintendo sales'),
       lty = c(1, 1), col = c('black', 'red'),
       cex = 0.6)

smooth_ps <- smooth.spline(dates[25:length(dates)], ps_tot, lambda = 0.001)
plot(dates[25:length(dates)], ps_tot, type = 'l', xlab = 'Time', ylab =  'PS sales',
     main = 'Confront PS values with the smoothed ones')
lines(dates[25:length(dates)], smooth_ps$y, type = 'l', col = 'blue')
legend(ISOdate(2019,6,1), 4900000, legend = c('PS sales', 
                                              'Smoothed PS sales'),
       lty = c(1, 1), col = c('black', 'blue'),
       cex = 0.6)

# plot all the smooth sales for home consoles

df_smooth_sales <- data.frame(dates = dates[13:length(dates)])
df_smooth_sales$XBOX_sales <- smooth_xbox$y
df_smooth_sales$Nintendo_sales[13:length(df_smooth_sales$dates)] <- smooth_nin$y
df_smooth_sales$PS_sales[13:length(df_smooth_sales$dates)] <- smooth_ps$y
p_sales_smooth <- ggplot(df_smooth_sales, aes(x = dates)) +
  geom_line(aes(y = Nintendo_sales, color = 'Nintendo sales')) +
  geom_line(aes(y = PS_sales, color = 'PS sales')) +
  geom_line(aes(y = XBOX_sales, color = 'XBOX sales')) +
  scale_color_manual(values = c('Nintendo sales' = 'red', 'PS sales' = 'blue', 'XBOX sales' = 'green')) +
  labs(x = 'Year', y = 'Sales', color = 'Legend', title = 'Smooth sales for all consoles    Lambda = 0.001')

print(p_sales_smooth)

# competition model between smooth PS and Xbox for home consoles

comp_xp_smooth <- UCRCD(smooth_xbox$y, smooth_ps$y, display = T)
summary(comp_xp_smooth)

# competition model between smooth Nintendo and Xbox for home consoles

comp_xn_smooth <- UCRCD(smooth_xbox$y, smooth_nin$y, display = T)
summary(comp_xn_smooth)

# problem: Nintendo does not follow a Bass model
# checking the plot for the problem

df_sales_nin <- data.frame(dates = dates, 
                           Wii_sales = sales$N_wii, 
                           WiiU_sales = sales$N_U, 
                           Switch_sales = sales$N_switch)

p_sales_nin_home <- ggplot(df_sales_nin, aes(x = dates)) +
  geom_line(aes(y = Wii_sales, color = 'Wii sales')) +
  geom_line(aes(y = WiiU_sales, color = 'WiiU sales')) +
  geom_line(aes(y = Switch_sales, color = 'Switch sales')) +
  scale_color_manual(values = c('Wii sales' = 'orange', 'WiiU sales' = 'darkred', 'Switch sales' = 'red')) +
  labs(x = 'Year', y = 'Sales', color = 'Legend', title = 'Total sales for Nintendo home consoles')

print(p_sales_nin_home)

# Trying competition in two different time periods to compensate for the 
# Nintendo WiiU downfall

# Visualize where to split using the yearly data

p_sales_nin_y <- ggplot(df_sales_y, aes(x = Year)) +
  geom_line(aes(y = Nintendo_sales, color = 'Nintendo sales')) +
  scale_color_manual(values = c('Nintendo sales' = 'red')) +
  labs(x = 'Year', y = 'Sales', color = 'Legend', title = 'Annual sales for Nintendo')

print(p_sales_nin_y)

# At the start of 2017 the Nintendo Switch was released improving the sales

plot(dates[13:147], smooth_xbox$y[1:135], type = 'l',
     main = 'First period of interest', xlab = 'Year',
     ylab = 'Sales', col = 'green',
     ylim = c(0, max(smooth_nin$y[1:123]) + 100000))
lines(dates[25:147], smooth_nin$y[1:123], type = 'l', col = 'red')
legend(ISOdate(2012,1,1), 1900000, legend = c('Smoothed XBOX sales 2006-2016', 
                                              'Smoothed Nintendo sales 2007-2016'),
       lty = c(1, 1), col = c('green', 'red'),
       cex = 0.6)


plot(dates[148:length(dates)], smooth_xbox$y[136:length(smooth_xbox$y)], type = 'l',
     main = 'Second period of interest', xlab = 'Year',
     ylab = 'Sales', col = 'green',
     ylim = c(0, max(smooth_nin$y[112:length(smooth_nin$y)]) + 600000))
lines(dates[148:length(dates)], smooth_nin$y[124:length(smooth_nin$y)],
      type = 'l', col = 'red')
legend(ISOdate(2021,1,1), 2800000, legend = c('Smoothed XBOX sales 2017-2023', 
                                              'Smoothed Nintendo sales 2017-2023'),
       lty = c(1, 1), col = c('green', 'red'),
       cex = 0.6)

# confront Xbox and Nintendo until start of 2017 (introduced switch)

comp_xn_pt1 <- UCRCD(smooth_xbox$y[1:135],
                     smooth_nin$y[1:123], display = T)

summary(comp_xn_pt1)

# confront Xbox and Nintendo from 2017 to end of 10/2023

comp_xn_pt2 <- UCRCD(smooth_xbox$y[136:length(smooth_xbox$y)],
                     smooth_nin$y[124:length(smooth_nin$y)], display = T)
summary(comp_xn_pt2)



# 3 ------ Competition between consoles of the same company --------------------

# To avoid the seasonality, we once again use smoothing
# we avoid yearly data because we would get too little data points
# the model would likely get bad results or they could not be computed

smooth_360 <-  smooth.spline(dates[13:158], xbox_360, lambda = 0.001)
smooth_one <- smooth.spline(dates[109:218], xbox_one, lambda = 0.001)
smooth_s <- smooth.spline(dates[193:length(dates)], xbox_s, lambda = 0.007)

# confront real and smoothed data

plot(dates[13:158], xbox_360, type = 'l', xlab = 'Year',
     ylab = 'Sales', main = 'Real vs smoothed XBOX 360 data')
lines(dates[13:158], smooth_360$y, type = 'l', col = 'red')

plot(dates[109:218], xbox_one, type = 'l', xlab = 'Year',
     ylab = 'Sales', main = 'Real vs smoothed XBOX One data')
lines(dates[109:218], smooth_one$y, type = 'l', col = 'red')

plot(dates[193:length(dates)], xbox_s, type = 'l', xlab = 'Year',
     ylab = 'Sales', main = 'Real vs smoothed XBOX S data')
lines(dates[193:length(dates)], smooth_s$y, type = 'l', col = 'red')

# plot all the smoothed data to confront them

plot(x = dates[13:length(dates)], xbox_tot, type = 'n',
     main = 'Smooth xbox consoles sales',
     xlab = 'Year', ylab = 'Sales',
     ylim = c(0,1500000))
lines(dates[13:158], smooth_360$y, type = 'l', col = 'green')
lines(dates[109:218], smooth_one$y, type = 'l', col = 'red')
lines(dates[193:length(dates)], smooth_s$y, type = 'l', col = 'blue')
legend(ISOdate(2019, 8, 1), 1500000, legend = c('Smooth XBOX 360', 
                                  'Smooth XBOX One', 
                                  'Smooth XBOX S'),
       lty = c(1, 1, 1), col = c('green', 'red', 'blue'),
       cex = 0.6)

df_smooth_sales_xbox <- data.frame(dates = dates[13:length(dates)])
df_smooth_sales_xbox$x360_sales <- NA
df_smooth_sales_xbox$x360_sales[1:146] <- smooth_360$y
df_smooth_sales_xbox$xone_sales <- NA
df_smooth_sales_xbox$xone_sales[97:206] <- smooth_one$y
df_smooth_sales_xbox$xs_sales <- NA
df_smooth_sales_xbox$xs_sales[181:length(df_smooth_sales_xbox$dates)] <- smooth_s$y

p_sales_smooth_xbox <- ggplot(df_smooth_sales_xbox, aes(x = dates)) +
  geom_line(aes(y = x360_sales, color = 'XBOX 360 sales')) +
  geom_line(aes(y = xone_sales, color = 'XBOX One sales')) +
  geom_line(aes(y = xs_sales, color = 'XBOX S sales')) +
  scale_color_manual(values = c('XBOX 360 sales' = 'mediumseagreen', 'XBOX One sales' = 'darkgreen', 'XBOX S sales' = 'green3')) +
  labs(x = 'Year', y = 'Sales', color = 'Legend', title = 'Smooth sales for XBOX consoles    Lambda = 0.001')

print(p_sales_smooth_xbox)

# create the models for competition and check them

renew_360_one <- UCRCD(smooth_360$y, smooth_one$y[1:50], display = T)
summary(renew_360_one)

renew_one_s <- UCRCD(smooth_one$y, smooth_s$y[1:26], display = T)
summary(renew_one_s)
