library(tidyverse)
library(forecast)
library(zoo)
library(here)
library(patchwork)
setwd(here())
rm(list=ls())

as <- read_csv("data/ApplianceShipments.csv")
head(as$Quarter)
tail(as$Quarter)

asts <- ts(as$Shipments, start = c(1985, 1), end = c(1989, 4),
           frequency = 4)
autoplot(asts) +
  ggtitle("Shipments of household appliances", subtitle = "1985-1989") +
  xlab("Year")+
  ylab("M US$")

acf(asts)

stl(asts) %>% seasonal(4,2)
asts %>%
  stl(s.window=4, t.window=2) %>%
  autoplot() +
  ggtitle("Shipments of household appliances", subtitle = "1985-1989") 


autoplot(asts) / autoplot(diff(asts,1)) /autoplot(diff(asts,4)) /
  autoplot(diff(diff(asts,4),4))

train.ts <- window(asts, end = c(1985, length(asts) - 4))
train.mat <- rollmean(train.ts, 
                      k = 4, # set window to 1 year
                      align = "right")
last.mat <- tail(train.mat, 1) # take the final value to forecast
mat.pred.ts <- ts(rep(last.mat, 4), # make ts of moving average forecast
                  start = c(1985, length(asts) - 4), 
                  end = c(1985, length(asts)), 
                  freq = 4) 

autoplot(asts) + # plot trailing moving average and forecast
  autolayer(train.mat) + 
  autolayer(mat.pred.ts)


gglagplot(asts, 
          #set.lags = c(1, 12) # the set.lags argument allows specific lags to be supplied
)

ggAcf(asts)

ma.centred <- ma(asts, order = 4 # this is the window argument
)
# trailing - uses 12 mths behind
# the zoo package contains a function, rollmean(), for calculating a trailing moving average
ma.trailing <- rollmean(asts,
                        k = 4, # this is the window argument
                        align = "right" # this sets to trailing average
)
# we can plot these using autoplot() and autolayer(), which is less verbose than base plot
autoplot(asts) +
  autolayer(ma.centred) +
  autolayer(ma.trailing)

as.ses <- ses(train.ts, alpha = 0.2, h = 4)
checkresiduals(as.ses)

# Ljung-Box test
# 
# data:  Residuals from Simple exponential smoothing
# Q* = 0.32658, df = 3, p-value = 0.955
# 
# Model df: 0.   Total lags used: 3


autoplot(as.ses) +
  autolayer(fitted(as.ses))

as.ets <- ets(train.ts)
ets.fc <- forecast(as.ets, h=4)
checkresiduals(ets.fc)

accuracy(ets.fc)

autoplot(ets.fc) + 
  autolayer(asts, colour = FALSE) +
  theme(legend.position = "none")

aaa.as.ets <- ets(train.ts, model = "AAA")
aaa.ets.fc <- forecast(aaa.as.ets, h=4)
checkresiduals(aaa.ets.fc)

accuracy(aaa.ets.fc)

autoplot(aaa.ets.fc) + 
  autolayer(asts, colour = FALSE) +
  theme(legend.position = "none")
