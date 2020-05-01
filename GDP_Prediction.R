install.packages("tidyverse")
install.packages("corrgram")
install.packages("tseries")
install.packages("readxl")
install.packages("urca")
install.packages("forecast")
install.packages("trend")
install.packages("zoo")
install.packages("reshape")

library(tidyverse)
library(readxl)
library(corrgram)
library(tseries)
library(urca)
library(forecast)
library(trend)
library(zoo)
library(reshape)


Unemployment<-read_excel("MacroData.xlsx",sheet="Sheet2")
Inflation<-read_excel("MacroData.xlsx",sheet="Sheet3")
GDP<-read_excel("MacroData.xlsx",sheet="Sheet7")


View(GDP)
view(Unemployment)
view(Inflation)
#select variables from the larger dataset

Year<-Unemployment[1,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

CanUn<-Unemployment[495,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

CanInf<-Inflation[1112,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47','Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

CanGDP<-GDP[572,c('Column6','Column7','Column8','Column9','Column10','Column11','Column12','Column13','Column14','Column15','Column16','Column17','Column18','Column19','Column20','Column21','Column22','Column23','Column24','Column25','Column26','Column27','Column28','Column29','Column30','Column31','Column32','Column33','Column34','Column35','Column36','Column37','Column38','Column39','Column40','Column41','Column42','Column43','Column44','Column45','Column46','Column47', 'Column48','Column49','Column50','Column51','Column52','Column53','Column54','Column55','Column56','Column57','Column58','Column59','Column60','Column61','Column62','Column63','Column64')]

View(Year)
#transpose the columns to observations

t_CanGDP<-t(CanGDP)

t_CanGDP<-as.numeric(t_CanGDP)

View(t_CanGDP)

t_CanInf<-t(CanInf)
t_CanInf<-as.numeric(t_CanInf)
View(t_CanInf)


t_CanUnem<-t(CanUn)
t_CanUnem<-as.numeric(t_CanUnem)
View(t_CanUnem)

t_Year<-t(Year)
t_Year<-as.numeric(t_Year)

View(t_Year)


TimeSeriesCan<-cbind(t_Year, t_CanGDP, t_CanInf, t_CanUnem)
View(TimeSeriesCan)



TimeSeriesCan<-as.data.frame(TimeSeriesCan)

TimeSeriesCan <- rename(TimeSeriesCan, c(t_Year="Years"))
View(TimeSeriesCan)


TimeSeriesCan <- rename(TimeSeriesCan, c(t_CanGDP="GrossDP"))

TimeSeriesCan <- rename(TimeSeriesCan, c(t_CanInf="Inflation"))

TimeSeriesCan <- rename(TimeSeriesCan, c(t_CanUnem="Unemploy"))

View(TimeSeriesCan)


#tell R this is time series data#
tsUR<-zoo(TimeSeriesCan$Unemploy, order.by = TimeSeriesCan$Years)
tsIF<-zoo(TimeSeriesCan$Inflation, order.by = TimeSeriesCan$Years)
tsGDP<-zoo(TimeSeriesCan$GrossDP, order.by = TimeSeriesCan$Years)



#create plots of the timeseries#

ggplot(data = TimeSeriesCan, aes(x = TimeSeriesCan$Years, y = TimeSeriesCan$GrossDP)) + geom_line()
ggplot(data = TimeSeriesCan, aes(x = TimeSeriesCan$Years, y = TimeSeriesCan$Inflation))+ geom_line()
ggplot(data = TimeSeriesCan, aes(x = TimeSeriesCan$Years, y = TimeSeriesCan$Unemploy))+ geom_line()


#test for stationarity#
#augmented dickey filler test
adf.test(tsGDP) # p-value of 0.8529, that means GDP not stationary
adf.test(tsIF) # p-value of 0.3608, that means Inflation not stationary
adf.test(tsUR) #p-value of 0.7774, that means unemp not stationary


# test for trend stationary-null is trend is stationarity and alternative is non stationarity

kpss.test(TimeSeriesCan$GrossDP, null = "Trend")
kpss.test(TimeSeriesCan$Inflation, null = "Trend")
kpss.test(TimeSeriesCan$Unemploy, null = "Trend")

#check the correlograms# 
acf(tsUR) #how many times the error term is correlated with the previous time period
acf(tsIF)
acf(tsGDP)

pacf(tsUR)
pacf(tsIF)
pacf(tsGDP)

Box.test(tsUR)
Box.test(tsIF)
Box.test(tsGDP)

#transform the data#

#differencing#
InflDiff1=diff(tsIF)
InflDiff2=diff(tsIF, differences=16)
URdiff=diff(tsUR)
URDiff2=diff(tsUR, differences=9)
GDPdiff=diff(tsGDP)
GDPdiff2=diff(tsGDP, differences=15)
YearDiff=diff(TimeSeriesCan$Years)

kpss.test(InflDiff2, null = "Trend")

kpss.test(GDPdiff2, null = "Trend")

kpss.test(URDiff2, null = "Trend")

# all three are trend stationary

acf(URDiff2) #
acf(InflDiff2)
acf(GDPdiff2)

pacf(URDiff2)# MA with 3
pacf(InflDiff2)# MA with 3
pacf(GDPdiff2)# MA with 3 

#detrending
m<-lm(coredata(GDPdiff)~index(GDPdiff))
GDPdetrend<-zoo(resid(m),index(GDPdiff))

plot(GDPdetrend)


#model data 



Arima(URDiff2, order = c(0,0,3),#use this
      include.mean = FALSE, include.drift = FALSE, include.constant =TRUE,
      method = "ML")
Arima(URDiff2, order = c(1,0,3),#use this
      include.mean = FALSE, include.drift = FALSE, include.constant =TRUE,
      method = "ML")
Arima(URDiff2, order = c(1,0,4),#use this
      include.mean = FALSE, include.drift = FALSE, include.constant =TRUE,
      method = "ML")
Arima(URDiff2, order = c(2,0,4),#use this
      include.mean = FALSE, include.drift = FALSE, include.constant =TRUE,
      method = "ML")
Arima(URDiff2, order = c(3,0,4),#use this
      include.mean = FALSE, include.drift = FALSE, include.constant =TRUE,
      method = "ML")
Arima(URDiff2, order = c(3,0,5),#use this
      include.mean = FALSE, include.drift = FALSE, include.constant =TRUE,
      method = "ML")
Arima(tsUR, order = c(1, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(tsGDP, order = c(2, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

#Unemployment Rate
Arima(tsUR, order = c(1, 0, 2),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

Arima(tsUR, order = c(1, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

#InflationRate
Arima(InflDiff2, order = c(2, 0, 5),
      include.mean = FALSE, include.drift = FALSE, include.constant =TRUE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 10),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 8),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 7),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 2),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(InflDiff2, order = c(0, 0, 2),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(GDPdiff, order = c(1, 0, 1),
      include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
      method = "ML")

Arima(GDPdiff2, order = c(2, 0, 5),
      include.mean = FALSE, include.drift = FALSE, include.constant =TRUE,
      method = "ML")
#modelling

UmData<-Arima(URDiff2, order = c(2,0,4),#use this
              include.mean = FALSE, include.drift = FALSE, include.constant =TRUE,
              method = "ML")

InfData<-Arima(InflDiff2, order = c(2, 0, 5),
               include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
               method = "ML")

GDPData<-Arima(GDPdiff2, order = c(2, 0, 5),
              include.mean = FALSE, include.drift = FALSE, include.constant =FALSE,
              method = "ML")

GDPData2<-Arima(tsGDP, order = c(2, 0, 1),
      include.mean = TRUE, include.drift = TRUE, include.constant =TRUE,
      method = "ML")

plot(forecast(UmData,h=10))
plot(forecast(InfData,h=10))
plot(forecast(GDPData,h=10))
plot(forecast(GDPData2,h=100))

