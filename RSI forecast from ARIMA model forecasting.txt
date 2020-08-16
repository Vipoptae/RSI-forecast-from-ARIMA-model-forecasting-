rm(list = ls())
if(!is.null(dev.list())) dev.off()
##### require package #####
library(quantmod)
library(forecast)
library(tseries)
library(timeSeries)
library(rugarch)
library(MLmetrics)

#####  specify startdate,end,forecast times #####
stockname=readline("Enter your Stock name : ")
stockname=sprintf("%s.BK",stockname)
startdate='2017-01-01'
# enddate='2019-12-31'
forecast_time=14

##### get data #####
stockvar=getSymbols(c(stockname),src = 'yahoo',from=startdate,auto.assign=FALSE)
# stockvar=to.weekly(stockvar)
stockvar=na.omit(stockvar)
stockvar_HLC=stockvar[,c(2,3,4)]
close_price=stockvar[,4]
close_price=na.omit(close_price)

##### model arima fit #####
modelfit <- auto.arima(close_price,seasonal = FALSE,stepwise = FALSE,approximation = FALSE)
price_forecast <- forecast(modelfit, h=forecast_time)
plot(price_forecast,sub=sprintf("%s",stockname),ylim=range(c(min(price_forecast$lower),price_forecast$upper)),xlim=range(c(length(close_price)+1,length(close_price)+14)))

##### train test  #####
N = length(close_price)
n = N-14
train = close_price[1:n, ]
test  = close_price[(n+1):N,  ]

##### train test arima #####
trainarimafit <- auto.arima(train,seasonal = FALSE,stepwise = FALSE,approximation = FALSE)
predlen=length(test)
trainarimaforecast <- forecast(trainarimafit, h=predlen)
close_price_merge=c(train,test)
trainmerge=c(as.numeric(train),rep(NA,length(test)))
testmerge=c(rep(NA,length(train)),test[1:length(test)])
trainarimafitmerge=c(as.numeric(train),trainarimaforecast$mean[1:length(test)])
trainarimafitmerge_min=c(as.numeric(train),trainarimaforecast$lower[,2])
trainarimafitmerge_max=c(as.numeric(train),trainarimaforecast$upper[,2])
Comparison <- data.frame(close_price_merge,trainmerge,
                         testmerge, trainarimafitmerge,trainarimafitmerge_min,trainarimafitmerge_max)
Comparison_ARIMA=Comparison
names(Comparison) <- c("actual", "Train_Data", "Test_Data", "HW_Forecast","min","max")

plot(Comparison$actual,type='l', col="red",
     main="Forecast Comparison Plot of ARIMA", xlab="Time", ylab="Values",sub=sprintf("%s",stockname),xlim=range(c(length(train)+2,length(train)+length(test))),ylim=range(c(min((trainarimaforecast$lower)),max((trainarimaforecast$upper)))))
lines(Comparison$HW_Forecast, col="green")
lines(Comparison$Test_Data, col="blue")
lines(Comparison$min, col="green")
lines(Comparison$max, col="green")
lines(Comparison$Train_Data, col="red")
MAPE_ARIMA=MAPE(trainarimaforecast$mean[1:length(test)],test)

###### forecast indicators in ARIMA model ######## data + forecast #####

price_all_arima_mean=c(as.ts(close_price),price_forecast$mean)
price_all_arima_min=c(as.ts(close_price),price_forecast$lower[,2])
price_all_arima_max=c(as.ts(close_price),price_forecast$upper[,2])

##### RSI ######

rsi_price_all_arima_mean=RSI(price_all_arima_mean)
rsi_price_all_arima_min=RSI(price_all_arima_min)
rsi_price_all_arima_max=RSI(price_all_arima_max)

rsi_forecast_present_mean=c(rsi_price_all_arima_mean[1:length(close_price)],rep(NA,forecast_time))
rsi_forecast_present_min=c(rsi_price_all_arima_min[1:length(close_price)],rep(NA,forecast_time))
rsi_forecast_present_max=c(rsi_price_all_arima_max[1:length(close_price)],rep(NA,forecast_time))
rsi_forecast_future_mean=c(rep(NA,length(close_price)),rsi_price_all_arima_mean[(length(close_price)+1):length(price_all_arima_mean)])
rsi_forecast_future_min=c(rep(NA,length(close_price)),rsi_price_all_arima_min[(length(close_price)+1):length(price_all_arima_min)])
rsi_forecast_future_max=c(rep(NA,length(close_price)),rsi_price_all_arima_max[(length(close_price)+1):length(price_all_arima_max)])
rsi_compare=data.frame(rsi_forecast_future_mean,rsi_forecast_future_min,rsi_forecast_future_max,
                       rsi_forecast_present_mean,rsi_forecast_present_min,rsi_forecast_present_max,
                       rsi_price_all_arima_mean,rsi_price_all_arima_min,rsi_price_all_arima_max)

plot(rsi_compare$rsi_price_all_arima_mean,type='l',col='green',main="RSI forecast ARIMA",sub=sprintf("%s",stockname),xlim=range(c(length(close_price),length(rsi_price_all_arima_mean))),ylim=range(c(30,70)))
# lines(rsi_forecast_future_mean,col='red')
# lines(rsi_forecast_future_min,col='blue')
# lines(rsi_forecast_future_max,col='blue')
predictRSI=rsi_price_all_arima_mean[(length(close_price)+1):length(price_all_arima_mean)]

##### ADX #####


HLC_forecast=as.ts(data.frame(price_forecast$upper[,2],
                              price_forecast$lower[,2],
                              price_forecast$mean))
names(HLC_forecast) <- c("price_all_arima_max", "price_all_arima_min", "price_all_arima_mean")
names(stockvar_HLC) <- c("price_all_arima_max", "price_all_arima_min", "price_all_arima_mean")
price_for_adx=rbind(as.ts(stockvar_HLC),HLC_forecast)
adx_forecast=ADX(price_for_adx)
adx=adx_forecast[,4]
adx_present=c(ADX(as.ts(stockvar_HLC))[,4],rep(NA,forecast_time))
plot(adx,type='l',col='red',main="ADX forecast ARIMA",xlim=range(c(length(close_price),length(close_price)+14)))
lines(adx_present,col='green')

##### stochastic #####

sto_price_all_arima_mean=stoch(price_all_arima_mean,nFastK = 50)
sto_price_all_arima_min=stoch(price_all_arima_min,nFastK = 50)
sto_price_all_arima_max=stoch(price_all_arima_max,nFastK = 50)

plot(sto_price_all_arima_max[,1]*100,type='l',col='red',main="STO forecast ARIMA",xlim=range(c(length(close_price),length(close_price)+14)),ylim=range(0,100))
lines(sto_price_all_arima_min[,1]*100,col='blue')
lines(sto_price_all_arima_mean[,1]*100,col='green')


# trainarimafit
MAPE_ARIMA
# price_forecast$model
close_price[length(close_price)]
price_forecast$mean
# as.ts(test)
# trainarimaforecast$mean
predictRSI









