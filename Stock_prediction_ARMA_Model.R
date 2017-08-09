mydata<-read.csv(choose.files(),header = TRUE)
library(ggplot2)
library(tseries)
plot(mydata$Close,type = "l",col="red",xlab = "Days",ylab = "Closing Price", main = "Stock Price")
stockR<-(mydata$Close[1:(length(mydata$Close)-1)]-mydata$Close[2:length(mydata$Close)])/mydata$Close[2:length(mydata$Close)]
plot(stockR,type = "l",col="green", xlab = "Number of Days", ylab = "Stock Return",main = "Stock Return")#forcasting Return
adf.test(stockR,alternative = "stationary")#rejecting null hypothasis p-value = 0.01.Using Augmented Dickey-Fuller Test
acf(stockR) #not significant 
pacf(stockR)# is Significant 
library(forecast)
auto_arima<-auto.arima(stockR,stationary = TRUE,seasonal = FALSE,ic="aic")
arima(stockR,order = c(1,0,0))
arima(stockR,order = c(2,0,0))
arima(stockR,order = c(1,0,1))
arima_final<-arima(stockR,order = c(1,0,0))
predicted<-predict(arima_final,n.head=10)
class(predicted)
write.csv(predicted,file = "ARIMA_Model")
tadiag(arima_final)
coloum(mydata)
head(mydata)
tsdiag(auto_arima)
