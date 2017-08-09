#Historical VAR Analysis of a Stock using OHLC data at differenct level of confidence
stockdata<-read.csv(choose.files(),header = TRUE)#importing data
close<-stockdata$Close #assigning closing price
returns<-(close[1:(length(close)-1)]-close[2:length(close)])/close[2:length(close)]#calculating returns
plot(close, type="l",xlab = "days",ylab = "Closing Price",main="Plotting Stock Data" ) #plotting stock data
library(PerformanceAnalytics)
Var99<-(VaR(returns,p=0.99,method="historical")*100)#Value at Risk at 99% Confidence
Var95<-(VaR(returns,p=0.95, method = "historical")*100) #Value at risk at 95% Confidence
var90<-(VaR(returns,p=0.90,method = "historical")*100) #value at risk at 90% confidence
varmatrix<-cbind(var90,Var95,Var99)
colnames(varmatrix)<-c("var90","Var95","Var99")#creating head
varmatrix
write.csv(varmatrix,file = "VAR of stock")
plot(varmatrix,type = "l")
barplot(varmatrix,ylab = "Risk in %",ylim = c(-1:-2))
xx<-qnorm(p=0.90,mean(returns),sd(returns))
mean(returns)
sd(returns)
plot(xx)
qnorm(0.999)
