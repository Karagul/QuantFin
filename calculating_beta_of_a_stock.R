library(plotly)
library(quantmod)
nifty<-read.csv(choose.files(),header = TRUE)
rcom<-read.csv(choose.files(),header = TRUE)
head(nifty)
head(rcom)
NC<-nifty$Close
RC<-rcom$Close.Price
summary(NC)
summary(RC)
plot(NC,type = "l",main="Nifty",col="Red")
plot(RC,type="l",main="RCOM",col="green")
indexR<-(NC[1:(length(NC)-1)]-NC[2:length(NC)])/NC[2:length(NC)]
head(indexR)
stockR<-(RC[1:(length(RC)-1)]-RC[2:length(RC)])/RC[2:length(RC)]
head(stockR)
result<-lm(stockR~indexR)
summary(result)
beta<-result$coefficients[2,1]
print(beta)
