mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)
mydata2=mydata2[,-c(1,2,7,17,18,19,20,21,22,23,24,25,26)]
mydata2=mydata2[,-c(1)]
dim(mydata2)
install.packages("neuralnet")
library(neuralnet)

require(neuralnet)
#compute R^2 from the true and predicted values
eval_results<-function(true, predicted, df) {
  SSE<-sum((predicted - true)^2)
  SST<-sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE <- sqrt(SSE/nrow(df))
  MAP <- sum(abs((predicted - true)/true))/nrow(df)
  
  #Model performance metrics
  data.frame(
    Rsquare = R_square,
    RMSE = RMSE,
    MAPE = MAP
  )
  
}

set.seed(499)
index=sample(1:nrow(mydata2),round(0.70*nrow(mydata2)))
training=mydata2[index,]
test=mydata2[-index,]
dim(training)
dim(test)

max = apply(mydata2,2,max)
min = apply(mydata2,2,min)
scaled = as.data.frame(scale(mydata2, center = min, scale = max-min))

trainNN = scaled[index,]
testNN = scaled[-index,]

#no2
nnno2 = neuralnet(NO2t.1~Density+Small+Heavy+TEMPt+wst+NO2t, data = trainNN, hidden = 3, linear.output = TRUE)
plot(nnno2)
nnno2$weights

predict_testNN = compute(nnno2,testNN)
PREDNO2 = (predict_testNN$net.result * (max(mydata2$NO2t.1)-min(mydata2$NO2t.1)))+min(mydata2$NO2t.1)
eval_results(test$NO2t.1, PREDNO2, test)

#no
nnno = neuralnet(NOt.1~Density+Small+Heavy+TEMPt+wst+NOt, data = trainNN, hidden = 3, linear.output = TRUE)
plot(nnno)
nnno$weights

predict_testNN = compute(nnno,testNN)
PREDNO = (predict_testNN$net.result * (max(mydata2$NOt.1)-min(mydata2$NOt.1)))+min(mydata2$NOt.1)
eval_results(test$NOt.1, PREDNO, test)

#co
nnco = neuralnet(COt.1~Density+Small+Heavy+TEMPt+wst+COt, data = trainNN, hidden = 3, linear.output = TRUE)
plot(nnco)
nnco$weights

predict_testNN = compute(nnco,testNN)
PREDCO = (predict_testNN$net.result * (max(mydata2$COt.1)-min(mydata2$COt.1)))+min(mydata2$COt.1)
eval_results(test$COt.1, PREDCO, test)

#co2
nnco2 = neuralnet(CO2t.1~Density+Small+Heavy+TEMPt+wst+CO2t, data = trainNN, hidden = 3, linear.output = TRUE)
plot(nnco2)
nnco2$weights

predict_testNN = compute(nnco2,testNN)
PREDCO2 = (predict_testNN$net.result * (max(mydata2$CO2t.1)-min(mydata2$CO2t.1)))+min(mydata2$CO2t.1)
eval_results(test$CO2t.1, PREDCO2, test)

#o3
nno3 = neuralnet(O3t.1~Density+Small+Heavy+TEMPt+wst+O3t, data = trainNN, hidden = 3, linear.output = TRUE)
plot(nno3)
nno3$weights

predict_testNN = compute(nno3,testNN)
PREDO3 = (predict_testNN$net.result * (max(mydata2$O3t.1)-min(mydata2$O3t.1)))+min(mydata2$O3t.1)
eval_results(test$O3t.1, PREDO3, test)

#pm25
nnpm25 = neuralnet(PM2.5t.1~Density+Small+Heavy+TEMPt+wst+PM2.5t, data = trainNN, hidden = 3, linear.output = TRUE)
plot(nnpm25)
nnpm25$weights

predict_testNN = compute(nnpm25,testNN)
PREDPM25 = (predict_testNN$net.result * (max(mydata2$PM2.5t.1)-min(mydata2$PM2.5t.1)))+min(mydata2$PM2.5t.1)
eval_results(test$PM2.5t.1, PREDPM25, test)

#plots
test3=data.frame(test,PREDCO2,PREDCO,PREDNO,PREDNO2,PREDO3,PREDPM25)
library(ggplot2)

NNco2 <- ggplot(test3, aes(CO2t.1,PREDCO2))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNco2

NNco <- ggplot(test3, aes(COt.1,PREDCO))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNco

NNno2 <- ggplot(test3, aes(NO2t.1,PREDNO2))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNno2

NNno <- ggplot(test3, aes(NOt.1,PREDNO))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNno

NNo3 <- ggplot(test3, aes(O3t.1,PREDO3))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNo3 

NNpm25 <- ggplot(test3, aes(PM2.5t.1,PREDPM25))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNpm25

library(ggpubr)
NNsummary <- ggarrange(NNco,NNco2,NNno,NNno2,NNo3,NNpm25,
                       labels = c("CO","CO2","NO","NO2","O3","PM2.5"),
                       ncol = 2, nrow = 3)
NNsummary
