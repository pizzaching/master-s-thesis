mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)
mydata2=mydata2[,-c(1,2,7,17,18,19,20,21,22,23,24,25,26)]
mydata2=mydata2[,-c(1)]

set.seed(100)
index=sample(1:nrow(mydata2),round(0.70*nrow(mydata2)))
training=mydata2[index,]
test=mydata2[-index,]
dim(training)
dim(test)

max = apply(mydata2,2,max)
min = apply(mydata2,2,min)
scaled = as.data.frame(scale(mydata2, center = min, scale = max-min))

training2 = scaled[index,]
test2 = scaled[-index,]

#CO2
MLMCO2=lm(CO2t.1~Density+Small+Heavy+TEMPt+wst+CO2t,training)
summary(MLMCO2)


PREDCO2<-predict(MLMCO2,newdata=test)



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

eval_results(test$CO2t.1, PREDCO2, test)


#CO
MLMCO=lm(COt.1~Density+Small+Heavy+TEMPt+wst+COt,data=training)
summary(MLMCO)

PREDCO<-predict(MLMCO,newdata=test)


eval_results(test$COt.1, PREDCO, test)

#NO
MLMNO=lm(NOt.1~Density+Small+Heavy+TEMPt+wst+NOt,data=training)
summary(MLMNO)

PREDNO<-predict(MLMNO,newdata=test)
PREDNO = PREDNO* (max(mydata2$NOt.1)-min(mydata2$NOt.1))+min(mydata2$NOt.1)
eval_results(test$NOt.1, PREDNO, test)

#NO2
MLMNO2=lm(NO2t.1~Density+Small+Heavy+TEMPt+wst+NO2t,data=training)
summary(MLMNO2)

PREDNO2<-predict(MLMNO2,newdata=test)
PREDNO2 = PREDNO2* (max(mydata2$NO2t.1)-min(mydata2$NO2t.1))+min(mydata2$NO2t.1)
eval_results(test$NO2t.1, PREDNO2, test)


#O3
MLMO3=lm(O3t.1~Density+Small+Heavy+TEMPt+wst+O3t,data=training)
summary(MLMO3)

PREDO3<-predict(MLMO3,newdata=test)
PREDO3 = PREDO3* (max(mydata2$O3t.1)-min(mydata2$O3t.1))+min(mydata2$O3t.1)
eval_results(test$O3t.1, PREDO3, test)

#PM25
MLMPM25=lm(PM2.5t.1~Density+Small+Heavy+TEMPt+wst+PM2.5t,data=training)
summary(MLMPM25)

PREDPM25<-predict(MLMPM25,newdata=test)

eval_results(test$PM2.5t.1, PREDPM25, test)



install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(mydata2, histogram=TRUE, pch=19)

plot(test$CO2,PREDCO2,col = "",pch = 19 ,xlab = "Actual CO2",ylab = "Predicted CO2")
abline(0,1)
install.packages("ggplot2")
library(ggplot2)

test3=data.frame(test,PREDCO2,PREDCO,PREDNO,PREDNO2,PREDO3,PREDPM25)

linearco2 <- ggplot(test3, aes(CO2t.1,PREDCO2))+
  geom_point(color = "#009999") +labs(x = "Observed CO2(t+1)", y = "Predicted CO2(t+1)")+
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)

linearco <- ggplot(test3, aes(COt.1,PREDCO))+
  geom_point(color = "#009999") +labs(x = "Observed CO(t+1)", y = "Predicted CO(t+1)")+
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
linearco

linearno2 <- ggplot(test3, aes(NO2t.1,PREDNO2))+
  geom_point(color = "#009999") + labs(x = "Observed NO2(t+1)", y = "Predicted NO2(t+1)")+
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
linearno2

linearno <- ggplot(test3, aes(NOt.1,PREDNO))+
  geom_point(color = "#009999") +labs(x = "Observed NO(t+1)", y = "Predicted NO(t+1)")+
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
linearno

linearo3 <- ggplot(test3, aes(O3t.1,PREDO3))+
  geom_point(color = "#009999") +labs(x = "Observed O3(t+1)", y = "Predicted O3(t+1)")+
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
linearo3

linearpm25 <- ggplot(test3, aes(PM2.5t.1,PREDPM25))+
  geom_point(color = "#009999") +labs(x = "Observed PM2.5(t+1)", y = "Predicted PM2.5(t+1)")+
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
linearpm25

library(ggpubr)
linearsummary <- ggarrange(linearco,linearco2,linearno,linearno2,linearo3,linearpm25,
                           labels = c("CO","CO2","NO","NO2","O3","PM2.5"),
                           ncol = 2, nrow = 3)
linearsummary
