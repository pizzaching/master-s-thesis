mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)
mydata2=mydata2[,-c(1,2,7,17,18,19,20,21,22,23,24,25,26)]
mydata2=mydata2[,-c(1)]
summary(lm(O3~Density+Speed+Small+Heavy+RH+TEMP+WS_R,data = training))

plot(mydata2$Speed,mydata2$CO)
install.packages("dplyr")
library(dplyr)
mydata2 <- mydata2 %>% mutate_all(~(scale(.) %>% as.vector))
mydata2

  
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
MLMCO2=lm(CO2~.,training2)
summary(MLMCO2)


PREDCO2<-predict(MLMCO2,newdata=test2)
PREDCO2 = PREDCO2* (max(mydata2$CO2)-min(mydata2$CO2))+min(mydata2$CO2)
MSECO2<-mean((PREDCO2-test$CO2)^2)
MAPECO2<-mean(abs((PREDCO2-test$CO2)/test$CO2))
MSECO2
MAPECO2

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

eval_results(test$CO2, PREDCO2, test)


#CO
MLMCO=lm(CO~.,data=training2)
summary(MLMCO)

PREDCO<-predict(MLMCO,newdata=test2)
PREDCO = PREDCO* (max(mydata2$CO)-min(mydata2$CO))+min(mydata2$CO)

eval_results(test$CO, PREDCO, test)

MSECO<-mean((PREDCO-test$co)^2)
MAPECO<-mean(abs((PREDCO-test$co)/test$co))
MSECO
MAPECO


#NO
MLMNO=lm(NO~.,data=training2)
summary(MLMNO)

PREDNO<-predict(MLMNO,newdata=test2)
PREDNO = PREDNO* (max(mydata2$NO)-min(mydata2$NO))+min(mydata2$NO)
eval_results(test$NO, PREDNO, test)

MSENO<-mean((PREDNO-test$no)^2)
MAPENO<-mean(abs((PREDNO-test$no)/test$no))
MSENO
MAPENO


#NO2
MLMNO2=lm(NO2~.,data=training2)
summary(MLMNO2)

PREDNO2<-predict(MLMNO2,newdata=test2)
PREDNO2 = PREDNO2* (max(mydata2$NO2)-min(mydata2$NO2))+min(mydata2$NO2)
eval_results(test$NO2, PREDNO2, test)

MSENO2<-mean((PREDNO2-test$no2)^2)
MAPENO2<-mean(abs((PREDNO2-test$no2)/test$no2))
MSENO2
MAPENO2

#O3
MLMO3=lm(O3~.,data=training2)
summary(MLMO3)

PREDO3<-predict(MLMO3,newdata=test2)
PREDO3 = PREDO3* (max(mydata2$O3)-min(mydata2$O3))+min(mydata2$O3)
eval_results(test$O3, PREDO3, test)

MSEO3<-mean((PREDO3-test$o3)^2)
MAPEO3<-mean(abs((PREDO3-test$o3)/test$o3))
MSEO3
MAPEO3

#PM25
MLMPM25=lm(PM2.5~.,data=training2)
summary(MLMPM25)

PREDPM25<-predict(MLMPM25,newdata=test2)
PREDPM25 = PREDPM25* (max(mydata2$PM2.5)-min(mydata2$PM2.5))+min(mydata2$PM2.5)
eval_results(test$PM2.5, PREDPM25, test)

MSEPM25<-mean((PREDPM25-test$pm25)^2)
MAPEPM25<-mean(abs((PREDPM25-test$pm25)/test$pm25))
MSEPM25
MAPEPM25



str(mydata2)
mydata3=mydata2[,-c(14,15,16,17,18,19)]

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(mydata2, histogram=TRUE, pch=19)

plot(test$CO2,PREDCO2,col = "",pch = 19 ,xlab = "Actual CO2",ylab = "Predicted CO2")
abline(0,1)
install.packages("ggplot2")
library(ggplot2)

test3=data.frame(test,PREDCO2,PREDCO,PREDNO,PREDNO2,PREDO3,PREDPM25)

linearco2 <- ggplot(test3, aes(CO2,PREDCO2))+
  geom_point(color = "#009999") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)

linearco <- ggplot(test3, aes(CO,PREDCO))+
  geom_point(color = "#009999") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
linearco

linearno2 <- ggplot(test3, aes(NO2,PREDNO2))+
  geom_point(color = "#009999") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
linearno2

linearno <- ggplot(test3, aes(NO,PREDNO))+
  geom_point(color = "#009999") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
linearno

linearo3 <- ggplot(test3, aes(O3,PREDO3))+
  geom_point(color = "#009999") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
linearo3

linearpm25 <- ggplot(test3, aes(PM2.5,PREDPM25))+
  geom_point(color = "#009999") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
linearpm25

library(ggpubr)
linearsummary <- ggarrange(linearco,linearco2,linearno,linearno2,linearo3,linearpm25,
                           labels = c("CO","CO2","NO","NO2","O3","PM2.5"),
                           ncol = 2, nrow = 3)
linearsummary
