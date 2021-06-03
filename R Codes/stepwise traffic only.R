mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)
mydata2=mydata2[,-c(1,2,7,17,18,19,20,21,22,23,24,25,26)]

install.packages("caret")
library(caret)

set.seed(3)
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

#set up repeated l-fold cross-validation
train.control <- trainControl(method = "cv", number = 10)

#train the model CO2
step.co2 <- train(CO2t.1~Density+Small+Heavy+TEMPt+wst+CO2t, data = training,
                  method = "leapBackward",
                  tuneGrid = data.frame(nvmax = 1:6),
                  trControl = train.control)
step.co2$results
step.co2$bestTune
summary(step.co2$finalModel)
coef(step.co2$finalModel,4)
STEPCO2 <- lm(CO2t.1~Density+Small+TEMPt+CO2t,data=training)
summary(STEPCO2)

PREDCO2<-predict(STEPCO2,newdata=test)

eval_results(test$CO2t.1, PREDCO2, test)

#train the model CO
step.co <- train(COt.1~Density+Small+Heavy+TEMPt+wst+COt, data = training,
                 method = "leapBackward",
                 tuneGrid = data.frame(nvmax = 1:6),
                 trControl = train.control)
step.co$results
step.co$bestTune
summary(step.co$finalModel)
coef(step.co$finalModel,5)
STEPCO <- lm(COt.1~Density+Small+Heavy+TEMPt+COt,data=training)
summary(STEPCO)

PREDCO<-predict(STEPCO,newdata=test)

eval_results(test$COt.1, PREDCO, test)


#train the model NO
step.no <- train(NOt.1~Density+Small+Heavy+TEMPt+wst+NOt, data = training,
                 method = "leapBackward",
                 tuneGrid = data.frame(nvmax = 1:6),
                 trControl = train.control)
step.no$results
step.no$bestTune
summary(step.no$finalModel)
coef(step.no$finalModel,4)
STEPNO <- lm(NOt.1~Small+TEMPt+wst+NOt,data=training)
summary(STEPNO)

PREDNO<-predict(STEPNO,newdata=test)

eval_results(test$NOt.1, PREDNO, test)

#train the model NO2
step.no2 <- train(NO2t.1~Density+Small+Heavy+TEMPt+wst+NO2t, data = training,
                  method = "leapBackward",
                  tuneGrid = data.frame(nvmax = 1:6),
                  trControl = train.control)
step.no2$results
step.no2$bestTune
summary(step.no2$finalModel)
coef(step.no2$finalModel,4)
STEPNO2 <- lm(NO2t.1~Density+Heavy+TEMPt+NO2t,data=training)
summary(STEPNO2)

PREDNO2<-predict(STEPNO2,newdata=test)

eval_results(test$NO2t.1, PREDNO2, test)

#train the model O3
step.o3 <- train(O3t.1~Density+Small+Heavy+TEMPt+wst+O3t, data = training,
                 method = "leapBackward",
                 tuneGrid = data.frame(nvmax = 1:6),
                 trControl = train.control)
step.o3$results
step.o3$bestTune
summary(step.o3$finalModel)
coef(step.o3$finalModel,5)
STEPO3 <- lm(O3t.1~Density+Small+TEMPt+wst+O3t,data=training)
summary(STEPO3)

PREDO3<-predict(STEPO3,newdata=test)

eval_results(test$O3t.1, PREDO3, test)

#train the model PM2.5
step.pm25 <- train(PM2.5t.1~Density+Small+Heavy+TEMPt+wst+PM2.5t, data = training,
                   method = "leapBackward",
                   tuneGrid = data.frame(nvmax = 1:6),
                   trControl = train.control)
step.pm25$results
step.pm25$bestTune
summary(step.pm25$finalModel)
coef(step.pm25$finalModel,1)
STEPPM25 <- lm(PM2.5t.1~PM2.5t,data=training)
summary(STEPPM25)

PREDPM25<-predict(STEPPM25,newdata=test)

eval_results(test$PM2.5t.1, PREDPM25, test)

#plots
test3=data.frame(test,PREDCO2,PREDCO,PREDNO,PREDNO2,PREDO3,PREDPM25)

stepco2 <- ggplot(test3, aes(CO2t.1,PREDCO2))+
  geom_point(color = "#ffaa00") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
stepco2

stepco <- ggplot(test3, aes(COt.1,PREDCO))+
  geom_point(color = "#ffaa00") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
stepco

stepno2 <- ggplot(test3, aes(NO2t.1,PREDNO2))+
  geom_point(color = "#ffaa00") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
stepno2

stepno <- ggplot(test3, aes(NOt.1,PREDNO))+
  geom_point(color = "#ffaa00") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
stepno

stepo3 <- ggplot(test3, aes(O3t.1,PREDO3))+
  geom_point(color = "#ffaa00") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
stepo3 

steppm25 <- ggplot(test3, aes(PM2.5t.1,PREDPM25))+
  geom_point(color = "#ffaa00") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
steppm25

library(ggpubr)
stepsummary <- ggarrange(stepco,stepco2,stepno,stepno2,stepo3,steppm25,
                         labels = c("CO","CO2","NO","NO2","O3","PM2.5"),
                         ncol = 2, nrow = 3)
stepsummary
