mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)
mydata2=mydata2[,-c(1,2,7,17,18,19,20,21,22,23,24,25,26)]
mydata2=mydata2[,-c(1)]

#loading the library
install.packages(c("glmnet","readr","dplyr","caret","ggplot2","repr","dpldr","tidyverse"))
library(glmnet)
library(plyr)
library(readr)
library(caret)
library(ggplot2)
library(repr)
library(dplyr)
library(tidyverse)
glimpse(mydata2)


#splitting the data into test and train
set.seed(100)
index = sample(1:nrow(mydata2),nrow(mydata2)*0.7)


train = mydata2[index,]
test = mydata2[-index,]
dim(train)
dim(test)
summary(train)

set.seed(288)
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

#or
x <- model.matrix(PM2.5~.,training2)[,-1]
y_train <- training2$PM2.5
x_test <- model.matrix(PM2.5~.,test2)[,-1]
y_test <- test$PM2.5


#setting alpha = 1 implements lasso regression
lambdas<-10^seq(2,-3,by = -.1)
lasso_reg <- cv.glmnet(x, y_train, alpha = 1, lambda = lambdas)

#identify the best lambda
lambda_best<-lasso_reg$lambda.min
lambda_best

#rebuilding the model with best lambda value identified
lasso_model<-glmnet(x, y_train, alpha = 1, lambda = lambda_best)

predictions_train <- predict(lasso_model, s = lambda_best, newx = x)
predictions_train <- predictions_train * (max(mydata2$PM2.5)-min(mydata2$PM2.5))+min(mydata2$PM2.5)

predictions_test <- predict(lasso_model, s = lambda_best, newx = x_test)
PREDPM25 <- predictions_test * (max(mydata2$PM2.5)-min(mydata2$PM2.5))+min(mydata2$PM2.5)


eval_results(training$PM2.5, predictions_train, training)
eval_results(y_test, PREDPM25, test)

#inspect beta coefficients
coef(lasso_model)

#plots
test3=data.frame(test,PREDCO2,PREDCO,PREDNO,PREDNO2,PREDO3,PREDPM25)

lassoco2 <- ggplot(test3, aes(CO2,PREDCO2))+
  geom_point(color = "#9c6ad6") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
lassoco2

lassoco <- ggplot(test3, aes(CO,PREDCO))+
  geom_point(color = "#9c6ad6") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
lassoco

lassono2 <- ggplot(test3, aes(NO2,PREDNO2))+
  geom_point(color = "#9c6ad6") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
lassono2

lassono <- ggplot(test3, aes(NO,PREDNO))+
  geom_point(color = "#9c6ad6") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
lassono

lassoo3 <- ggplot(test3, aes(O3,PREDO3))+
  geom_point(color = "#9c6ad6") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
lassoo3 

lassopm25 <- ggplot(test3, aes(PM2.5,PREDPM25))+
  geom_point(color = "#9c6ad6") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
lassopm25

library(ggpubr)
lassosummary <- ggarrange(lassoco,lassoco2,lassono,lassono2,lassoo3,lassopm25,
                         labels = c("CO","CO2","NO","NO2","O3","PM2.5"),
                         ncol = 2, nrow = 3)
lassosummary



