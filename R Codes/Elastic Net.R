mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)
mydata2=mydata2[,-c(1,2,7,17,18,19,20,21,22,23,24)]

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

#regularization of data that panalizes large coefficients
cols_reg = c('flow','density','speed','small','heavy','truck_prop','co2','pm25','pm10',
             'co','no','no2','o3','humidity','temperature','WS_R','WD_R')
dummies <- dummyVars(o3~.,data = mydata2[,cols_reg])
train_dummies = predict(dummies,newdata = train[,cols_reg])
test_dummies = predict(dummies,newdata = test[,cols_reg])
print(dim(train_dummies));print(dim(test_dummies))

#or
x <- model.matrix(o3~.,train)[,-1]
y_train <- train$o3
x_test <- model.matrix(o3~.,test)[,-1]
y_test <- test$o3

#create the training and test data matrices for x and y
x = as.matrix(train_dummies)
y_train = train$o3
x_test = as.matrix(test_dummies)
y_test = test$o3
#
#
#
#building the model using the training set
model <- train(no~., data = train, method = "glmnet",
               trControl = trainControl("cv", number = 10),
               tuneLength = 10
               )

#identify the best tuning parameter
model$bestTune
coef(model$finalModel,model$bestTune$lambda)

#Make predictions on the test data
x_test <- model.matrix(no~., test)[,-1]
predictions <- model %>% predict(x_test)

#Performance metrics
MAPE <- mean(abs(predictions - test$no)/test$no)
data.frame(
  RMSE = RMSE(predictions, test$no),
  Rsquare = R2(predictions, test$no),
  MAPE
)

#compute R^2 from the true and predicted values
eval_results<-function(true, predicted, df) {
  SSE<-sum((predicted - true)^2)
  SST<-sum((true - mean(true))^2)
  R_square <- 1 - SSE / SST
  RMSE <- sqrt(SSE/nrow(df))
  MAP <- sum(abs((predicted - true)/true))/nrow(df)
  
  #Model performance metrics
  data.frame(
    RMSE = RMSE,
    Rsquare = R_square,
    MAPE = MAP
  )
  
}






eval_results(y_train, predictions_train, train)
eval_results(y_test, predictions_test, test)

#inspect beta coefficients
coef(lasso_model)

#plot coefficients
lbs_fun <- function(fit, offset_x)  {
  L <- length(fit$lambda)
  x_var <- log(fit$lambda[L]) + offset_x
  v_var <- fit$beta[,L]
  labs <- names(y_var)
  text (x_var, y_var, labels = labs)
}

lasso <- glmnet(scale(x),y_train, alpha = 1)
plot(lasso, xvar = "lambda", label = T)
lbs_fun(lasso_reg, offset_x = 1)
abline(v=lasso_reg$lambda.min, col = "red", lty = 2)
abline(v=lasso_reg$lambda.1se, col = "blue", lty = 2)

