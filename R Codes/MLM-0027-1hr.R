mydata1=read.csv(file.choose(),header=T)
mydata1=mydata1[,-c(1,2,3,4,5,6,17,18,19)]
mydata2=na.omit(mydata1)
# the above line remove all "NA" in the csv
head(mydata2)
str(mydata2)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(mydata2, histogram=TRUE, pch=19)        

MLMCO=lm(co~humidity+temperature+Hourly.Flow+Density+Speed,data=mydata2)
summary(MLMCO)

MLMCO2=lm(co2~.,data=mydata2)
summary(MLMCO2)

MLMNO2=lm(no2~.,data=mydata2)
summary(MLMNO2)

MLMPM25=lm(pm25~.,data=mydata2)
summary(MLMPM25)

MLMSO2=lm(so2~.,data=mydata2)
summary(MLMSO2)

MLMO3=lm(o3~.,data=mydata2)
summary(MLMO3)

MLMNO=lm(no~.,data=mydata2)
summary(MLMNO)

set.seed(1)
index = sample(1:nrow(mydata2),round(0.70*nrow(mydata2)))
# usually the training set should be >= test set. In my case, I use 70%/30%. You can use other splitting.
training=mydata2[index,]
dim(training)
test=mydata2[-index,]
dim(test)

MLMCO=lm(co~humidity+temperature+Hourly.Flow+Density+Speed,data=training)
summary(MLMCO)

MLMCO2=lm(co2~humidity+temperature+Hourly.Flow+Density+Speed,data=training)
summary(MLMCO2)

MLMNO2=lm(no2~humidity+temperature+Hourly.Flow+Density+Speed,data=training)
summary(MLMNO2)

MLMPM25=lm(pm25~humidity+temperature+Hourly.Flow+Density+Speed,data=training)
summary(MLMPM25)

MLMSO2=lm(so2~humidity+temperature+Hourly.Flow+Density+Speed,data=training)
summary(MLMSO2)

MLMO3=lm(o3~humidity+temperature+Hourly.Flow+Density+Speed,data=training)
summary(MLMO3)

MLMNO=lm(no~humidity+temperature+Hourly.Flow+Density+Speed,data=training)
summary(MLMNO)

predict_mlmCO <- predict(MLMCO, newdata=test)
# the formula is predict(developed model, newdata=.)
MSECO <- sum((predict_mlmCO - test$co)^2)/nrow(test)
MSECO
(MSECO)^0.5/mean(test$co)

predict_mlmCO2 <- predict(MLMCO2, newdata=test)
# the formula is predict(developed model, newdata=.)
MSECO2 <- sum((predict_mlmCO2 - test$co2)^2)/nrow(test)
MSECO2
(MSECO2)^0.5/mean(test$co2)

predict_mlmNO2 <- predict(MLMNO2, newdata=test)
# the formula is predict(developed model, newdata=.)
MSENO2 <- sum((predict_mlmNO2 - test$no2)^2)/nrow(test)
MSENO2
(MSENO2)^0.5/mean(test$no2)

predict_mlmNO <- predict(MLMNO, newdata=test)
# the formula is predict(developed model, newdata=.)
MSENO <- sum((predict_mlmNO - test$no)^2)/nrow(test)
MSENO
(MSENO)^0.5/mean(test$no)

predict_mlmO3 <- predict(MLMO3, newdata=test)
# the formula is predict(developed model, newdata=.)
MSEO3 <- sum((predict_mlmO3 - test$o3)^2)/nrow(test)
MSEO3
(MSEO3)^0.5/mean(test$o3)

predict_mlmSO2 <- predict(MLMSO2, newdata=test)
# the formula is predict(developed model, newdata=.)
MSESO2 <- sum((predict_mlmSO2 - test$so2)^2)/nrow(test)
MSESO2
(MSESO2)^0.5/mean(test$so2)

predict_mlmPM25 <- predict(MLMPM25, newdata=test)
# the formula is predict(developed model, newdata=.)
MSEPM25 <- sum((predict_mlmPM25 - test$pm25)^2)/nrow(test)
MSEPM25
(MSEPM25)^0.5/mean(test$pm25)






