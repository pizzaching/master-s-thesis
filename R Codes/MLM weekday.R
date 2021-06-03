mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)
mydata2=mydata2[,-c(1,15,22,23,24,25,26)]

set.seed(1)
index=sample(1:nrow(mydata2),round(0.70*nrow(mydata2)))
training=mydata2[index,]
test=mydata2[-index,]
dim(training)
dim(test)

#CO2
MLMCO2=lm(co2~.,data=training)
summary(MLMCO2)

PREDCO2<-predict(MLMCO2,newdata=test)
MSECO2<-mean((PREDCO2-test$co2)^2)
MAPECO2<-mean(abs((PREDCO2-test$co2)/test$co2))
MSECO2^0.5
MAPECO2

MLMCO2_R<-lm(co2~Flow+Density+Speed+Truck.Flow..Veh.Hour.+pm25+co+no+no2++o3+temperature+WS_R,data=training)
summary(MLMCO2_R)

PREDCO2_R<-predict(MLMCO2_R,newdata=test)
MSECO2_R<-mean((PREDCO2_R-test$co2)^2)
MAPECO2_R<-mean(abs((PREDCO2_R-test$co2)/test$co2))
MSECO2_R
MAPECO2_R

#CO
MLMCO=lm(co~.,data=training)
summary(MLMCO)

PREDCO<-predict(MLMCO,newdata=test)
MSECO<-mean((PREDCO-test$co)^2)
MAPECO<-mean(abs((PREDCO-test$co)/test$co))
MSECO^0.5
MAPECO

MLMCO_R<-lm(co~Flow+Density+Speed+Truck.Flow..Veh.Hour.+co2+no2+no+o3+humidity+temperature+WS_R,data=training)
summary(MLMCO_R)

PREDCO_R<-predict(MLMCO_R,newdata=test)
MSECO_R<-mean((PREDCO_R-test$co)^2)
MAPECO_R<-mean(abs((PREDCO_R-test$co)/test$co))
MSECO_R
MAPECO_R

#NO
MLMNO=lm(no~.,data=training)
summary(MLMNO)

PREDNO<-predict(MLMNO,newdata=test)
MSENO<-mean((PREDNO-test$no)^2)
MAPENO<-mean(abs((PREDNO-test$no)/test$no))
MSENO^0.5
MAPENO

MLMNO_R<-lm(no~Flow+Density+Speed+Truck.Flow..Veh.Hour.+co2+pm25+pm10+o3+WS_R+WD_R,data=training)
summary(MLMNO_R)

PREDNO_R<-predict(MLMNO_R,newdata=test)
MSENO_R<-mean((PREDNO_R-test$no)^2)
MAPENO_R<-mean(abs((PREDNO_R-test$no)/test$no))
MSENO_R
MAPENO_R

#NO2
MLMNO2=lm(no2~.,data=training)
summary(MLMNO2)

PREDNO2<-predict(MLMNO2,newdata=test)
MSENO2<-mean((PREDNO2-test$no2)^2)
MAPENO2<-mean(abs((PREDNO2-test$no2)/test$no2))
MSENO2^0.5
MAPENO2

MLMNO2_R<-lm(no2~Flow+Density+Speed+Truck.Flow..Veh.Hour.+co2+co+o3+humidity+temperature+sin.WD.+cos.WD.,data=training)
summary(MLMNO2_R)

PREDNO2_R<-predict(MLMNO2_R,newdata=test)
MSENO2_R<-mean((PREDNO2_R-test$no2)^2)
MAPENO2_R<-mean(abs((PREDNO2_R-test$no2)/test$no2))
MSENO2_R
MAPENO2_R

#O3
MLMO3=lm(o3~.,data=training)
summary(MLMO3)

PREDO3<-predict(MLMO3,newdata=test)
MSEO3<-mean((PREDO3-test$o3)^2)
MAPEO3<-mean(abs((PREDO3-test$o3)/test$o3))
MSEO3^0.5
MAPEO3

MLMO3_R<-lm(o3~Flow+Density+Speed+Truck.Flow..Veh.Hour.+co2+pm25+pm10+co+no+no2+o3+humidity+temperature+WD_R,data=training)
summary(MLMO3_R)

PREDO3_R<-predict(MLMO3_R,newdata=test)
MSEO3_R<-mean((PREDO3_R-test$o3)^2)
MAPEO3_R<-mean(abs((PREDO3_R-test$o3)/test$o3))
MSEO3_R
MAPEO3_R

#PM25
MLMPM25=lm(pm25~.,data=training)
summary(MLMPM25)

PREDPM25<-predict(MLMPM25,newdata=test)
MSEPM25<-mean((PREDPM25-test$pm25)^2)
MAPEPM25<-mean(abs((PREDPM25-test$pm25)/test$pm25))
MSEPM25^0.5
MAPEPM25

MLMPM25_R<-lm(pm25~Flow+Density+Speed+Truck.Flow..Veh.Hour.+co+no+no2+o3+humidity+temperature+WS_R,data=training)
summary(MLMO3_R)

PREDPM25_R<-predict(MLMPM25_R,newdata=test)
MSEPM25_R<-mean((PREDPM25_R-test$pm25)^2)
MAPEPM25_R<-mean(abs((PREDPM25_R-test$pm25)/test$pm25))
MSEPM25_R
MAPEPM25_R

str(mydata2)
mydata3=mydata2[,-c(14,15,16,17,18,19)]

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(mydata3, histogram=TRUE, pch=19)
