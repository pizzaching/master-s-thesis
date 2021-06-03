mydata1=read.csv(file.choose(),header=T)
head(mydata1)
mydata2=na.omit(mydata1)
str(mydata2)
mydata2=mydata2[,-c(1,15,23)]

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
chart.Correlation(mydata2, histogram=TRUE, pch=19)

install.packages("GGally")
library(GGally)
ggpairs(mydata2)

install.packages("corrplot")
library(corrplot)
M<-cor(mydata2)
head(M)
corrplot(M,type="upper",method="number")

OzoneClass=ifelse(mydata2$AQI_O<=50,"Good",ifelse(mydata2$AQI_O<=100,"Moderate",ifelse(mydata2$AQI_O<=150,"UnhealthySen",ifelse(mydata2$AQI_O<=200,"Unhealthy","VeryUnhealthy"))))
OzoneClass=as.factor(OzoneClass)
mydata2=cbind(mydata2,OzoneClass)
head(mydata2)

install.packages(c("tree","ISLR"))
library(tree)
library(ISLR)


set.seed(1) 
train=sample (1: nrow(mydata2), dim(mydata2)[1]/2) 
set.seed(1)
index=sample (1:nrow(mydata2),round(0.70*nrow(mydata2)))
training=mydata2[index,]
test=mydata2[-index,]
dim(training)
dim(test)


tree.o3=tree(o3???Density+Speed+Small+Truck.Flow..Veh.Hour.+humidity+temperature+WS_R+sin.WD.,mydata2 ,subset=index) 
summary (tree.o3)
plot(tree.o3)
text(tree.o3,pretty=0) 

o3.yhat=predict(tree.o3,newdata=mydata2[-index,]) 
o3.test=mydata2[-index ,"o3"] 
plot(o3.yhat ,o3.test)
abline (0,1) 
o3.MSE<-mean((o3.yhat-o3.test)^2)
o3.MSE
o3.MSE^0.5
mean(abs((o3.yhat-o3.test)/o3.test))

tree.co2=tree(co2???Density+Speed+Small+Truck.Flow..Veh.Hour.+humidity+temperature+WS_R+sin.WD.,mydata2 ,subset=index) 
summary (tree.co2)
plot(tree.co2)
text(tree.co2,pretty=0) 
co2.yhat=predict(tree.co2,newdata=mydata2[-index,]) 
co2.test=mydata2[-index ,"co2"] 
plot(co2.yhat ,co2.test)
abline (0,1) 
co2.MSE<-mean((co2.yhat-co2.test)^2)
co2.MSE
co2.MSE^0.5
mean(abs((co2.yhat-co2.test)/co2.test))

tree.co=tree(co???Density+Speed+Small+Truck.Flow..Veh.Hour.+humidity+temperature+WS_R+sin.WD.,mydata2 ,subset=index) 
summary (tree.co)
plot(tree.co)
text(tree.co,pretty=0) 
co.yhat=predict(tree.co,newdata=mydata2[-index,]) 
co.test=mydata2[-index ,"co"] 
plot(co.yhat ,co.test)
abline (0,1) 
co.MSE<-mean((co.yhat-co.test)^2)
co.MSE
co.MSE^0.5
mean(abs((co.yhat-co.test)/co.test))



tree.no=tree(no???Density+Speed+Small+Truck.Flow..Veh.Hour.+humidity+temperature+WS_R+sin.WD.,mydata2 ,subset=index) 
summary (tree.no)
plot(tree.no)
text(tree.no,pretty=0) 
no.yhat=predict(tree.no,newdata=mydata2[-index,]) 
no.test=mydata2[-index ,"no"] 
plot(no.yhat ,no.test)
abline (0,1) 
no.MSE<-mean((no.yhat-no.test)^2)
no.MSE
no.MSE^0.5
mean(abs((no.yhat-no.test)/no.test))

tree.no2=tree(no2???Density+Speed+Small+Truck.Flow..Veh.Hour.+humidity+temperature+WS_R+sin.WD.,mydata2 ,subset=index) 
summary (tree.no2)
plot(tree.no2)
text(tree.no2,pretty=0) 
no2.yhat=predict(tree.no2,newdata=mydata2[-index,]) 
no2.test=mydata2[-index ,"no2"] 
plot(no2.yhat ,no2.test)
abline (0,1) 
no2.MSE<-mean((no2.yhat-no2.test)^2)
no2.MSE
no2.MSE^0.5
mean(abs((no2.yhat-no2.test)/no2.test))

tree.pm25=tree(pm25???Density+Speed+Small+Truck.Flow..Veh.Hour.+humidity+temperature+WS_R+sin.WD.,mydata2 ,subset=index) 
summary (tree.pm25)
plot(tree.pm25)
text(tree.pm25,pretty=0) 
pm25.yhat=predict(tree.pm25,newdata=mydata2[-index,]) 
pm25.test=mydata2[-index ,"pm25"] 
plot(pm25.yhat ,pm25.test)
abline (0,1) 
pm25.MSE<-mean((pm25.yhat-pm25.test)^2)
pm25.MSE
pm25.MSE^0.5
mean(abs((pm25.yhat-pm25.test)/pm25.test))




install.packages("randomForest")
library(randomForest)
rf.ozone=randomForest(o3~.,data=mydata2,subset=train,mtry=5,importance=TRUE)
tree.pred=predict(rf.ozone,mydata2[-train])
importance(rf.ozone)
varImpPlot((rf.ozone))
mean((tree.pred-rf.ozone)^2)
