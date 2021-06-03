mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)
mydata2=mydata2[,-c(1,2,7,17,18,19,20,21,22,23,24,25,26)]
mydata2=mydata2[,-c(1,2,7,18,19,21,25)]
dim(mydata2)
install.packages("neuralnet")
library(neuralnet)

require(neuralnet)

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

nnno2 = neuralnet(NO2t.1~Density+Speed+Small+Heavy+RHt.1+TEMPt.1+wst.1+NO2t, data = trainNN, hidden = 3, linear.output = TRUE)
plot(nnno2)
nnno2$weights

predict_testNN = compute(nnno2,testNN)
PREDNO2 = (predict_testNN$net.result * (max(mydata2$NO2t.1)-min(mydata2$NO2t.1)))+min(mydata2$NO2t.1)


eval_results(test$NO2t.1, PREDNO2, test)

plot(test$CO2,predict_testNN, pch = 16)
abline(0,1)

#plots
test3=data.frame(test,PREDCO2,PREDCO,PREDNO,PREDNO2,PREDO3,PREDPM25)

NNco2 <- ggplot(test3, aes(CO2,PREDCO2))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNco2

NNco <- ggplot(test3, aes(CO,PREDCO))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNco

NNno2 <- ggplot(test3, aes(NO2t.1,PREDNO2))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNno2

NNno <- ggplot(test3, aes(NO,PREDNO))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNno

NNo3 <- ggplot(test3, aes(O3,PREDO3))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNo3 

NNpm25 <- ggplot(test3, aes(PM2.5,PREDPM25))+
  geom_point(color = "#E40045") +
  geom_abline(intercept = 0, slope = 1, color = "#0D58A6", size = 2)
NNpm25

library(ggpubr)
NNsummary <- ggarrange(NNco,NNco2,NNno,NNno2,NNo3,NNpm25,
                         labels = c("CO","CO2","NO","NO2","O3","PM2.5"),
                         ncol = 2, nrow = 3)
NNsummary
