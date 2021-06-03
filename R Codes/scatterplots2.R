mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)

summary(mydata2$flow)
Group.flow = ifelse(mydata2$flow >= 8729,"4th Quartile", ifelse(mydata2$flow >=8209, "3rd Quartile", ifelse(mydata2$flow >=7527,"2nd Quartile","1st Quartile")))
Group.flow = as.factor(Group.flow)
mydata2 = cbind(mydata2,Group.flow)
head(mydata2)

install.packages("ggplot2")
library(ggplot2)

ggplot(mydata2,
       aes(x = flow,
           y = no2,
           color = Group.flow))+
  geom_point() +
  labs(title = "NO2 vs. Flow")

ggplot(mydata2,
       aes(x = flow, y = no))+
         geom_boxplot(outlier.shape = NA)+
         facet_wrap(~Group.flow, ncol = 4) +
         labs(title = "NO vs. Flow")

ggplot(mydata2,
       aes(x = flow, y = no,color = Group.flow))+
  geom_boxplot()+
  facet_wrap(~Group.flow, ncol = 4)+
  labs(title = "NO vs. Flow")

ggplot(mydata2,
       aes(x = flow, y = no2,color = Group.flow))+
  geom_boxplot()+
  labs(title = "NO2 vs. Flow")

ggplot(mydata2,
       aes(x = flow, y = co2,color = Group.flow))+
  geom_boxplot()+
  labs(title = "CO2 vs. Flow")

ggplot(mydata2,
       aes(x = flow, y = co,color = Group.flow))+
  geom_boxplot()+
  labs(title = "CO vs. Flow")

ggplot(mydata2,
       aes(x = flow, y = o3,color = Group.flow))+
  geom_boxplot()+
  labs(title = "O3 vs. Flow")

ggplot(mydata2,
       aes(x = flow, y = pm25,color = Group.flow))+
  geom_boxplot()+
  labs(title = "PM2.5 vs. Flow")
