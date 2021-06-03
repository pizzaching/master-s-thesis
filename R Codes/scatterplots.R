mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)

install.packages("ggplot2")
library(ggplot2)
install.packages("lattice")
library(lattice)
require(stats)
install.packages("dplyr")
library(dplyr)

summary(mydata2)
aggregate(CO2~Month,mydata2,mean)
a <- aggregate(Flow~time,mydata2,mean)
a
plot(aggregate(Heavy~time_local,mydata2,mean),
     type = "line", xlab = "Time of Day", ylab = "Flow")

ggplot(mydata2, aes(x=Density, y=CO))+geom_point()

ggplot(mydata2, aes(x = Time.of.Day, y= Flow))+geom_histogram()

hisCO2 <- ggplot(mydata2, aes(x = CO2))+
  geom_histogram(fill = "cornflowerblue",color = "white")+facet_wrap(~time_local,ncol = 6)
hisCO2

hisNO <- ggplot(mydata2, aes(x = NO, ,color = time_local))+
  geom_boxplot(fill = "#ffaa00")
hisNO

plotdataMonth <- mydata2 %>%
  group_by(month) %>%
  summarize(mean_CO2 = mean(CO2),mean_CO = mean(CO),mean_NO=mean(NO),mean_NO2=mean(NO2),mean_O3=mean(O3),mean_PM2.5=mean(PM2.5))

plotdataHour <- mydata2 %>%
  group_by(time) %>%
  summarize(mean_CO2 = mean(CO2),mean_CO = mean(CO),mean_NO=mean(NO),mean_NO2=mean(NO2),mean_O3=mean(O3),mean_PM2.5=mean(PM2.5))

plotFlow <- mydata2 %>%
  group_by(time,Day) %>%
  summarize(meanflow = mean(Flow),meantruck = mean(Heavy),meandensity=mean(Density),meanspeed=mean(Speed))
plotFlow
write.csv(plotFlow,file = "average flow2.csv")
#month summary
monthco2 <- ggplot(plotdataMonth, aes(x = month, y = mean_CO2))+
  geom_bar(stat = "identity", fill = "#009999", color = "white")+
  geom_text(aes(label = round(mean_CO2, digits = 2)), vjust = -0.25)
monthco2

monthco <- ggplot(plotdataMonth, aes(x = month, y = mean_CO))+
  geom_bar(stat = "identity", fill = "#ffaa00")+
  geom_text(aes(label = round(mean_CO, digits = 2)), vjust = -0.25)
monthco

monthno <- ggplot(plotdataMonth, aes(x = month, y = mean_NO))+
  geom_bar(stat = "identity", fill = "#0d58a6")+
  geom_text(aes(label = round(mean_NO, digits = 2)), vjust = -0.25)
monthno

monthno2 <- ggplot(plotdataMonth, aes(x = month, y = mean_NO2))+
  geom_bar(stat = "identity", fill = "#e40045")+
  geom_text(aes(label = round(mean_NO2, digits = 2)), vjust = -0.25)
monthno2

montho3 <- ggplot(plotdataMonth, aes(x = month, y = mean_O3))+
  geom_bar(stat = "identity", fill = "#9c6ad6")+
  geom_text(aes(label = round(mean_O3, digits = 2)), vjust = -0.25)
montho3

monthpm25 <- ggplot(plotdataMonth, aes(x = month, y = mean_PM2.5))+
  geom_bar(stat = "identity", fill = "#63afd0")+
  geom_text(aes(label = round(mean_PM2.5, digits = 2)), vjust = -0.25)
monthpm25
library(ggpubr)
monthsummary <- ggarrange(monthco,monthco2,monthno,monthno2,montho3,monthpm25,
                       labels = c("CO","CO2","NO","NO2","O3","PM2.5"),
                       ncol = 2, nrow = 3)
monthsummary

write.csv(plotdataHour,file = "Hourly average.csv")


#colorplot
mydata2=mydata1[,-c(1,2,7,17,18,19,20,21,23,24)]
ggplot(mydata2,
       aes(x = small,
           y = no,
           color = Day))+
  geom_point()+
  facet_wrap(~timeinterval,ncol = 2) +
  labs(title = "NO vs. Flow")


ggplot(mydata2,
       aes(x = ws,
           y = no2,
           color = Cardinal.WD))+
  geom_point()+
  labs(title = "NO vs. Flow")

ggplot(mydata2,
       aes(x = ws,
           y = no))+
  geom_point()+
  facet_wrap(~Cardinal.WD,ncol = 4) +
  labs(title = "NO vs. Flow")

ggplot(mydata2,
       aes(x = Small,
           y = CO,
           color = time_local))+
  geom_point()+
  facet_wrap(~timeinterval,ncol = 2) +
  labs(title = "CO vs. Flow")

ggplot(mydata2,
       aes(x = small,
           y = o3,
           color = Day))+
  geom_point()+
  facet_wrap(~timeinterval,ncol = 2) +
  labs(title = "O3 vs. Flow")

ggplot(mydata2,
       aes(x = small,
           y = pm25,
           color = Day))+
  geom_point()+
  facet_wrap(~timeinterval,ncol = 2) +
  labs(title = "PM2.5 vs. Flow")

ggplot(mydata2,
       aes(x = heavy,
           y = co2,
           color = time_local))+
  geom_point()+
  facet_wrap(~timeinterval,ncol = 2) +
  labs(title = "CO2 vs. Heavy")

#
ggplot(mydata2,
       aes(x = co,
           y = co2))+
  geom_point()+
  geom_smooth(se = FALSE,
              method = "lm")+
  labs(title = "CO2 vs. CO")


#
#


ggplot(mydata2,aes(x = co2)) +
  geom_histogram(color = "white",
                 fill = "cornflowerblue",
                 ) +
  facet_wrap(~time_local, ncol = 3) +
  labs(title = "CO2 concentrations by time")

ggplot(mydata2,aes(x = no))+
  geom_boxplot()+
  facet_grid(timeinterval)

ggplot(mydata2,aes(x = co)) +
  geom_boxplot(orientation = y) +
  facet_grid(~Month)

aggregate(mydata2$co,list(mydata2$Month),mean)

install.packages("lattice")
library(lattice)
bwplot(time_local~co, mydata2)
