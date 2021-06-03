mydata1=read.csv(file.choose(),header=T)
mydata2=na.omit(mydata1)
str(mydata2)

p1<-plot.windrose(spd=mydata2$WS_R,dir=mydata2$WD_R)
install.packages("openair")
install.packages("tidyverse")
library(openair)
library(tidyverse)
str(mydata2)

windRose(mydata2,ws = "ws", wd = "wd", 
         breaks=c(0,1,2,3,3.5,4),
         auto.text = TRUE,
         paddle = FALSE, 
         angle = 45,type = "Month",
         statistic = "abs.count",
         key=list(labels=c("0-1","1-2","2-3","3-3.5","3.5-4",">4")),
         key.footer="WSP(Knots)",key.position="bottom",
         col=c("#e0e4cc","#ffbf00","#2a9d8f","#106588","#db504a","#8f2d56"))

#by type
co <- windRose(mydata2,ws = "ws", wd = "wd", 
         breaks=c(0,1,2,3,3.5,4),
         auto.text = TRUE,
         paddle = FALSE, 
         angle = 45,type = "CO", layout=c(4,1),
         statistic = "abs.count",
         key=list(labels=c("0-1","1-2","2-3","3-3.5","3.5-4",">4")),
         key.footer="WSP(Knots)",key.position="bottom",
         col=c("#e0e4cc","#ffaa00","#009999","#0d58a6","#db504a","#8f2d56"))

co2 <- windRose(mydata2,ws = "ws", wd = "wd", 
               breaks=c(0,1,2,3,3.5,4),
               auto.text = TRUE,
               paddle = FALSE, 
               angle = 45,type = "CO2", layout=c(4,1),
               statistic = "abs.count",
               key=list(labels=c("0-1","1-2","2-3","3-3.5","3.5-4",">4")),
               key.footer="WSP(Knots)",key.position="bottom",
               col=c("#e0e4cc","#ffaa00","#009999","#0d58a6","#db504a","#8f2d56"))

no <- windRose(mydata2,ws = "ws", wd = "wd", 
               breaks=c(0,1,2,3,3.5,4),
               auto.text = TRUE,
               paddle = FALSE, 
               angle = 45,type = "NO", layout=c(4,1),
               statistic = "abs.count",
               key=list(labels=c("0-1","1-2","2-3","3-3.5","3.5-4",">4")),
               key.footer="WSP(Knots)",key.position="bottom",
               col=c("#e0e4cc","#ffaa00","#009999","#0d58a6","#db504a","#8f2d56"))

no2 <- windRose(mydata2,ws = "ws", wd = "wd", 
               breaks=c(0,1,2,3,3.5,4),
               auto.text = TRUE,
               paddle = FALSE, 
               angle = 45,type = "NO2", layout=c(4,1),
               statistic = "abs.count",
               key=list(labels=c("0-1","1-2","2-3","3-3.5","3.5-4",">4")),
               key.footer="WSP(Knots)",key.position="bottom",
               col=c("#e0e4cc","#ffaa00","#009999","#0d58a6","#db504a","#8f2d56"))

o3 <- windRose(mydata2,ws = "ws", wd = "wd", 
               breaks=c(0,1,2,3,3.5,4),
               auto.text = TRUE,
               paddle = FALSE, 
               angle = 45,type = "O3", layout=c(4,1),
               statistic = "abs.count",
               key=list(labels=c("0-1","1-2","2-3","3-3.5","3.5-4",">4")),
               key.footer="WSP(Knots)",key.position="bottom",
               col=c("#e0e4cc","#ffaa00","#009999","#0d58a6","#db504a","#8f2d56"))

pm25 <- windRose(mydata2,ws = "ws", wd = "wd", 
               breaks=c(0,1,2,3,3.5,4),
               auto.text = TRUE,
               paddle = FALSE, 
               angle = 45,type = "PM2.5", layout=c(4,1),
               statistic = "abs.count",
               key=list(labels=c("0-1","1-2","2-3","3-3.5","3.5-4",">4")),
               key.footer="WSP(Knots)",key.position="bottom",
               col=c("#e0e4cc","#ffaa00","#009999","#0d58a6","#db504a","#8f2d56"))

windsummary <- ggarrange(co,co2,no,no2,o3,pm25,
                          labels = c("CO","CO2","NO","NO2","O3","PM2.5"),
                          ncol = 6, nrow = 1)
windsummary



pollutionRose(mydata2,angle = 45,wd = "wd",
              pollutant = "NO2",
              auto.text = TRUE,
              paddle = FALSE,
              annotate = TRUE,
              key.footer = expression(paste(NO2~"(",??g/m^3,")")),
              key.position = "bottom",
              col=c("#e0e4cc","#ffbf00","#2a9d8f","#106588","#db504a","#8f2d56"))

#pollutant vs.RH and Temp
pollutionRose(mydata2,angle = 45,
              pollutant = "NO",
              auto.text = TRUE,
              paddle = FALSE,
              annotate = TRUE,
              type = c("humidity","temperature"),
              statistic = "abs.count",
              key.footer = expression(paste(NO~"(",??g/m^3,")")),
              key.position = "bottom",
              col=c("#e0e4cc","#ffbf00","#2a9d8f","#106588","#db504a","#8f2d56"))

pollutionRose(mydata2,angle = 45,
              pollutant = "NO",
              auto.text = TRUE,
              paddle = FALSE,
              annotate = TRUE,
              type = c("ws","flow"),
              key.footer = expression(paste(NO~"(",??g/m^3,")")),
              key.position = "bottom",
              col=c("#e0e4cc","#ffbf00","#2a9d8f","#106588","#db504a","#8f2d56"))

#pollutant vs.small and heavy
pollutionRose(mydata2,angle = 45,
              pollutant = "no",
              auto.text = TRUE,
              paddle = FALSE,
              annotate = TRUE,
              type = "ws",
              statistic = "abs.count",
              layout = c(4,1),
              key.footer = expression(paste(NO~"(",??g/m^3,")")),
              key.position = "bottom",
              col=c("#e0e4cc","#ffbf00","#2a9d8f","#106588","#db504a","#8f2d56"))

pollutionRose(mydata2,angle = 45,
              pollutant = "no",
              auto.text = TRUE,
              paddle = FALSE,
              annotate = TRUE,
              type = "heavy",
              layout = c(4,1),
              key.footer = expression(paste(NO~"(",??g/m^3,")")),
              key.position = "bottom",
              col=c("#e0e4cc","#ffbf00","#2a9d8f","#106588","#db504a","#8f2d56"))


windRose(mydata2,type = "CO", 
         breaks = c(0,1,2,3,4),
         angle = 45,
         auto.text = FALSE,
         paddle = FALSE, 
         key.footer = "WSP(Knots)")

pollutionRose(mydata2, pollutant = "flow",
              auto.text = FALSE,
              angle = 45,
              layout = c(4,1),
              type = "no2",
              paddle = FALSE)

pollutionRose(mydata2, pollutant = "co2",
              auto.text = FALSE,
              angle = 45,
              layout = c(4,1),
              type = "flow",
              paddle = FALSE)
