setwd("/Users/Kullu/Desktop")
dat<-read.csv("Annex 4.csv", header=T)
library(tidyverse)
#library(dplyr)
dat
prey.data<-gather(subset(dat,select= c(Country, Landscape, 
                                       Control.Conservation, 
                                       Study.area, Prey.species, X2016,X2017)), 
                         Year, Prey.density, X2016:X2017, factor_key = T)
prey.data$Prey.density<-prey.data$Prey.density/prey.data$Study.area                  

par(mfcol = c(2,1))
par(mar = c(1,6,2,1))
## Plotting prey data
prey.data<-prey.data[order(prey.data$Country),]
barplot(prey.data$Prey.density, col = c("Grey", "black"), 
        space = c(rep(c(0.5,0.1),5)), 
#        names = (prey.data$Country), 
        las = 2,
        ylab = "Prey density (per sq. km.)")
text(12,3, labels = "Prey density")

par(mar = c(6,6,0,1))
## ploting predator data
pred.data<-gather(subset(dat,select= c(Country, Landscape, 
                                       Control.Conservation, 
                                       X2016.1,X2017.1)), 
                  Year, Pred.density, X2016.1:X2017.1, factor_key = T)
pred.data<-pred.data[order(pred.data$Country),]

barplot(pred.data$Pred.density, col = c("grey", "black"), 
        space = c(rep(c(0.5,0.1),5)),
        names = (pred.data$Country), las = 2,
        ylab = "Snow leopard density \n(per 100 sq. km.)")
text(13,1.4, labels = "Snow leopard density")
