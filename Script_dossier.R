library(readxl)
library(fBasics)	# descriptive stat
library(stats)		# descriptive stat
library(FinTS)
library(e1071)
library(PerformanceAnalytics)
library(DescTools)
library(aTSA)
library(quantmod)
library(tidyverse)
library(rugarch)
library(GAS)
library(Cairo)
library(RJDemetra)

base <- read.csv2("EDF_PA.csv", sep=",")
base <- base[,c(1,6)]
str(base)
base$Adj.Close <- as.numeric(base$Adj.Close)
str(base)
Date <- base$Date
## Statistiques basic
stat1 = basicStats(base)
show(stat1)
base$Date <- as.Date(base$Date)
## Graphique de la série avant la corréction des points atypiques
#CairoPNG(filename = "Serieprix.png", width = 800, height = 500)
ggplot(base, aes(x = Date, y = Adj.Close))+
  geom_line()+
  scale_x_date(date_labels = "%Y-%m")
#dev.off()

## Transformation 
#log(prix t) - log(prix t-1)
Cours <- ts(base$Adj.Close)
Rentabilite <- dailyReturn(Cours)
base$Rentabilite <- Rentabilite

base2 <- base  
str(base2)

#CairoPNG(filename = "SerieRentabilite.png", width = 800, height = 500)
ggplot(base2, aes(x = Date, y = Rentabilite))+
  geom_line()+
  scale_x_date(date_labels = "%Y-%m")
#dev.off()
cly <- Return.clean(Rentabilite, method = "boudt")
base2$CleanReturn <- cly 

#Superposition de la série corrigée et non corrigée 
#des points atypiques
base3 <- base2
str(base3)
base3$Rentabilite <- as.data.frame(base3$Rentabilite)
base3$Rentabilite <- as.numeric(base3$Rentabilite$V1)
base3$CleanReturn <- as.data.frame(base3$CleanReturn)
base3$CleanReturn <- as.numeric(base3$CleanReturn$V1)

#CairoPNG(filename = "SerieRentabilite2.png", width = 800, height = 500)
ggplot(base3,aes(x = Date)) +
  geom_line(aes(y = Rentabilite),color = "black") +
  geom_line(aes(y = CleanReturn),color = "red") +
  scale_x_date(date_labels = "%Y-%m")
#dev.off()

base3$diff <- base3$Rentabilite - base3$CleanReturn 

#CairoPNG(filename = "SerieDiffRenta.png", width = 800, height = 500)
ggplot(base3, aes(x = Date, y = diff))+
  geom_line()+
  scale_x_date(date_breaks = "6 month",date_labels = "%Y-%m")+
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))
#dev.off()

help(scale_x_date)
