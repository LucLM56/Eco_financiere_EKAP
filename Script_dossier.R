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

base$Date <- as.Date(base$Date)
## Graphique de la série avant la corréction des points atypiques
#CairoPNG(filename = "Serieprix.png", width = 800, height = 500)
ggplot(base, aes(x = Date, y = Adj.Close))+
  geom_line()+
  scale_x_date(date_breaks = "6 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))
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
  scale_x_date(date_breaks = "6 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))
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
  scale_x_date(date_breaks = "6 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))
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

###rentabilité au carré
base3$Rentabilite2 <- base3$Rentabilite^2
base3$CleanReturn2 <- base3$CleanReturn^2

##Rentabilité brute au carré
#CairoPNG(filename = "SerieRentabiliteCarre.png", width = 800, height = 500)
ggplot(base3, aes(x = Date, y = Rentabilite2))+
  geom_line()+
  scale_x_date(date_breaks = "6 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))
#dev.off()

##Rentabilité corrigé au carré
#CairoPNG(filename = "SerieRentabilite2Carre.png", width = 800, height = 500)
ggplot(base3,aes(x = Date)) +
  geom_line(aes(y = Rentabilite2),color = "black") +
  geom_line(aes(y = CleanReturn2),color = "red") +
  scale_x_date(date_breaks = "6 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))
#dev.off()

## Différence entre les deux rentabilités au carré
base3$diff2 <- base3$diff^2

#CairoPNG(filename = "SerieDiffRentaCarre.png", width = 800, height = 500)
ggplot(base3, aes(x = Date, y = diff2))+
  geom_line()+
  scale_x_date(date_breaks = "6 month",date_labels = "%Y-%m")+
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))
#dev.off()

### Graphiques des corrélations des rentabilités
### et rentabilités au carré (séries corrigées)

#CairoPNG(filename = "CorrelogrammeRentabilites.png", width = 800, height = 500)
par(mfrow=c(2,1))
acf(base3$CleanReturn, main = "", xlab = "Retards")
pacf(base3$CleanReturn, main = "", xlab = "Retards")
#dev.off()

#CairoPNG(filename = "CorrelogrammeRentabilitesCarre.png", width = 800, height = 500)
par(mfrow=c(2,1))
acf(base3$CleanReturn2, main = "", xlab = "Retards")
pacf(base3$CleanReturn2, main = "", xlab = "Retards")
#dev.off()


### Statistiques descriptives sur la série corrigées ###
##Base sur la série corrigée
basec <- base3[,c(1,4,7)]

## Statistiques basic
stat1 = basicStats(basec$CleanReturn)
show(stat1)


stat2 = FinTS.stats(zoo(basec$CleanReturn))
show(stat2)

sdev = SemiDeviation(basec$CleanReturn)
show(sdev)

sd(basec$CleanReturn)

#Calcul de v1 et v2 pour skewness et kurtosis
v1 = -0.1313/sqrt(6/1278)
v1

kurtosis = 1.6272 + 3
kurtosis
v2 = 1.6272/sqrt(24/1278)
v2

## Caractéristique sur la distribution
Return <- ts(basec$CleanReturn)

#Test de normalité
JBtest = JarqueBeraTest(Return, robust = FALSE, method = "chisq")
show(JBtest)

#Test Q(10)
autocorrTest = Box.test(basec$CleanReturn, lag = 10, type = "Ljung-Box", fitdf = 0)
show(autocorrTest)

#Test Q²(10)
condheteroTest = Box.test(basec$CleanReturn2, lag = 10, type = "Ljung-Box", fitdf = 0)
show(condheteroTest)

#Test LM-ARCH(10)
testLM = ArchTest(zoo(basec$CleanReturn),lag=10)
show(testLM)

par(mfrow=c(1,1))

#Histogramme de la série corrigée
h<-hist(Return, main = "Distribution",  col="light blue", border="blue", xlab="Returns of Brent")
xfit <- seq(min(Return), max(Return), length = 40) 
yfit <- dnorm(xfit, mean = mean(Return), sd = sd(Return)) 
yfit <- yfit * diff(h$mids[1:2]) * length(Return) 
lines(xfit, yfit, col = "red", lwd = 2)


### Estimation des modèles de volatilité ###
## GARCH ##

spec = ugarchspec(variance.model=list(model = "sGARCH"),mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
fit = ugarchfit(data = Return, spec = spec)
fit

#Persistance
pers = persistence(fit)
show(pers)

#Half-life
hl = halflife(fit)
show(hl)
