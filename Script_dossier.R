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
library(forecast)
library(multDM)
library(MCS)

base <- read.csv2("EDF_PA.csv", sep=",")
base <- base[,c(1,6)]
str(base)
base$Adj.Close <- as.numeric(base$Adj.Close)
str(base)
Date <- base$Date

base$Date <- as.Date(base$Date)
basetest <- base[1023:1278,]
base <- base[1:1022,]
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
#basetest
Courstest <- ts(basetest$Adj.Close)
Rentabilitetest <- dailyReturn(Courstest)
basetest$Rentabilite <- Rentabilitetest

basetest2 <- basetest  
str(basetest2)

#CairoPNG(filename = "SerieRentabilite.png", width = 800, height = 500)
ggplot(base2, aes(x = Date, y = Rentabilite))+
  geom_line()+
  scale_x_date(date_breaks = "6 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))
#dev.off()

cly <- Return.clean(Rentabilite, method = "boudt")
base2$CleanReturn <- cly 
clytest <- Return.clean(Rentabilitetest, method = "boudt")
basetest2$CleanReturn <- clytest

#Superposition de la série corrigée et non corrigée 
#des points atypiques
base3 <- base2
str(base3)
base3$Rentabilite <- as.data.frame(base3$Rentabilite)
base3$Rentabilite <- as.numeric(base3$Rentabilite$V1)
base3$CleanReturn <- as.data.frame(base3$CleanReturn)
base3$CleanReturn <- as.numeric(base3$CleanReturn$V1)

basetest3 <- basetest2
str(basetest3)
basetest3$Rentabilite <- as.data.frame(basetest3$Rentabilite)
basetest3$Rentabilite <- as.numeric(basetest3$Rentabilite$V1)
basetest3$CleanReturn <- as.data.frame(basetest3$CleanReturn)
basetest3$CleanReturn <- as.numeric(basetest3$CleanReturn$V1)

#CairoPNG(filename = "SerieRentabilite2.png", width = 800, height = 500)
ggplot(base3,aes(x = Date)) +
  geom_line(aes(y = Rentabilite),color = "black") +
  geom_line(aes(y = CleanReturn),color = "red") +
  scale_x_date(date_breaks = "6 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))
#dev.off()

base3$diff <- base3$Rentabilite - base3$CleanReturn 
basetest3$diff <- basetest3$Rentabilite - basetest3$CleanReturn 

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
basetest3$Rentabilite2 <- basetest3$Rentabilite^2
basetest3$CleanReturn2 <- basetest3$CleanReturn^2

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
basetest3$diff2 <- basetest3$diff^2

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
v1 = -0.119001/sqrt(6/1022)
v1

kurtosis = 1.507325 + 3
kurtosis
v2 = 1.507325/sqrt(24/1022)
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
#CairoPNG(filename = "HistogrammeDistribution.png", width = 800, height = 500)
h<-hist(Return, main = "Distribution",  col="light blue", border="blue", xlab="Returns")
xfit <- seq(min(Return), max(Return), length = 40) 
yfit <- dnorm(xfit, mean = mean(Return), sd = sd(Return)) 
yfit <- yfit * diff(h$mids[1:2]) * length(Return) 
lines(xfit, yfit, col = "red", lwd = 2)
#dev.off()


### Estimation des modèles de volatilité ###
### Distribution normale
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

#Tests sur les r?sidus
#Q(5) : 7.816 p-value : 0.03274
#Q^2(5) : 5.256 p-value : 0.13387
#LM-ARCH(5) : 0.44122 p-value : 0.9008
#Engle-Ng Sign Test : 2.1703 p-value : 0.5378


## GJR-GARCH ##
spec2 = ugarchspec(variance.model=list(model = "gjrGARCH"),mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
fit2 = ugarchfit(data = Return, spec = spec2)
fit2

#Persistance
pers2 = persistence(fit2)
show(pers2)

#Half-life
hl2 = halflife(fit2)
show(hl2)

#Tests sur les r?sidus
#Q(5) : 7.751 p-value : 0.03396
#Q^2(5) : 5.487 p-value : 0.1185
#LM-ARCH(5) : 0.40395 p-value : 0.9115
#Engle-Ng Sign Test : 2.0674 p-value : 0.5585


## IGARCH ##
spec3 = ugarchspec(variance.model=list(model = "iGARCH"),mean.model=list(armaOrder=c(0,0), include.mean=TRUE))
fit3 = ugarchfit(data = Return, spec = spec3)
fit3

#Tests sur les r?sidus
#Q(5) : 8.838 p-value : 0.018182
#Q^2(5) : 5.465 p-value : 0.11989
#LM-ARCH(5) : 0.39232 p-value : 0.9148
#Engle-Ng Sign Test : 1.3662 p-value : 0.7135


## Riskmetrics ##
spec4 = ugarchspec(variance.model=list(model = "iGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0), include.mean=TRUE),distribution.model="norm",fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))
fit4 = ugarchfit(data = Return, spec = spec4)
fit4

#Tests sur les r?sidus
#Q(5) : 9.795 p-value : 0.010389
#Q^2(5) : 5.108 p-value : 0.1447
#LM-ARCH(5) : 0.4444 p-value : 0.8999
#Engle-Ng Sign Test : 1.1638 p-value : 0.7617


### Distribution Student
## GARCH ##
specst = ugarchspec(variance.model=list(model = "sGARCH"),mean.model=list(armaOrder=c(0,0), include.mean=TRUE),distribution.model = "std")
fitst = ugarchfit(data = Return, spec = specst)
fitst

#Persistance
persst = persistence(fitst)
show(persst)

#Half-life
hlst = halflife(fitst)
show(hlst)

#Tests sur les r?sidus
#Q(5) : 8.126 p-value : 0.02742
#Q^2(5) : 5.386 p-value : 0.12499
#LM-ARCH(5) : 0.4709 p-value : 0.8922
#Engle-Ng Sign Test : 1.7122 p-value : 0.6342


## GJR-GARCH ##
spec2st = ugarchspec(variance.model=list(model = "gjrGARCH"),mean.model=list(armaOrder=c(0,0), include.mean=TRUE),distribution.model = "std")
fit2st = ugarchfit(data = Return, spec = spec2st)
fit2st

#Persistance
pers2st = persistence(fit2st)
show(pers2st)

#Half-life
hl2st = halflife(fit2st)
show(hl2st)

#Tests sur les r?sidus
#Q(5) : 7.918 p-value : 0.03088
#Q^2(5) : 6.083 p-value : 0.086
#LM-ARCH(5) : 0.35516 p-value : 0.9253
#Engle-Ng Sign Test : 1.5542 p-value : 0.6698


## IGARCH ##
spec3st = ugarchspec(variance.model=list(model = "iGARCH"),mean.model=list(armaOrder=c(0,0), include.mean=TRUE),distribution.model = "std")
fit3st = ugarchfit(data = Return, spec = spec3st)
fit3st

#Tests sur les r?sidus
#Q(5) : 8.677 p-value : 0.019969
#Q^2(5) : 5.585 p-value : 0.11246
#LM-ARCH(5) : 0.42936 p-value : 0.9043
#Engle-Ng Sign Test : 1.8563 p-value : 0.6028


## Riskmetrics ##
spec4st = ugarchspec(variance.model=list(model = "iGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0), include.mean=TRUE),distribution.model="std",fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))
fit4st = ugarchfit(data = Return, spec = spec4st)
fit4st

#Tests sur les r?sidus
#Q(5) : 9.803 p-value : 0.010344
#Q^2(5) : 5.185 p-value : 0.1389
#LM-ARCH(5) : 0.4465 p-value : 0.8993
#Engle-Ng Sign Test : 1.4614 p-value : 0.6912


### Pr?visions de la volatilit? ### 
basetestc <- basetest3[,c(1,4,7)]
basec2 <- rbind(basec,basetestc)
Returnprev <- basec2$CleanReturn


estim <- 1022
h <- 256
statmat <- matrix(nrow=h, ncol=4)
statmat <- as.data.frame(statmat)
varmat <- matrix(nrow=h, ncol=4)
varmat <- as.data.frame(varmat)
esmat <- matrix(nrow=h, ncol=4)
esmat <- as.data.frame(esmat)

## GARCH - Normal ##
spec = ugarchspec(variance.model=list(model = "sGARCH"),mean.model=list(armaOrder=c(0,0), include.mean=TRUE))

for (i in 1:h){
  yy <- Returnprev[1:(estim-1+i)]
  fit = ugarchfit(data = yy, spec = spec)
  forc =  ugarchforecast(fit, n.ahead=1)
  statmat[i,1] <- round(sigma(forc)^2,8)
  varmat[i,1] <- qnorm(0.05)*sigma(forc)
  esmat[i,1] <- -dnorm(qnorm(0.05))/0.05*sigma(forc)
}

# A comparer aux rentabilites au carre
# MSE
l1 <- LossVol(basetestc$CleanReturn2,statmat[,1],which = "SE1")
mean(l1)


## Riskmetrics - Normal ##
spec4 = ugarchspec(variance.model=list(model = "iGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0), include.mean=TRUE),distribution.model="norm",fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))

for (i in 1:h){
  yy <- Returnprev[1:(estim-1+i)]
  fit = ugarchfit(data = yy, spec = spec4)
  forc =  ugarchforecast(fit, n.ahead=1)
  statmat[i,2] <- round(sigma(forc)^2,8)
  varmat[i,2] <- qnorm(0.05)*sigma(forc)
  esmat[i,2] <- -dnorm(qnorm(0.05))/0.05*sigma(forc)
}

# MSE
l2 <- LossVol(basetestc$CleanReturn2,statmat[,2],which = "SE1")
mean(l2)

## GARCH - Student ##
specst = ugarchspec(variance.model=list(model = "sGARCH"),mean.model=list(armaOrder=c(0,0), include.mean=TRUE),distribution.model = "std")

for (i in 1:h){
  yy <- Returnprev[1:(estim-1+i)]
  fit = ugarchfit(data = yy, spec = specst)
  forc =  ugarchforecast(fit, n.ahead=1)
  statmat[i,3] <- round(sigma(forc)^2,8)
  varmat[i,3] <- qnorm(0.05)*sigma(forc)
  esmat[i,3] <- -dnorm(qnorm(0.05))/0.05*sigma(forc)
}

# A comparer aux rentabilites au carre
# MSE
l3 <- LossVol(basetestc$CleanReturn2,statmat[,3],which = "SE1")
mean(l3)

## Riskmetrics - Student ##
spec4st = ugarchspec(variance.model=list(model = "iGARCH",garchOrder=c(1,1)),mean.model=list(armaOrder=c(0,0), include.mean=TRUE),distribution.model="std",fixed.pars=list(omega=0,alpha1=0.06,beta1=0.94))

for (i in 1:h){
  yy <- Returnprev[1:(estim-1+i)]
  fit = ugarchfit(data = yy, spec = spec4st)
  forc =  ugarchforecast(fit, n.ahead=1)
  statmat[i,4] <- round(sigma(forc)^2,8)
  varmat[i,4] <- qnorm(0.05)*sigma(forc)
  esmat[i,4] <- -dnorm(qnorm(0.05))/0.05*sigma(forc)
}

# MSE
l4 <- LossVol(basetestc$CleanReturn2,statmat[,4],which = "SE1")
mean(l4)

##Tests DM et MCS
DM.test(statmat[,1],statmat[,2],basetestc$CleanReturn2,loss.type = "SE",1,c=FALSE,H1="same")
DM.test(statmat[,1],statmat[,3],basetestc$CleanReturn2,loss.type = "SE",1,c=FALSE,H1="same")
DM.test(statmat[,1],statmat[,4],basetestc$CleanReturn2,loss.type = "SE",1,c=FALSE,H1="same")
DM.test(statmat[,2],statmat[,3],basetestc$CleanReturn2,loss.type = "SE",1,c=FALSE,H1="same")
DM.test(statmat[,2],statmat[,4],basetestc$CleanReturn2,loss.type = "SE",1,c=FALSE,H1="same")
DM.test(statmat[,3],statmat[,4],basetestc$CleanReturn2,loss.type = "SE",1,c=FALSE,H1="less")


loss=cbind(l1,l2,l3,l4)
summary(loss)
MCS <- MCSprocedure(Loss=loss[,1:4],alpha=0.1,B=5000,statistic="Tmax",cl=NULL)


### Pr?vision de la VAR ###
varmat
Datetest <- Date[1023:1278]
Datetest <- as.Date(Datetest)
VAR <- cbind(Datetest,varmat)
# Graphiques
VAR$returns <- basetestc$CleanReturn
str(VAR)


## GARCH Normal ##
#CairoPNG(filename = "VAR_GARCH_Norm.png", width = 800, height = 500)
ggplot(VAR, aes(x = Datetest, y = V1))+
  geom_line()+
  scale_x_date(date_breaks = "2 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))+
  xlab("Date")+
  ylab("VAR")
#dev.off()

#Comparaison avec les expected returns
#CairoPNG(filename = "Comp_VAR_GARCH_Norm.png", width = 800, height = 500)
ggplot(VAR,aes(x = Datetest))+
  geom_line(aes(y = V1),color = "blue",show.legend = T)+
  geom_line(aes(y = returns), color = "red",show.legend = T)+
  scale_x_date(date_breaks = "2 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.position = "left")+
  xlab("Date")+
  ylab("VAR")+
  scale_fill_discrete(labels=c("Returns","VAR"))
#dev.off()


## Riskmetrics Normal ##
#CairoPNG(filename = "VAR_Riskmetrics_Norm.png", width = 800, height = 500)
ggplot(VAR, aes(x = Datetest, y = V2))+
  geom_line()+
  scale_x_date(date_breaks = "2 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))+
  xlab("Date")+
  ylab("VAR")
#dev.off()

#Comparaison avec les expected returns
#CairoPNG(filename = "Comp_VAR_Riskmetrics_Norm.png", width = 800, height = 500)
ggplot(VAR,aes(x = Datetest))+
  geom_line(aes(y = V2),color = "blue",show.legend = T)+
  geom_line(aes(y = returns), color = "red",show.legend = T)+
  scale_x_date(date_breaks = "2 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.position = "left")+
  xlab("Date")+
  ylab("VAR")+
  scale_fill_discrete(labels=c("Returns","VAR"))
#dev.off()


## GARCH Student ##
#CairoPNG(filename = "VAR_GARCH_Std.png", width = 800, height = 500)
ggplot(VAR, aes(x = Datetest, y = V3))+
  geom_line()+
  scale_x_date(date_breaks = "2 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))+
  xlab("Date")+
  ylab("VAR")
#dev.off()

#Comparaison avec les expected returns
#CairoPNG(filename = "Comp_VAR_GARCH_Std.png", width = 800, height = 500)
ggplot(VAR,aes(x = Datetest))+
  geom_line(aes(y = V3),color = "blue",show.legend = T)+
  geom_line(aes(y = returns), color = "red",show.legend = T)+
  scale_x_date(date_breaks = "2 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.position = "left")+
  xlab("Date")+
  ylab("VAR")+
  scale_fill_discrete(labels=c("Returns","VAR"))
#dev.off()


## Riskmetrics Student ##
#CairoPNG(filename = "VAR_Riskmetrics_Std.png", width = 800, height = 500)
ggplot(VAR, aes(x = Datetest, y = V4))+
  geom_line()+
  scale_x_date(date_breaks = "2 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10))+
  xlab("Date")+
  ylab("VAR")
#dev.off()

#Comparaison avec les expected returns
#CairoPNG(filename = "Comp_VAR_Riskmetrics_Std.png", width = 800, height = 500)
ggplot(VAR,aes(x = Datetest))+
  geom_line(aes(y = V4),color = "blue",show.legend = T)+
  geom_line(aes(y = returns), color = "red",show.legend = T)+
  scale_x_date(date_breaks = "2 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.position = "left")+
  xlab("Date")+
  ylab("VAR")+
  scale_fill_discrete(labels=c("Returns","VAR"))
#dev.off()

#CairoPNG(filename = "Comp_VAR_Riskmetrics_Std.png", width = 800, height = 500)
ggplot(VAR,aes(x = Datetest))+
  geom_line(aes(y = V1),color = "blue",show.legend = T)+
  geom_line(aes(y = V2), color = "red",show.legend = T)+
  geom_line(aes(y = V3), color = "green",show.legend = T)+
  geom_line(aes(y = V4), color = "yellow",show.legend = T)+
  scale_x_date(date_breaks = "2 month",date_labels = "%Y-%m") +
  theme(axis.text.x = element_text(face="bold", size=10),
        axis.text.y = element_text(face = "bold", size = 10),
        legend.position = "left")+
  xlab("Date")+
  ylab("VAR")+
  scale_fill_discrete(labels=c("Returns","VAR"))
#dev.off()

### Calculs de diff?rentes mesures ###

## Le nombre d'exceptions ##
Excep = basetestc$CleanReturn - varmat$V1
sort(Excep) # 7 exceptions

Excep2 = basetestc$CleanReturn - varmat$V2
sort(Excep2) # 8 exceptions

Excep3 = basetestc$CleanReturn - varmat$V3
sort(Excep3) # 7 exceptions

Excep4 = basetestc$CleanReturn - varmat$V4
sort(Excep4) # 8 exceptions

alpha = 0.05
test = ts(basetestc$CleanReturn)


## Backtesting ##

## GARCH - Normal ##
var = ts(varmat$V1)
backtest <- BacktestVaR(data = test, VaR = var, alpha = alpha)

N1 = 7
T1 = 256

#Z_uc
Z_uc1 <- (N1-alpha*T1)/(sqrt(alpha*T1*(1-alpha)))
Z_uc1
2*pnorm(-abs(Z_uc1))#0.09625959
#Kupiec
backtest$LRuc
#EM
backtest$DQ

## Riskmetrics - Normal ##
var2 = ts(varmat$V2)
backtest2 <- BacktestVaR(data = test, VaR = var2, alpha = alpha)
N2 = 8

#Z_uc
Z_uc2 <- (N2-alpha*T1)/(sqrt(alpha*T1*(1-alpha)))
Z_uc2
2*pnorm(-abs(Z_uc2))#0.1686686
#Kupiec
backtest2$LRuc
#EM
backtest2$DQ

## GARCH - Student ##
var3 = ts(varmat$V3)
backtest3 <- BacktestVaR(data = test, VaR = var3, alpha = alpha)
N3 = 7

#Z_uc
Z_uc3 <- (N3-alpha*T1)/(sqrt(alpha*T1*(1-alpha)))
Z_uc3
2*pnorm(-abs(Z_uc3))#0.09625959
#Kupiec
backtest3$LRuc
#EM
backtest3$DQ

## Riskmetrics - Student ##
var4 = ts(varmat$V4)
backtest4 <- BacktestVaR(data = test, VaR = var4, alpha = alpha)
N4 = 8

#Z_uc
Z_uc4 <- (N4-alpha*T1)/(sqrt(alpha*T1*(1-alpha)))
Z_uc4
2*pnorm(-abs(Z_uc4))#0.1686686
#Kupiec
backtest4$LRuc
#EM
backtest4$DQ


## VaR Moyennes ##
VaR1 <- mean(varmat$V1)
VaR2 <- mean(varmat$V2)
VaR3 <- mean(varmat$V3)
VaR4 <- mean(varmat$V4)
VaR1
VaR2
VaR3
VaR4


## Expected Shortfall ##
ES1 <- mean(esmat$V1)
ES2 <- mean(esmat$V2)
ES3 <- mean(esmat$V3)
ES4 <- mean(esmat$V4)
ES1
ES2
ES3
ES4





