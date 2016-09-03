library(gdata)
library(stringr)
library(plyr)
library(dplyr)
library(graphics)
library(forecast)
library(stats)
library(fGarch)
library(FitAR)
library(astsa)
library(tseries)


#**************************************************************
#********************                    **********************
#********************Last semester's code**********************
#********************                    **********************
#**************************************************************


data1<- read.csv("UVA_DSI_DailySummary.csv")
#***************************************************************
#Overview of each types of buildings
#***************************************************************

# To look into the Building 0201 #Ohill Dining Hall
dataone <- subset(data1, grepl("Buildings/0201", data1$Tagname))
summary(dataone$Tagname)
tail(dataone$Tagname)

par(mfrow=c(2,3))
# To look into the Energy
Energydata <- subset(dataone, grepl("Energy", dataone$Tagname))
plot(Energydata$Interpolative_Value ~ Energydata$Timestamp, col = 77,main='Energy')

# To look into the Heating:
Heatingdata <- subset(dataone, grepl("Heating", dataone$Tagname))
plot(Heatingdata$Interpolative_Value ~ Heatingdata$Timestamp, col = 77,main='Heating')

# To look into the Electric
Electricdata <- subset(dataone, grepl("Electric", dataone$Tagname))
plot(Electricdata$Interpolative_Value ~ Electricdata$Timestamp, col = 77,main='Electric')

# To look into the chilled water:
waterdata <- subset(dataone, grepl("Chilled Water", dataone$Tagname))
plot(waterdata$Interpolative_Value ~ waterdata$Timestamp, col = 77,main='Chilled Water')

# To look into the Steam
Steamdata <- subset(dataone, grepl("Steam", dataone$Tagname))
plot(Steamdata$Interpolative_Value ~ Steamdata$Timestamp, col = 77,main='Steam')

#Tok look into the HW
HWdata <- subset(dataone, grepl("HW", dataone$Tagname))
plot(HWdata$Interpolative_Value ~ HWdata$Timestamp, col = 77,main='Hot Water')



# To look into the Building 0202 #Olsson Hall
datatwo <- subset(data1, grepl("Buildings/0202", data1$Tagname))
datatwo$mydates <- as.Date(datatwo$Timestamp)
summary(datatwo$Tagname)
tail(datatwo$Tagname)

par(mfrow=c(2,3))
# To look into the Energy
Energydata <- subset(datatwo, grepl("Energy", datatwo$Tagname))
plot(Energydata$Interpolative_Value ~ Energydata$Timestamp, col = 77,main='Energy')

# To look into the Heating:
Heatingdata <- subset(datatwo, grepl("Heating", datatwo$Tagname))
plot(Heatingdata$Interpolative_Value ~ Heatingdata$Timestamp, col = 77,main='Heating')

# To look into the Electric
Electricdata <- subset(datatwo, grepl("Electric", datatwo$Tagname))
plot(Electricdata$Interpolative_Value ~ Electricdata$Timestamp, col = 77,main='Electric')

# To look into the chilled water:
waterdata <- subset(datatwo, grepl("Chilled Water", datatwo$Tagname))
plot(waterdata$Interpolative_Value ~ waterdata$Timestamp, col = 77,main='Chilled Water')

# To look into the Steam
Steamdata <- subset(datatwo, grepl("Steam", datatwo$Tagname))
plot(Steamdata$Interpolative_Value ~ Steamdata$Timestamp, col = 77,main='Steam')

#Tok look into the HW
HWdata <- subset(datatwo, grepl("HW", datatwo$Tagname))
plot(HWdata$Interpolative_Value ~ HWdata$Timestamp, col = 77,main='Hot Water')



# To look into the Building 0214 #Rice Hall
datathree <- subset(data1, grepl("Buildings/0214", data1$Tagname))
summary(datathree$Tagname)
tail(datathree$Tagname)

par(mfrow=c(2,3))
# To look into the Energy
Energydata <- subset(datathree, grepl("Energy", datathree$Tagname))
plot(Energydata$Interpolative_Value ~ Energydata$Timestamp, col = 77,main='Energy')

# To look into the Heating:
Heatingdata <- subset(datathree, grepl("Heating", datathree$Tagname))
plot(Heatingdata$Interpolative_Value ~ Heatingdata$Timestamp, col = 77,main='Heating')

# To look into the Electric
Electricdata <- subset(datathree, grepl("Electric", datathree$Tagname))
plot(Electricdata$Interpolative_Value ~ Electricdata$Timestamp, col = 77,main='Electric')

# To look into the chilled water:
waterdata <- subset(datathree, grepl("Chilled Water", datathree$Tagname))
plot(waterdata$Interpolative_Value ~ waterdata$Timestamp, col = 77,main='Chilled Water')

# To look into the Steam
Steamdata <- subset(datathree, grepl("Steam", datathree$Tagname))
plot(Steamdata$Interpolative_Value ~ Steamdata$Timestamp, col = 77,main='Steam')

#Tok look into the HW
HWdata <- subset(datathree, grepl("HW", datathree$Tagname))
plot(HWdata$Interpolative_Value ~ HWdata$Timestamp, col = 77,main='Hot Water')



# To look into the Building 0238 #FM LANDSCAPE SHOP
datafour<- subset(data1, grepl("Buildings/0238", data1$Tagname))
summary(datafour$Tagname)
tail(datafour$Tagname)

par(mfrow=c(2,3))
# To look into the Energy
Energydata <- subset(datafour, grepl("Energy", datafour$Tagname))
plot(Energydata$Interpolative_Value ~ Energydata$Timestamp, col = 77,main='Energy')

# To look into the Heating:
Heatingdata <- subset(datafour, grepl("Heating", datafour$Tagname))
plot(Heatingdata$Interpolative_Value ~ Heatingdata$Timestamp, col = 77,main='Heating')

# To look into the Electric
Electricdata <- subset(datafour, grepl("Electric", datafour$Tagname))
plot(Electricdata$Interpolative_Value ~ Electricdata$Timestamp, col = 77,main='Electric')

# To look into the chilled water:
waterdata <- subset(datafour, grepl("Chilled Water", datafour$Tagname))
plot(waterdata$Interpolative_Value ~ waterdata$Timestamp, col = 77,main='Chilled Water')

# To look into the Steam
Steamdata <- subset(datafour, grepl("Steam", datafour$Tagname))
plot(Steamdata$Interpolative_Value ~ Steamdata$Timestamp, col = 77,main='Steam')

#Tok look into the HW
HWdata <- subset(datafour, grepl("HW", datafour$Tagname))
plot(HWdata$Interpolative_Value ~ HWdata$Timestamp, col = 77,main='Hot Water')





#***************************************************************
#Pick Olsson Hall as an example, do the splines & ACF & PACF
#***************************************************************

olsson <- subset(data1, grepl("Buildings/0202", data1$Tagname))
olsson$mydates <- as.Date(olsson$Timestamp)
olssonchilledwater <- subset(olsson, grepl("Chilled Water", olsson$Tagname))
olssonSteam <- subset(olsson, grepl("Steam", olsson$Tagname))
olssonElectric <- subset(olsson, grepl("Electric", olsson$Tagname))
olssonHeating <- subset(olsson, grepl("Heating", olsson$Tagname))
olssonEnergy <- subset(olsson, grepl("Energy", olsson$Tagname))
olssonHW <- subset(olsson, grepl("HW", olsson$Tagname))

#Splines are an extension of polynomial regression whereby we divide all times t into k intervals called knots. 
#For each intervals, a regression is fit with typically three parameters (cubic spline). 

par(mfrow=c(2,3))
plot(olssonEnergy$Interpolative_Value, type='p', ylab='Energy',main='OlssonEnergy')
lines(smooth.spline(time(olssonEnergy$Interpolative_Value), olssonEnergy$Interpolative_Value))
lines(smooth.spline(time(olssonEnergy$Interpolative_Value), olssonEnergy$Interpolative_Value, spar=1),lwd=3)

plot(olssonHeating$Interpolative_Value, type='p', ylab='Heating',main='OlssonHeating')
lines(smooth.spline(time(olssonHeating$Interpolative_Value), olssonHeating$Interpolative_Value))
lines(smooth.spline(time(olssonHeating$Interpolative_Value), olssonHeating$Interpolative_Value, spar=1),lwd=3)

plot(olssonElectric$Interpolative_Value, type='p', ylab='Electric',main='OlssonElectric')
lines(smooth.spline(time(olssonElectric$Interpolative_Value), olssonElectric$Interpolative_Value))
lines(smooth.spline(time(olssonElectric$Interpolative_Value), olssonElectric$Interpolative_Value, spar=1),lwd=3)

plot(olssonchilledwater$Interpolative_Value, type='p', ylab='Energy',main='OlssonChilledwater')
lines(smooth.spline(time(olssonchilledwater$Interpolative_Value), olssonchilledwater$Interpolative_Value))
lines(smooth.spline(time(olssonchilledwater$Interpolative_Value), olssonchilledwater$Interpolative_Value, spar=1),lwd=3)

plot(olssonSteam$Interpolative_Value, type='p', ylab='Energy',main='OlssonSteam')
lines(smooth.spline(time(olssonSteam$Interpolative_Value), olssonSteam$Interpolative_Value))
lines(smooth.spline(time(olssonSteam$Interpolative_Value), olssonSteam$Interpolative_Value, spar=1),lwd=3)

plot(olssonHW$Interpolative_Value, type='p', ylab='Energy',main='OlssonHotWater')
lines(smooth.spline(time(olssonHW$Interpolative_Value), olssonHW$Interpolative_Value))
lines(smooth.spline(time(olssonHW$Interpolative_Value), olssonHW$Interpolative_Value, spar=1),lwd=3)

par(mfrow=c(1,3))


#***************************************************************
#Pick Olsson Hall as an example, decide the model for each types of energy
#***************************************************************


#ARMA for energy
summary(r.arma <- arma(olssonEnergy$Interpolative_Value, order = c(1, 0)))
summary(r.arma <- arma(olssonEnergy$Interpolative_Value, order = c(2, 0)))
summary(r.arma <- arma(olssonEnergy$Interpolative_Value, order = c(0, 1)))
summary(r.arma <- arma(olssonEnergy$Interpolative_Value, order = c(0, 2)))
summary(r.arma <- arma(olssonEnergy$Interpolative_Value, order = c(1, 1)))
#ARMA(1,1) has the smallest AIC

plot(r.arma$residuals,main="Energy")
abline(0,0)

#ARMA for electrictiy
summary(r1.arma <- arma(olssonElectric$Interpolative_Value, order = c(1, 0)))
summary(r1.arma <- arma(olssonElectric$Interpolative_Value, order = c(2, 0)))
summary(r1.arma <- arma(olssonElectric$Interpolative_Value, order = c(0, 1)))
summary(r1.arma <- arma(olssonElectric$Interpolative_Value, order = c(0, 2)))
summary(r1.arma <- arma(olssonElectric$Interpolative_Value, order = c(1, 1)))
summary(r1.arma <- arma(olssonElectric$Interpolative_Value, order = c(1, 2)))
summary(r1.arma <- arma(olssonElectric$Interpolative_Value, order = c(2, 1)))
summary(r1.arma <- arma(olssonElectric$Interpolative_Value, order = c(2, 2)))

#ARMA(1,1) has the smallest AIC
plot(r1.arma$residuals)

#ARMA for heating
summary(r2.arma <- arma(olssonHeating$Interpolative_Value, order = c(1, 0)))
summary(r2.arma <- arma(olssonHeating$Interpolative_Value, order = c(2, 0)))
summary(r2.arma <- arma(olssonHeating$Interpolative_Value, order = c(0, 1)))
summary(r2.arma <- arma(olssonHeating$Interpolative_Value, order = c(0, 2)))
summary(r2.arma <- arma(olssonHeating$Interpolative_Value, order = c(1, 1)))
summary(r2.arma <- arma(olssonHeating$Interpolative_Value, order = c(1, 2)))
summary(r2.arma <- arma(olssonHeating$Interpolative_Value, order = c(2, 1)))
summary(r2.arma <- arma(olssonHeating$Interpolative_Value, order = c(2, 2)))

#ARMA(2,2)
plot(r2.arma$residuals)

#ARMA for chilledwater
summary(r3.arma <- arma(olssonchilledwater$Interpolative_Value, order = c(1, 0)))
summary(r3.arma <- arma(olssonchilledwater$Interpolative_Value, order = c(2, 0)))
summary(r3.arma <- arma(olssonchilledwater$Interpolative_Value, order = c(0, 1)))
summary(r3.arma <- arma(olssonchilledwater$Interpolative_Value, order = c(0, 2)))
summary(r3.arma <- arma(olssonchilledwater$Interpolative_Value, order = c(1, 1)))
summary(r3.arma <- arma(olssonchilledwater$Interpolative_Value, order = c(1, 2)))
summary(r3.arma <- arma(olssonchilledwater$Interpolative_Value, order = c(2, 1)))
summary(r3.arma <- arma(olssonchilledwater$Interpolative_Value, order = c(2, 2)))
summary(r3.arma <- arma(olssonchilledwater$Interpolative_Value, order = c(3, 1)))
#ARMA(3,1)
plot(r3.arma$residuals)


#***************************************************************
#Check the estimated model
#***************************************************************
#ARMA(1,1) model for energy
sarima(olssonEnergy$Interpolative_Value, 1, 0, 1) 

#Reference book: tsa3,P145
#Reference web: http://rpubs.com/ryankelly/ts3

#The AR coefficient is statistically significant (z = 0.8622/0.0382 = 22.57). 
#So it's not necessary to test the mean coefficient. We know that it's not 0.
#The time series plot of the standardized residuals seems to indicate there is no trend in the residuals.
#Notice that there are outliers.
#The ACF of the residuals shows no significant autocorrelations.
#The normal Q-Q plot of the residuals shows departure from normality at the tails due to the outliers that occurred primarily 
#in the begining and the end.
#The last plot on the bottom gives p-values for the Ljung-Box-Pierce statistic for each lag up to 20. 
#These tests consider the accumulated residual autocorrelation from lag 1. 
#The dashed blue line is at 0.05, and nearly all p-values are above it, which is a GOOD result for this test.


#ARMA(1,1) model for electricity
sarima(olssonElectric$Interpolative_Value, 1, 0, 1) 
#ARMA(2,2) model for heating
sarima(olssonHeating$Interpolative_Value, 2, 0, 2) 

#ARMA(3,1) model for chilledwater
sarima(olssonchilledwater$Interpolative_Value, 3, 0, 1) 




#***************************************************************
#Fit GARCH(1,1) model on Olsson hall energy
#***************************************************************
garchfit <- garchFit(~garch(1,1),data=olssonEnergy$Interpolative_Value,include.mean = F, trace = F)
summary(garchfit)



#***************************************************************
#Recognize the pattern by spectral analysis-Parametric Estimation of the Spectral Density
#***************************************************************

#Reference book: tsa3, Chap.4.6
n = length(olssonEnergy$Interpolative_Value)
AIC = rep(0, 30) -> AICc -> BIC
for (k in 1:30){
  sigma2 = ar(olssonEnergy$Interpolative_Value, order=k, aic=FALSE)$var.pred
  BIC[k] = log(sigma2) + (k*log(n)/n)
  AICc[k] = log(sigma2) + ((n+k)/(n-k-2))
  AIC[k] = log(sigma2) + ((n+2*k)/n)
}
IC = cbind(AIC, BIC+1)
ts.plot(IC, type="o", xlab="p",ylab="AIC / BIC")

speaic=spec.ar(olssonEnergy$Interpolative_Value, log ="no")

#AR(8)
par(mfrow=c(2,3))
speaic=spec.ar(olssonEnergy$Interpolative_Value, log ="no")
speaic=spec.ar(olssonHeating$Interpolative_Value, log ="no")
speaic=spec.ar(olssonchilledwater$Interpolative_Value, log ="no")
speaic=spec.ar(olssonElectric$Interpolative_Value, log ="no")
speaic=spec.ar(olssonSteam$Interpolative_Value, log ="no")







#**************************************************************
#********************                    **********************
#********************This semester's code**********************
#********************                    **********************
#**************************************************************


#***************************************************************
#Check the regression of building information
#***************************************************************
dataset <- read.csv("buildinginfo_clean.csv",header=TRUE)
dataset$BLDGVALUE <- as.numeric(dataset$BLDGVALUE)
dataset$NET_SF <- as.numeric(dataset$NET_SF)
l1 <- lm(BLDGVALUE~FLOORS+GROSS_SF+PRIMARY_USE+FACILITY_TYPE+NET_SF+District,data=dataset)
summary(l1)
anova(l1)

library(MASS)
step <- stepAIC(l1, direction="both")
step$anova # display results

l2 <- lm(BLDGVALUE ~ FLOORS + PRIMARY_USE + FACILITY_TYPE + District,data=dataset)
summary(l2)
anova(l2)
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
#Diagnostic Plots
plot(l2)


#***************************************************************
#Overview of the Olsson Hall energy
#***************************************************************
library(ggplot2)
library(reshape2)

energy<- read.csv("Energy.csv")
datao <- subset(energy, grepl("202", energy$buildingName))
datao <- head(datao,1) 

datao <- melt(datao[, 2:ncol(datao)])
plot(datao$variable,datao$value)


