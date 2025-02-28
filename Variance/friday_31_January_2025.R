#Recreation of Training session Friday, 31 January 2025
#Linear Models
install.packages("lme4")
install.packages("nlme")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("plyr")

library("lme4")
library("nlme")
library("tidyverse")
library("dplyr")
library("plyr")

setwd("C:\\Users\\BoemoMolefe\\task.org.za\\DMJ - Document Shares\\TRAINING\\EBA MODELLING\\Resources\\Example Datasets\\Zuur et al\\RChapter4\\")
getwd()

Squid<-read.table(file="Squid.txt",header=TRUE)
view(Squid)

Squid$fMONTH <- factor(Squid$MONTH)

#data overview
view(Squid)
summary(Squid)
str(Squid)
unique(Squid$YEAR)
unique(Squid$Location)

#Linear models
model_1 <- lm(GSI ~ YEAR  + fMONTH, data = Squid)
summary(model_1)
AIC(model_1)

model_2 <- lm(GSI ~ YEAR  + YEAR:fMONTH, data = Squid)
summary(model_2)
AIC(model_2)

model_3 <- lm(GSI ~ fMONTH, data = Squid)
summary(model_3)
AIC(model_3)

anova(model_1,model_2)

#Thabo_Plot
op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
plot(M2, which = c(1), col = 1, add.smooth = FALSE,
     caption = "")
plot(Squid$fMONTH, resid(M1), xlab = "Month",
     ylab = "Residuals")
plot(Squid$YEAR, resid(M1), xlab = "YEAR",
     ylab = "Residuals")
par(op)

#Thabo_Plot
Argentina<-read.table(file="Argentina.txt",header=TRUE)
names(Argentina)
boxplot(Argentina$L.acuta, xlab="L. acuta")

view(Argentina)






