#Chapter4
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


#Start
library(AED); data(Squid)
Squid$fMONTH <- factor(Squid$MONTH)
M1 <- lm(Testisweight ! DML * fMONTH, data = Squid)
op <- par(mfrow = c(2, 2), mar = c(4, 4, 2, 2))
plot(M1, which = c(1), col = 1, add.smooth = FALSE, caption = "")
plot(Squid$fMONTH, resid(M1), xlab = "Month", ylab = "Residuals")
plot(Squid$DML, resid(M1), xlab = "DML", ylab = "Residuals")
par(op)

#fixed variance structure
library(nlme)
M.lm <- gls(Testisweight ! DML * fMONTH, data=Squid)
vf1Fixed <- varFixed(!DML)
M.gls1 <- gls(Testisweight ! DML * fMONTH, weights = vf1Fixed, data = Squid)
anova(M.lm, M.gls1)


#VarIdent
vf2 <- varIdent(form= ! 1 | fMONTH)
M.gls2 <- gls(Testisweight ! DML*fMONTH, data =Squid, weights = vf2)
anova(M.lm, M.gls1, M.gls2)
anova(M.lm, M.gls2)
summary(M.gls2)
plot(M.lm,which = c(1), col = Squid$MONTH, add.smooth = FALSE, caption = "")
E <- resid(M.lm)
coplot(E ! DML | fMONTH, data = Squid)

#VarPower
vf3 <- varPower(form =! DML)
M.gls3 <- gls(Testisweight ! DML * fMONTH, weights = vf3, data = Squid)

vf4 <- varPower(form =! DML | fMONTH)
M.gls4 <- gls(Testisweight ! DML * fMONTH, data = Squid, weights = vf4)

#varExp
vf5 <- varExp(form =~ DML)
M.gls5 <- gls(Testisweight ~ DML * fMONTH, weights = vf5, data = Squid)

#VarConstPower
vf6 <- varConstPower(form =! DML)
M.gls6 <- gls(Testisweight ! DML * fMONTH, weights = vf6, data = Squid)

vf7 <- varConstPower(form =! DML | fMONTH)
M.gls7 <- gls(Testisweight ! DML * fMONTH, weights = vf7, data = Squid)

#varComb
vf8 <- varComb(varIdent(form =! 1 | fMONTH) , varExp(form =! DML) )
M.gls8 <- gls(Testisweight ! DML * fMONTH, weights = vf8, data = Squid)
anova(M.lm, M.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls6, M.gls7, M.gls8)

AIC(M.lm, M.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls6, M.gls7, M.gls8)

#Graphical Validation
E1 <- resid(M.gls4)
coplot(E1 ! DML | fMONTH, ylab = "Ordinary residuals", data = Squid)

E2 <- resid(M.gls4, type = "normalized")
coplot(E2 ~ DML | fMONTH, data = Squid, ylab = "Normalised residuals")