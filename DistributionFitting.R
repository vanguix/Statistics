setwd("C:\\Users\\victoria\\Downloads")
install.packages("RColorBrewer")
library("RColorBrewer")

AGES = read.csv("AGES.csv", sep = ";", dec = ".")
MINUTES = read.csv("MINUTES.csv", sep = ";", dec = ".")

attach(AGES)
attach(MINUTES)

#First, we plot the data to choose a valid model to fit
dev.new()
hist(Minutes, freq = F, col=brewer.pal(n = 8, name = "Paired"))
dev.new()
hist(AGE, freq = F, col=brewer.pal(n = 7, name = "Paired"))

#By observing the plots, we will try with exponential for the minutes
# and gaussian for the age
library(fitdistrplus)
#Fit by MM
fit_MM_minutes = fitdist(Minutes,"exp",method="mme")
fit_MM_minutes$estimate

fit_MM_age = fitdist(AGE,"norm",method="mme")
fit_MM_age$estimate

#Fit by MLE 
fit_MLE_minutes = fitdist(Minutes,"exp",method="mle")
fit_MLE_minutes$estimate

fit_MLE_age = fitdist(AGE,"norm",method="mle")
fit_MLE_age$estimate

#plots
dev.new()
hist(Minutes, freq = F, col=brewer.pal(n = 8, name = "Paired"))
curve(dexp(x, rate = fit_MM_minutes$estimate), from = 0,col= "blue", add = TRUE)
curve(dexp(x, rate = fit_MLE_minutes$estimate), from = 0, col = "red", add = TRUE, lty = 4)
legend("topleft",c("MM","MLE"),col=c("blue","red"),lty=c(1,4))

dev.new()
hist(AGE, freq = F, col=brewer.pal(n = 8, name = "Paired"))
grid=seq(0,70,0.01)
lines(grid,dnorm(grid,fit_MM_age$estimate[1],fit_MM_age$estimate[2]),col="blue")
lines(grid,dnorm(grid,fit_MLE_age$estimate[1],fit_MLE_age$estimate[2]), col = "red", lty = 4)
legend("topleft",c("MM","MLE"),col=c("blue","red"),lty=c(1,4))

# Model comparison using AIC
fit_MM_minutes$aic
fit_MLE_minutes$aic
fit_MM_age$aic
fit_MLE_age$aic

# In both variables, the two methods get the same AIC value.
# In this case, we should choose the MLE method as MLE estimators
# have higher probability of being close to the true parameters and are
# more often unbiased than moment estimators.
