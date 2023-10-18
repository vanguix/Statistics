setwd("C:/Users/laram/Desktop/Todo/UC3M/Statistics/Proyecto")


#We select minutes as calculating its mean makes more sense
#to the project objectives
MINUTES = read.csv("MINUTES.csv", sep = ";", dec = ".")

attach(MINUTES)

#First, we plot the data
dev.new()
hist(Minutes, freq = F)
dev.new()
hist(AGE, freq = F)

# 95% confidence interval for the population mean grade assuming a 
# Gaussian distribution for the minutes.
t.test(Minutes)
n= length(Minutes)
m=mean(MINUTES$Minutes)
sigma = sd(MINUTES$Minutes)
z.0.025 = qnorm(0.975)

#For large samples
m-z.0.025*sigma/sqrt(n) #lower bound: 229.655
m+z.0.025*sigma/sqrt(n) #upper bound: 300.6822

#For Gaussian data it would be:
#m-qt(0.975,n-1)*sd(Minutes)/sqrt(n) # 229.4019
#m+qt(0.975,n-1)*sd(Minutes)/sqrt(n) # 300.9353

#95 percent confidence interval: 229.655 300.6822

#Asymptotic interval? YES:
#As the sample size is large enough, the confidence interval
#can be considered asymptotic because it approximates a normal
#distribution and uses the z-distribution

#INTERPRETATION OF THE INTERVAL
#With 95% confidence, it can be said that the true average (mean)
#time spent, represented by "Minutes," is somewhere between
#229.655 minutes and 300.6822 minutes. This interval provides a
#range within which the true population mean is likely to fall.