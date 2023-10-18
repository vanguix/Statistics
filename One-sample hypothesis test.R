setwd("C:\\Users\\victoria\\Downloads")

MINUTES = read.csv("MINUTES.csv", sep = ";", dec = ".")

attach(MINUTES)

#First, we plot the data
dev.new()
hist(Minutes, freq = F)

#Hypothesis 0: Spanish who have practiced sport during 2023 spent 
# 326 minutes per week

#Hyphotesis 1: They practice more than 326 minutes per week

t.test(Minutes, mu = 326, alternative="g")

#pvalue= 0.9995, there is not enough evidence to accept hypothesis 1


# Explain if your test is asymptotic and why: To determine if t-test is asymptotic,

#1)The sample size is big enough to apply the CLT
dim(MINUTES) #Results approximate to normality due to CLT

#2) Verifying that data follows a normal distribution and if the t-test is appropriate

#normality test as prueba de Shapiro-Wilk can be applied for this
shapiro.test(MINUTES$Minutes) #p-value = 9.621e-12

#As the p-value here is smaller than 0.05, there is enough evidence to
#reject the normality hypothesis in the data --> t-test is not appropriate

#CONCLUSION: The test is NOT asymptotic

#However, even if the variable MINUTES does not follow a Normal distribution (which is the case)
# an asymptotic interval for mu could be constructed
