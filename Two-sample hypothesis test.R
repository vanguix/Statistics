setwd("C:\\Users\\victoria\\OneDrive - Universidad Rey Juan Carlos\\Escritorio\\MASTER\\Statistics for data analysis\\Project\\Statistics")


x = read.csv("MINUTES_N_SPORTS.csv",sep = ";", dec = ",")
attach(x)
names(x)
# Test for differences in the mean minutes of sport per week
#of people doing 1 type of sport or doing more than 3.
Y1 = Minutes[Num_sports=="1"]
Y2 = Minutes[Num_sports=="3+"]

dev.new()
boxplot(Y1,Y2)
dev.new()
hist(Y1,freq=F)
dev.new()
hist(Y2,freq=F)

#From the plots its difficult to determine that the samples are following a Gaussian
length(Y1)
length(Y2)

#However, as the lengths are high enough we can use the CLT to approximate a t.test
#As we cannot be sure if variances are equal or not, we will perform both

t.test(Y1,Y2,var.equal = T)
t.test(Y1,Y2,var.equal = F)


# On both cases, null hypothesis can be rejected as p value is smaller than alpha, 
# so there is enough evidence to affirm that there is a difference between the means.