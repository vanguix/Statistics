setwd("C:\\Users\\victoria\\OneDrive - Universidad Rey Juan Carlos\\Escritorio\\MASTER\\Statistics for data analysis\\Project")

x = read.csv("sex_sport.csv", sep = ";", dec = ",")
attach(x)
names(x)


# Test the dependence between doing sports or not and sex
chisq.test(SPORT_B,SEX)
chisq.test(SPORT_B,SEX)$observed
chisq.test(SPORT_B,SEX)$expected

# Cramers' V coefficient
Chi = chisq.test(SPORT_B,SEX)$statistic
N = length(SPORT_B)
V = sqrt(Chi/N)
V

#p-value = 0.466
#there is not sufficient evidence to say they are dependent
#Cramers' V coefficient=0.05559055, as closer to 0, more independency.
