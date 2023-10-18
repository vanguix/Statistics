setwd("C:\\Users\\victoria\\OneDrive - Universidad Rey Juan Carlos\\Escritorio\\MASTER\\Statistics for data analysis\\Project\\Statistics")


x = read.csv("MINUTES_N_SPORTS.csv",sep = ";", dec = ",")
attach(x)
names(x)
# Test for differences in the mean minutes of sport per week
#of people doing different number of type of sports (from 0 to +3).


dev.new()
boxplot(Minutes~Num_sports)
summary(aov(Minutes~Num_sports))


# Null hypothesis can be rejected as p value is smaller than alpha, or similarly, 
# the factor is statistically significant , so there is enough evidence to affirm 
# that THERE IS AT LEAST A PAIR OF GROUPS WITH DIFFERENT MEANS.