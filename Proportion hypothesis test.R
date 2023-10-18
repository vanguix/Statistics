#More than 60% of the Spanish population practiced some kind of sport during last year.
x = read.csv("sex_sport.csv", sep = ";", dec = ",")
attach(x)
names(x)


n=172 #total of people in the sample
x=20# persons who do exercise
p=0.60 #proportion of people that exercise
binom.test(x, n, p, alternative="g")