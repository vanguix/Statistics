#More than 60% of the Spanish population practiced some kind of sport during last year.
x = read.csv("sex_sport.csv", sep = ";", dec = ",")
attach(x)
names(x)


n=172 #total of people in the sample
e= sum(SPORT_B == 'Si')  # persons who do exercise
p=0.60 #proportion of people that exercise
binom.test(e, n, p, alternative="g")

#pvalue= 2.2e-16