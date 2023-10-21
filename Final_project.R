#The project will try to confirm these two hypotheses extracted from the 2022 Survey:
  
  #More than 60% of the Spanish population practiced some kind of sport during last year.
  #Spanish who have practiced sport during 2023 spent at least 326 minutes per week.

#Using statistical methods and clear visual representations, the aim is to present 
#a comprehensive view of sports engagement and to confirm or dismiss the previous 
#hypotheses with the information from the survey. 
library("RColorBrewer")
library(fitdistrplus)

setwd("C:\\Users\\victoria\\OneDrive - Universidad Rey Juan Carlos\\Escritorio\\MASTER\\Statistics for data analysis\\Project\\Statistics")


survey = read.csv("SPORTS_SURVEY.csv",sep = ";", dec = ",")
attach(survey)
names(survey)


## DESCRIPTIVE ANALYSIS: some plots of the data (rest were obtained from Google survey)

#Q1. What is your date of birth?
dev.new()
hist(AGE, freq = F, col= "deepskyblue3")
age_mean= mean(AGE)
age_var = var(AGE)
age_mean
age_var

#Q4. How many types of sports do you practice on a monthly basis?
dev.new()
barplot(table(N_SPORTS),main = "Number of different sports",
        xlab = "Number of sports",
        ylab = "Frequency", col=brewer.pal(n = 4, name = "Paired") )


#Q6. How many minutes did you spend practicing sports last week?
dev.new()
hist(MINUTES, freq = F, col= "deepskyblue3")



##MODEL FITTING

# We choose to fit a univariate distribution model for the minutes variable. 
#By observing the plot, we will try with exponential, gamma and lognormal,
# and compare the results 

#Before fitting, we need to get rid of the 0 on the data, as some distributions
#do not support them.

df_survey = data.frame(survey)
df_nonzero = df_survey[df_survey$MINUTES != 0, , drop = FALSE]
MINUTES_NZ= df_nonzero[,"MINUTES"]
hist(MINUTES_NZ, freq = F, col= "deepskyblue3")

#Fit by MM
fit_MM_exp = fitdist(MINUTES_NZ,"exp",method="mme")
fit_MM_lnorm = fitdist(MINUTES_NZ,"lnorm",method="mme")
fit_MM_gamma = fitdist(MINUTES_NZ,"gamma",method="mme")

fit_MM_exp$estimate
fit_MM_lnorm$estimate
fit_MM_gamma$estimate


#Fit by MLE 
fit_MLE_exp = fitdist(MINUTES_NZ,"exp",method="mle")
fit_MLE_lnorm = fitdist(MINUTES_NZ,"lnorm",method="mle")
fit_MLE_gamma = fitdist(MINUTES_NZ,"gamma",method="mle")

fit_MLE_exp$estimate
fit_MLE_lnorm$estimate
fit_MLE_gamma$estimate

#Plot all together on the histogram
dev.new()
hist(MINUTES_NZ, freq = FALSE, col = 'white', main = 'Distribution fitting', ylim = c(0, 0.004))

curve(dexp(x, rate = fit_MM_exp$estimate), from = 0, col = "red", add = TRUE,lwd = 2)
curve(dexp(x, rate = fit_MLE_exp$estimate), from = 0, col = "blue", add = TRUE, lty = 2,lwd = 2)

grid = seq(0, 1400, 1)
lines(grid, dlnorm(grid, fit_MM_lnorm$estimate[1], fit_MM_lnorm$estimate[2]), col = 'blueviolet', lwd = 2)  
lines(grid, dlnorm(grid, fit_MLE_lnorm$estimate[1], fit_MLE_lnorm$estimate[2]), col = 'cyan4', lwd = 2)  

lines(grid, dgamma(grid, fit_MM_gamma$estimate[1], fit_MM_gamma$estimate[2]), col = "deeppink", lwd = 2)  
lines(grid, dgamma(grid, fit_MLE_gamma$estimate[1], fit_MLE_gamma$estimate[2]), col = "chartreuse3", lty = 2, lwd = 2)  

legend("topright", c("MM exp", "MLE exp", "MM lnorm", "MLE lnorm", "MM gamma", "MLE gamma"), col = c("red", "blue", 'blueviolet', 'cyan4', 'deeppink', 'chartreuse3'), lty = c(1, 2, 1, 1, 1, 2), lwd = 2)  # Ajusta el grosor a 2

# Plot on the ecdf
dev.new()
plot(ecdf(MINUTES_NZ),main='Distribution fitting')
curve(pexp(x, rate = fit_MM_exp$estimate), from = 0, col = "red", add = TRUE,lwd = 2)
curve(pexp(x, rate = fit_MLE_exp$estimate), from = 0, col = "blue", add = TRUE, lty = 2,lwd = 2)

lines(grid, plnorm(grid, fit_MM_lnorm$estimate[1], fit_MM_lnorm$estimate[2]), col = 'blueviolet', lwd = 2)  
lines(grid, plnorm(grid, fit_MLE_lnorm$estimate[1], fit_MLE_lnorm$estimate[2]), col = 'cyan4', lwd = 2)  

lines(grid, pgamma(grid, fit_MM_gamma$estimate[1], fit_MM_gamma$estimate[2]), col = "deeppink", lwd = 2)  
lines(grid, pgamma(grid, fit_MLE_gamma$estimate[1], fit_MLE_gamma$estimate[2]), col = "chartreuse3", lty = 2, lwd = 2)  

legend("topleft", c("MM exp", "MLE exp", "MM lnorm", "MLE lnorm", "MM gamma", "MLE gamma"), col = c("red", "blue", 'blueviolet', 'cyan4', 'deeppink', 'chartreuse3'), lty = c(1, 2, 1, 1, 1, 2), lwd = 2)  # Ajusta el grosor a 2


# Model comparison using AIC

fit_MM_exp$aic
fit_MM_lnorm$aic
fit_MM_gamma$aic


fit_MLE_exp$aic
fit_MLE_lnorm$aic
fit_MLE_gamma$aic


# By the AIC, Gamma MLE fits better.

## STATISTICAL INFERENCE OF ONE VARIABLE 

#First hypothesis: More than 60% of the Spanish population practiced some kind 
# of sport during last year.

n=172 #total of people in the sample
e= sum(SPORT_B == 'Si')  # persons who do exercise
p=0.60 #proportion of people that exercise
binom.test(e, n, p, alternative="g")

#pvalue= 2.2e-16--> we can confirm the hypothesis


#Second hypothesis: Spanish who have practiced sport during 2023 spent 
#more than 326 minutes per week

t.test(MINUTES, mu = 326, alternative="g")

#pvalue= 0.9995, there is not enough evidence to accept the hypothesis 



## STATISTICAL INFERENCE OF TWO VARIABLES 

        #Chi-squared hypothesis test
    #Test the dependence between doing sports or not and sex
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

        ##Multiple sample hypothesis test (ANOVA)
      #Test for differences in the mean minutes of sport per week
#of people doing different number of type of sports (from 0 to +3).

dev.new()
boxplot(MINUTES~N_SPORTS, col='deepskyblue3')
summary(aov(MINUTES~N_SPORTS))


# Null hypothesis can be rejected as p value is smaller than alpha, or similarly, 
# the factor is statistically significant , so there is enough evidence to affirm 
# that THERE IS AT LEAST A PAIR OF GROUPS WITH DIFFERENT MEANS.



