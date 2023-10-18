setwd("C:\\Users\\victoria\\OneDrive - Universidad Rey Juan Carlos\\Escritorio\\MASTER\\Statistics for data analysis\\Project")
install.packages("RColorBrewer")
library("RColorBrewer")

AGES = read.csv("AGES.csv", sep = ";", dec = ".")
MINUTES = read.csv("MINUTES.csv", sep = ";", dec = ".")
NUM_SPORTS = read.csv("NUM_SPORTS.csv", sep = ";", dec = ".")
attach(AGES)
attach(MINUTES)
attach(NUM_SPORTS)
#First, we plot the data to choose a valid model to fit
dev.new()
hist(Minutes, freq = F)
dev.new()
hist(AGE, freq = F, col=brewer.pal(n = 7, name = "Paired"))

dev.new()
barplot(table(Num_sports),main = "Number of different sports",
        xlab = "Number of sports",
        ylab = "Frequency", col=brewer.pal(n = 4, name = "Paired") )
