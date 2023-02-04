# Wilcox Rank Sum Test is used for examining Difference between  median of Two Independent Population.
# Numeric Outcome and  categorical Explanotory Variable X with 2 groups .

LungsData <- read.table(file.choose(),header=T,sep="\t")
View(LungsData)

attach(LungsData)
names(LungsData)

class(LungCap)
class(Smoke)

levels(Smoke)

?wilcox.test

boxplot(LungCap ~ Smoke)

# Ho = Median Lung Capacity of both Smoker and Non Smoker are equals
# H1 = Median Lung Capacity of both Smoker and Non Smoker are not equals 

# Two Sided Test
wilcox.test(LungCap ~ Smoke,mu=0,alt="two.sided",conf.int=T,conf.level=0.95,paired=F,exact=T,correct=T)
wilcox.test(LungCap ~ Smoke,mu=0,alt="two.sided",conf.int=T,conf.level=0.95,paired=F,exact=F,correct=T)
# Difference in Medians = -0.8000564 
# If we dont give conf.int=T then it will not give Confidence Interval


# ========================================== Wilcoxon Signed Rank Test =======================================================
# This is used for examining Median difference in observations for 2 Populations that  are paired or dependent on one another.


Bloodpressure <- read.table(file.choose(),header=T,sep="\t")
View(Bloodpressure)

attach(Bloodpressure)
names(Bloodpressure)

Bloodpressure[c(1,3,5),]

# Wilcox.test is used for examining Difference between  Median of Two Population.
?wilcox.test

boxplot(Before,After)

# Ho = Median change in both population   = 0
# H1 = Median change in both population  != 0

wilcox.test(Before,After,mu=0,alt="two.sided",paired = T,conf.level = 0.99,conf.int = T,exact = F,correct=F)












