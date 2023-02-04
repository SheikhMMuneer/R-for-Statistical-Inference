# One Sample T-tet and Confidence Interval 

LungsData <- read.table(file.choose(),header=T,sep="\t")
View(LungsData)

attach(LungsData)
names(LungsData)

class(LungCap)

help("t.test")

boxplot(LungCap)

# Null Hypothesis is mean is 8 
# Alternative Hypothesis is mean is < 8

# One-Sample t-test with 95% Confidence Interval
t.test(LungCap,mu=8,alternative = "less",conf.level = 0.95)

# One Sample Two-Sided t-test with 95% Confidence Interval
Test <- t.test(LungCap,mu=8,alternative = "two.sided",conf.level = 0.95)
Test

#Attributes of Test
attributes(Test)

# Two Sample t-test for Difference in Two opulation
boxplot(LungCap~Smoke)

# H0 = both Smoker and Non-Smoker LungCap mean is equal
# H1 = both Smoker and Non-Smoker LungCap mean is not equal
# Assume not equal variances

two_Test <- t.test(LungCap~Smoke,mu=0,alternative = "two.sided",conf.level = 0.95,var.eq=F,paired=F)
#two_Test <- t.test(LungCap[Smoke=="no"],LungCap[Smoke=="yes"],mu=0,alternative = "two.sided",conf.level = 0.95,var.eq=F,paired=F)
two_Test

# How to Compare Variance for two Groups
var(LungCap[Smoke=="no"])
var(LungCap[Smoke=="yes"])
# Both variance are different so var.equal would be Falase


# Levene Test when to check whether Population variances are equal or not.
# Ho = Population variances are equal
# H1 = Population variances are not  equal

install.packages("car")
library("car")

leveneTest(LungCap~Smoke)
# Since p-value is small enough we reject Null hypothesis and accept that Population variances are not equal








