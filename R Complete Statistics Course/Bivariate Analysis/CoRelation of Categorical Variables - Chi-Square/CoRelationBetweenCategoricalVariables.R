# Strength of association between 2 Categorical Variables

LungsData <- read.delim(file.choose(),header=T)
View(LungsData)

attach(LungsData)
names(LungsData)

class(Smoke)
class(Gender)

levels(Gender)
levels(Smoke)

TAB <- table(Gender,Smoke)
TAB

# Plot of Categorical Table
barplot(TAB,beside=T,legend=T)

library("epiR")
help(package= "epiR")

TAB2 <- matrix(c(44,314,33,334),nrow = 2,byrow = T)
TAB2


TAB3  <- cbind(TAB[,2],TAB[,1])
colnames(TAB3) <- c("Yes","No")
TAB3

epi.2by2(TAB3,method= "cohort.count")


# ================= Chi-Square Independence Test for Comparing 2 Categorical Variabkes =====================
CHI <- chisq.test(TAB,correct=T)

attributes(CHI)
CHI$expected


# Fisher Exact Test is Equivalent to CHI-SQUARE Test
fisher.test(TAB,conf.int = T,conf.level = 0.99)
# P-value = 18% so there is no relation between Smoker and Gender










