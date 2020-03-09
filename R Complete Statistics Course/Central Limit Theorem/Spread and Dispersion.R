
LungCapData <- read.table(file.choose(),sep="\t",header = T)
View(LungCapData)
attach(LungCapData)
dim(LungCapData)

class(LungCap)
class(Smoke)

levels(Smoke)

# use table for Categorical Summary
table(Smoke)
table(Smoke)/nrow(LungCapData)*100


# use summary for numeric Summary
summary(LungCap)
summary(Smoke)


#Contingency Table for 2 Categorical Variables
table(Smoke,Gender)

# ===============
mean(LungCap)
median(LungCap)

#Find Varianceby two ways
var(LungCap)
sd(LungCap)^2


# Find Standard Deviation by 2 ways
sd(LungCap)
sqrt(var(LungCap))

# Find Range values of Variables
range(LungCap)

quantile(LungCap,probs = c(0.20,0.50,0.75,0.90,1))


#Find pearson Correlation and spearman Correlation between 2 numeric Variables
cor(LungCap,Age)      # pearson
cor(LungCap,Age,method="spearman")

#Find Covariance between 2 numeric variables
cov(LungCap,Age)

# OverAll Summary
summary(LungCapData)






