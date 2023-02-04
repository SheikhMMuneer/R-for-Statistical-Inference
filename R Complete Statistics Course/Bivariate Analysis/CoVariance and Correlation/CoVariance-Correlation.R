# Relationship between 2 numeric Variables

# (this data has a 'nice' non-linearity to work with)
LungCapData <- read.table(file = file.choose(), sep=",", header=T)
View(LungCapData)


# and attach the data
attach(LungCapData)

?cor.test
plot(Age,LungCap,main="Scatterplot",las=1)

cor(Age,LungCap ,method="pearson")  # default
cor(Age,LungCap ,method="spearman")
cor(Age,LungCap ,method="kendall")

# Ho = Correlation is equal to 0
# H1  = Correlation is not equal to 0

cor.test(Age,LungCap ,method="pearson")
# p-value = 0 this mean correlation is not 0

# One-Sided Test
cor.test(Age,LungCap ,method="pearson",alt="greater",conf.level= 0.99)
cor.test(Age,LungCap ,method="spearman",exact=F)

# Covariance
cov(Age,LungCap)

#use pairs for Numeric Variables Only
pairs(LungCapData[1:3])

cor(LungCapData[,1:3])





