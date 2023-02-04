# Multi Column Regression

install.packages('GGally')
install.packages("pls")

library(olsrr)
library(ggplot2)
library(GGally)

df_hamilton <- read.table("datasets/hamilton.txt",header = TRUE)
View(df_hamilton)

GGally::ggpairs(df_hamilton)
# Here X2 is Correlated with X1 


df_equalOpp <- read.table("datasets/equalOpportunity.txt", header = TRUE)
View(df_equalOpp)


lm_equalOpp1 <- lm(ACHV ~ SCHOOL + PEER + FAM, data = df_equalOpp)
summary(lm_equalOpp1)


# Analysis of Variance
aov(lm_equalOpp1)
# School have less variance


ols_plot_resid_fit(lm_equalOpp1)

df_equalOpp[,2:4]
GGally::ggpairs(df_equalOpp[,2:4])


lm_equalOpp2 <- lm(ACHV ~ SCHOOL, data = df_equalOpp)
summary(lm_equalOpp2)


lm_equalOpp4 <- lm(ACHV ~ SCHOOL + FAM, data = df_equalOpp)
summary(lm_equalOpp4)


lm_equalOpp3 <- lm(ACHV ~ SCHOOL + PEER, data = df_equalOpp)
summary(lm_equalOpp3)


df_frenchImports <- read.table("datasets/frenchEconomy.txt", header = TRUE)
View(df_frenchImports)

lm_frenchImports1 <- lm(IMPORT ~ DOPROD + STOCK + CONSUM, data = df_frenchImports)
summary(lm_frenchImports1)


# Analysis of Variance
aov(lm_frenchImports1)


ols_plot_resid_fit(lm_frenchImports1)
ols_plot_resid_stud(lm_frenchImports1)

# Subset Year Wise Tracking
lm_frenchImports2 <- lm(IMPORT ~ DOPROD + STOCK + CONSUM,data = subset(df_frenchImports, YEAR < 60))
summary(lm_frenchImports2)
# Coefficient of DOPROD -ve, when it should be positive


ols_plot_resid_stud(lm_frenchImports2)
cor(df_frenchImports[2:5])

#=========================================================================
df_advertising <- read.table("datasets/advertising.txt", header = TRUE)
View(df_advertising)


lm_advertising1 <- lm(St ~ At + Pt + Et + At.1 + Pt.1, data = df_advertising)
summary(lm_advertising1)

aov(lm_advertising1)
cor(df_advertising[2:6])

# check change if one variable is dropped
lm_advertising2 <- lm(St ~ Pt + Et + At.1 + Pt.1, data = df_advertising)
summary(lm_advertising2)

# change in Pt, At.1, and Pt.1
# check At with others

lm_advertisingTestAt <- lm(At ~ Pt + At.1 + Pt.1, data = df_advertising)
summary(lm_advertisingTestAt)
# discovered: At + Pt + At.1 + Pt.1 = 5

# VIF: VIF[j] = 1/(1 - R^2[j]), j = 1...p
# The value of VIF j also measures the amount by which the variance of the jth
# regression coefficient is increased due to the linear association of Xj with other
# predictor variables relative to the variance that would result if Xj were not related
# to them linearly. This explains the naming of this particular diagnostic.

library(car)
vif(lm_equalOpp1)

# dropping one of the variables will not eliminate collinearity
vif(lm_frenchImports1)


# A regression equation containing either CONSUM or DOPROD along with STOCK will eliminate collinearity
summary(lm_frenchImports2)
vif(lm_frenchImports2)

lm_frenchImports3 <- lm(IMPORT ~ DOPROD + STOCK, data = df_frenchImports)
summary(lm_frenchImports3)
vif(lm_frenchImports3)
# vif should not be greter than 10 otherwise it is MultioCollinearity sign


summary(lm_advertising1)
vif(lm_advertising1)
# Remove one by one variable  and then check vif
Summary(lm_advertising2)
vif(lm_advertising2)
# Now all vif is almost one by removing only 1 variable


# Here the prescription might be to regress sales St against Et and three of the
# remaining four variables (At, Pt, At-I, St-t) and examine the resulting VIF/s to
# see if collinearity has been eliminated.


# principal components regression using pls

library(pls)
lm_frenchImportsPcr <- pcr(IMPORT ~ ., data = df_frenchImports, scale = TRUE,validation = "CV")
summary(lm_frenchImportsPcr)
validationplot(lm_frenchImportsPcr)


lm_advertisingPcr <- pcr(St ~ ., data = df_advertising,validation = "CV")
summary(lm_frenchImportsPcr)
validationplot(lm_advertisingPcr)

library(leaps) # for regsubsets()


