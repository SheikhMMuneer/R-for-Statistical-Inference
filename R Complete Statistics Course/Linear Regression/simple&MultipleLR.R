
LungsData <- read.delim(file.choose(),header=T)
View(LungsData)

attach(LungsData)
names(LungsData)

class(Age)
class(LungCap)

plot(Age,LungCap,main="Scatterplot")
cor(Age,LungCap)

mod  <- lm(LungCap ~ Age)
summary(mod)

attributes(mod)

mod$coefficients
mod$coef
coef(mod)


plot(Age,LungCap,main="Scatterplot")
abline(mod,col=2,lwd=3)


# Confidence Interval
confint(mod)
confint(mod,level=0.99)

# Analysis of Variance
anova(mod)
# It tell about f-statistics

abline(mod)
# To show plot in On windows by subplot
par(mfrow=c(2,2))
plot(mod)


# In Fitted vs Residual Plot if there is a curve in line then it is Non-Linear Model


# ========================== Multiple Linear Regression ===========================
class(Smoke)  # factors
levels(Smoke)

model1  <- lm(LungCap ~ Age + Height)
summary(model1)
# Multiple R-Squared is 84% which mean 84% variability in LungCap can be explained by Age and Height

# Null Hypothesis is that All Model coefficients are 0 mean ( Age and Height Coefficients are 0)
# Std Error  = e = y-y~ mean (Y - expected Y)
# Intercept is Estimated Y value when all Xs are 0 mean when Age and Heights are 0
# Age = 0.126 mean 1 year increase in Age will Increase LungsCapacity by 0.126

anova(model1)
cor(Age,Height,method="pearson")  #83%


# Confidence Interval for Model Coefficients
confint(model1,conf.level=0.95)


# Multiple Linear Regression Variables
model2  <- lm(LungCap ~ Age + Height + Smoke + Gender + Caesarean)
summary(model2)

plot(model2)  # This is linear relation

# Converting Numeric Variable into Categorical
CatHeight <- cut(Height,breaks = c(0,50,55,60,65,70,100),labels = c("A","B","C","D","E","F"))
CatHeight
# CatHeight <- cut(Height,break = c(0,50,55,60,65,70,100),labels = c("A","B","C","D","E","F"),right = FALSE)

Height[1:10]
CatHeight[1:10]


# ========================= Creating Dummy or Indicator Variable =======================
# We can include Categorical variable into Regression using Dummy Variable

levels(Smoke)
levels(CatHeight)

mean(LungCap[CatHeight=="A"])
mean(LungCap[CatHeight=="B"])
mean(LungCap[CatHeight=="C"])
mean(LungCap[CatHeight=="D"])
mean(LungCap[CatHeight=="E"])
mean(LungCap[CatHeight=="F"])

mod <- lm(LungCap ~ CatHeight)
summary(mod)

# Assume would be of mean of Specific Category
# R did Dummy Encoding automatically when give categorical variable

# ================= Change Baseline Category for a Categorical Variable ===================
help("relevel")

mod1  <- lm(LungCap ~ Age + Smoke)
summary(mod1)

table(Smoke)

# Change Arrangement 
Smoke <- relevel(Smoke,ref="yes")
table(Smoke)

# Reparameterizing the Model
mod2  <- lm(LungCap ~ Age + Smoke)
summary(mod2)

# ================= Group Categorical Variable in Regression Model  =====================

class(Smoke)
levels(Smoke)


model1  <- lm(LungCap ~ Age + Smoke)
summary(model1)
# NoSmoke include in Age 1+08572 is NoSmoker
# Yes Smoker include 1.08572 + 0.55540 - 0.64859

# First plot the Data of Non-Smoker in Blue
plot(Age[Smoke=="no"],LungCap[Smoke=="no"],col="blue",ylim=c(0,15),xlab="Age",ylab="Lung Cap",main="Lung Cap vs Age,Smoke")

# Second plot the Data of Smoker in Red
points(Age[Smoke=="yes"],LungCap[Smoke=="yes"],col="red",pch=16)

legend(3,15,legend=c("Non-Smoker","Smoker"),col=c("blue","red"),pch=c(1,16),bty="n")

# First for Non-Smoker in Blue
abline(a= 1.08,b=0.555,col="blue",lwd="3")

# Second for Smoker in Red
abline(a= 0.431,b=0.555,col="red",lwd="3")
# There is no Interaction between Smoking and Age.

# =================================
class(CatHeight)
levels(CatHeight)


model1  <- lm(LungCap ~ Age + CatHeight)
summary(model1)


# First plot the Data of Non-Smoker in Blue
plot(Age[CatHeight=="A"],LungCap[CatHeight=="A"],col=2,ylim=c(0,15), xlim=c(0,20),xlab="Age",ylab="Lung Cap",main="Lung Cap vs Age,CatHeight")

# Second plot the Data of CatHeight B in Red
points(Age[CatHeight=="B"],LungCap[CatHeight=="B"],col=3)
points(Age[CatHeight=="C"],LungCap[CatHeight=="C"],col=4)
points(Age[CatHeight=="D"],LungCap[CatHeight=="D"],col=5)
points(Age[CatHeight=="E"],LungCap[CatHeight=="E"],col=6)
points(Age[CatHeight=="F"],LungCap[CatHeight=="F"],col=7)

# First for Non-Smoker in Blue
abline(a= 0.98,b=0.20, col=2,lwd="3")
abline(a= 2.46,b=0.20,col=3,lwd="3")
abline(a= 3.67,b=0.20,col=4,lwd="3")
abline(a= 4.92,b=0.20,col=5,lwd="3")
abline(a= 5.99,b=0.20,col=6,lwd="3")
abline(a= 7.52,b=0.20,col=7,lwd="3")
abline(a= 1.08,b=0.20,col=8,lwd="3")

legend(3,15,legend=c("A","B","C","D","E","F"),col=c(2,3,4,5,6,7),bty="n")


# ==================== Categorical Variables Coefficient Relationhip by Regression ===========================
# Fit a Regression Model using Age Smoke and Their Interaction
# * return Age,Smoke and AgeSmoke

modl1 <- lm(LungCap ~Age*Smoke)
# We can use :  for Same Task
modl1 <- lm(LungCap ~ Age+Smoke+Age:Smoke)
coef(modl1)
summary(modl1)

# As Age:Smokeyes P value =  0.377 which is 37% not significan that'swhy we cant include this in our model

# =================== Partial F-Test for Comparing Model  =================
# Partial F-Test is used in Model Building and Model Selection to help decide if a variable can be removed from a model.
# Partial F-Test is used to compare Nested Models for Comparing RSS is less or not
# Larger Model = Full Model
# Reduced Model = one or more variable removed.

# Suppose we have a model to estimate the mean Lung Capacity using Age,Gender,Smoke and Height this is Full Model
# Suppose we have a model to estimate the mean Lung Capacity using Age,Gender,Smoke  this is Reduced Model
# Does including Height decrease Error and increase predictive power?
# Does excluding Height decrease error and increase significant power
# RSS (Residual Sum of Squared Error) should be less. (RSS = Difference between Actual Y - observed Y)

# Partial F-Tets HO = No Significant Difference in SSE of full and Reduced Model.
# HA  = There is Significant Difference in SSE of full Model than Reduced Model.(Full Model is lower SSE)

# Find the Full Model
Full.Model <- lm(LungCap ~ Age + I(Age^2))
summary(Full.Model)

# Find the Reduced Model
Reduced.Model <- lm(LungCap ~ Age)
summary(Reduced.Model)

# Seeing  R-Squared and RSS  Full Model is better.

#Perform Partial F-Test
anova(Reduced.Model,Full.Model)
# P-value is 0.17 which is 17% so we accept Null Hypothesis that Full Model is not Significantly better.


# Now onMultiple Linear Regression Variables
# Fit the Full Model and called it Model1
model_Full  <- lm(LungCap ~ Age +Gender + Smoke + Height)
summary(model_Full)

# Fit the Reduced Model and called it Model1
model_Reduced  <- lm(LungCap ~ Age +Gender + Smoke)
summary(model_Reduced)

# Compare the Two Model Using Partial F-Test
anova(model_Reduced,model_Full)
# P-Value is 0 so we concluded that Full Model is significantly better than Reduced Model
