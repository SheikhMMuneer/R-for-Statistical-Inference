## Importing libraries

library(tidyverse)
library(dplyr)
library(openintro)
library(UsingR)
library(statsr)
library(broom)
library(MASS)
library(car)
library(olsrr)
library(ggplot2)

##  =================== Get Mode Function==============
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


## Import Dataset
df_ames <- readr::read_csv("train.csv")
View(df_ames)

df_test <- readr::read_csv("test.csv")
View(df_test)

## DataTypes of DataSet
str(df_ames)

## Dimension of DataSet
dim(df_ames)
# There are 1460 Rows and 81 Columns

# Summary of DataSet
summary(df_ames)

## ========================= UniVariate Analysis ===================================

class(df_ames$MSSubClass)
unique(df_ames$MSSubClass)
df_ames$MSSubClass <- as.character(df_ames$MSSubClass)
class(df_ames$MSSubClass)

## As MSSubClass respresentation is of Categorical so We will convert this into Character Representation.


## ======================= Data Cleaning and Imputation  =================

All_nulls  <- colSums(is.na(df_ames))
All_nulls  <- All_nulls[All_nulls>0] 
barplot(All_nulls, main="Null Values Comparison in Ames Housing",xlab="Columns",ylab="Number of Missing Values",las=1,col=1)


# identifying variables with same values not to be considered in modeling since not significant to the study
var_to_use <- df_ames %>% summarise_all(funs(length(unique(.))))
var_to_use <- names(data.frame(var_to_use)[,var_to_use>1])


# select useful variables only for the model
df_ames <- df_ames %>% dplyr::select(var_to_use)
df_ames

# Deal with NA values 

## Seperate Numerical from Categorical variables
ames_var_types <- split(names(df_ames), sapply(df_ames, function(x) paste(class(x), collapse = " ")))
ames_var_types

## Dealing of Missing numeric values with Mean Imputation

ames_train_int <- dplyr::select_if(df_ames, is.numeric)
ames_train_int

# class(ames_train_int)

ames_train_int[is.na(ames_train_int)] <- lapply(ames_train_int, mean, na.rm = TRUE)
ames_train_int[ames_train_int=="NA"] <-  lapply(ames_train_int, mean, na.rm = TRUE) 
ames_train_int[ames_train_int==""] <-    lapply(ames_train_int, mean, na.rm = TRUE)

dim(ames_train_int)


## Dealing of Missing categorical values  with Mode Imputation for Categorical Variables

ames_train_fac <- dplyr::select_if(df_ames, is_character)
ames_train_fac <- sapply(ames_train_fac, as.character)
ames_train_fac[is.na(ames_train_fac)] <-   getmode(ames_train_fac)  
ames_train_fac[ames_train_fac==""]<- getmode(ames_train_fac)        
ames_train_fac <- data.frame(ames_train_fac)
dim(ames_train_fac)

## Merging both Numerical and Categoricals Variables in Training DataFrame
df_ames <- cbind(ames_train_int, ames_train_fac)
View(df_ames)

# =============================== Exploratory Data Analysis ==========================================

hist(df_ames$SalePrice)
# Sine Sales Price is not linear we will use log function to make this linear.

# We will use log for Linear Distribution of both Numeric Variables
ggplot(data = df_ames, aes(x = log(LotArea), y = log(SalePrice))) +  geom_point()  + geom_smooth(method = 'lm')


# Analysis of Sale Price by Sale Condition
ggplot(df_ames,aes(SaleCondition,log(SalePrice))) +  geom_boxplot(aes(col='Red')) 
# We can See that Partial Sales Condition have High Sale Price.Normal and Abnormal have Outliers values.

# Analysis of Sale Price by Sale Type
ggplot(df_ames,aes(SaleType,log(SalePrice))) +  geom_boxplot(aes(col='Brown')) 
# We can See that New Sale Type have high Sales Price than Others COD and WD have outliers values.


# Analysis of Sale Price by MSSubClass
boxplot(df_ames$SalePrice ~ df_ames$MSSubClass,main="Sale Price by MSSubClass",col="Blue")


# ======================= Linear Regression Modelling ==============================
# Full Linear Regression Model

# I will Exclude Utilities Column which have Only 1 level having same Value in All Rows which will not permit our linear model to fit Perfectly.
df_ames = subset(df_ames, select=-c(Utilities))
full_model_lm <- lm(log(SalePrice)~., data=df_ames)
summary(full_model_lm)

# In Full Model Variability of all Variables to Identify SalePrice is 95% which is Ok.but Residual Standard Error is little bit Ok
# which is 0.09

# Analyze AOV to Identify One Model Variation and Residuals Squared of each Variable
aov(full_model_lm)

par(mfrow=c(2,2))
plot(full_model_lm)

ols_plot_resid_fit(full_model_lm)
ols_plot_resid_lev(full_model_lm)
ols_plot_resid_stud(full_model_lm)
ols_plot_resid_qq(full_model_lm)
ols_plot_dffits(full_model_lm)
ols_plot_cooksd_chart(full_model_lm)
ols_plot_hadi(full_model_lm)
ols_plot_added_variable(full_model_lm)
ols_plot_comp_plus_resid(full_model_lm)


# We will make 5 Groups having 5 variables in each group to Idefiy only Significant Variables.

## ------------------------------- group_basement ------------------------------------------------------

group_basement <- as.formula(log(SalePrice) ~ BsmtQual+BsmtCond+BsmtFullBath+BsmtFinType1+BsmtFinType2)
pairs(group_basement, data = df_ames)

lm_group_basement <- lm(group_basement, data = df_ames)
summary(lm_group_basement)

par(mfrow = c(2, 2))
plot(lm_group_basement)

# Analysis of Variance
aov(lm_group_basement)

# Detect MultiCollinearity
# alias(lm_group_basement)
# attributes(alias(lm_group_basement)$Complete)$dimnames[[1]]

vif(lm_group_basement)
# We are removing BsmtFinType1 and BsmtFinType2  because Thes variable have high MultiCollinearity

# Model selection using AIC
AIC_lm_group_base <- stepAIC(lm_group_basement,direction = "backward",k=2, trace = 0, steps = 1000)
summary(AIC_lm_group_base)

# We will chose Most Significant Variables from AIC_lm_group_base Model (BsmtQual,BsmtCond)

## -------------------------------- group_Exter -------------------------------------------------------

group_Exter <-   as.formula(log(SalePrice) ~ ExterQual+ExterCond+Exterior1st+Exterior2nd+MasVnrArea)
pairs(group_Exter,data = df_ames)

lm_group_Exter <-   lm(group_Exter, data = df_ames)
summary(lm_group_Exter)

par(mfrow = c(2, 2))
plot(lm_group_Exter)

# Analysis of Variance
aov(lm_group_Exter)

# Detect MultiCollinearity
attributes(alias(lm_group_Exter)$Complete)$dimnames[[1]]

# vif(lm_group_Exter)

# Model selection using AIC
AIC_lm_group_Exter <- stepAIC(lm_group_Exter,direction = "backward",k=2, trace = 0, steps = 1000)
summary(AIC_lm_group_Exter)

# We will chose Most Significant Variables from AIC_lm_group_Exter Model (ExterQual,MasVnrArea)


## -------------------------------- group_Others -------------------------------------------------------
group_Others    <-   as.formula(log(SalePrice) ~ MSSubClass+MSZoning+log(LotArea)+Neighborhood+HouseStyle)
pairs(group_Others,data = df_ames)

lm_group_Others <-  lm(group_Others, data = df_ames)
summary(lm_group_Others)

par(mfrow = c(2, 2))
plot(lm_group_Others)

# Analysis of Variance
aov(lm_group_Others)

# Detect MultiCollinearity
# alias(lm_group_Others)
# attributes(alias(lm_group_Others)$Complete)$dimnames[[1]]
vif(lm_group_Others)
# We will exclude Neighbourhood  and MSZoning as It possess high MultiCollinearity


# Model selection using AIC
AIC_lm_group_Others <- stepAIC(lm_group_Others,direction = "backward",k=2, trace = 0, steps = 1000)
summary(lm_group_Others)

# We will chose Most Significant Variables from AIC_lm_group_Exter Model (LotArea,MSZoning)


## ---------------------------------- group_OverAll -----------------------------
group_OverAll    <-   as.formula("log(SalePrice) ~ OverallQual+OverallCond+`1stFlrSF`+`2ndFlrSF`+TotalBsmtSF")    
pairs(group_OverAll, data = df_ames)

lm_group_OverAll <-  lm(group_OverAll, data = df_ames)
summary(lm_group_OverAll)

par(mfrow = c(2, 2))
plot(lm_group_OverAll)

# Analysis of Variance
aov(lm_group_OverAll)


# Detect MultiCollinearity
# alias(lm_group_OverAll)
# attributes(alias(lm_group_OverAll)$Complete)$dimnames[[1]]
vif(lm_group_OverAll)


# Model selection using AIC
AIC_lm_group_OverAll <- stepAIC(lm_group_OverAll,direction = "backward",k=2, trace = 0, steps = 1000)
summary(AIC_lm_group_OverAll)
# We will chose All Significant Variables from AIC_lm_group_OverAll (OverallQual,OverallCond,`1stFlrSF`,`2ndFlrSF`,TotalBsmtSF)

## ---------------------------------- group__Garage -------------------------------------------------------
group_Garage1  <-    as.formula(log(SalePrice) ~ BedroomAbvGr+KitchenAbvGr+PoolArea+GarageArea+GarageQual)
pairs(group_Garage1, data = df_ames)

lm_group_Garage1 <-  lm(group_Garage1, data = df_ames)
summary(lm_group_Garage1)

par(mfrow = c(2, 2))
plot(lm_group_Garage1)

# Analysis of Variance
aov(lm_group_Garage1)

# Detect MultiCollinearity
# alias(lm_group__Garage)
# attributes(alias(lm_group__Garage)$Complete)$dimnames[[1]]
vif(lm_group_Garage1)


# Model selection using AIC
AIC_lm_group_Garage <- stepAIC(lm_group_Garage1,direction = "backward",k=2, trace = 0, steps = 1000)
summary(AIC_lm_group_Garage)
# We will chose All Significant Variables from AIC_lm_group_Garage (BedroomAbvGr+KitchenAbvGr+GarageArea)



# ================================ Final Model ======================================================
group_final <-  as.formula("log(SalePrice) ~ BsmtQual+BsmtCond+ExterQual+MasVnrArea+log(LotArea)+MSZoning+OverallQual+OverallCond+`1stFlrSF`+`2ndFlrSF`+TotalBsmtSF+BedroomAbvGr+KitchenAbvGr+GarageArea")
final_model <- lm(group_final, data = df_ames)
summary(final_model)

par(mfrow = c(2, 2))
plot(final_model)

# Analysis of Variance
aov(final_model)

# Detect MultiCollinearity
# alias(final_model)
# attributes(alias(final_model)$Complete)$dimnames[[1]]
vif(final_model)


# Model selection using AIC
AIC_final_model <- stepAIC(final_model,direction = "backward",k=2, trace = 0, steps = 1000)
summary(AIC_final_model)
# We will chose All Significant Variables from AIC_final_model (GarageArea +`1stFlrSF`+`2ndFlrSF`+OverallQual+OverallCond+MSZoning+LotArea+ExterQual+BsmtCond+BsmtQual)


# ========================== Final Model after Performing AIC =======================================
final_model1 <- as.formula("log(SalePrice) ~ GarageArea +`1stFlrSF`+`2ndFlrSF`+OverallQual+OverallCond+MSZoning+log(LotArea)+BsmtCond+BsmtQual")
final_model_lm <- lm(final_model1, data = df_ames)
summary(final_model_lm)


#R-Squared is 85% which mean these 10 Variables id defining 85% variability of Sales Price.where Residual Standard Error is quite low as 0.1
# which is Satisfactory and F-statistics is also High enough.P-value is 0 which Indicate Our Alternative Model Accuracy. 

par(mfrow=c(2,2))
plot(final_model)

ols_plot_resid_fit(final_model)
ols_plot_resid_lev(final_model)
ols_plot_resid_stud(final_model)
ols_plot_resid_qq(final_model)
ols_plot_dffits(final_model)
ols_plot_cooksd_chart(final_model)
ols_plot_hadi(final_model)
ols_plot_added_variable(final_model)
ols_plot_comp_plus_resid(final_model)


# ======================= ANOVA for Comparing Two Models ===============================
# Ho = both model are equal there is no Significant difference
# H1 = initial_model is Significantly better than final_model Model.
ANOVA1 <- Anova(final_model_lm,full_model_lm)
summary(ANOVA1)

# =========================== Prediction of Sales Price with 95 Confidence ====================================================

#Now We will Predict Sales Price of Test Dataset
predict.full <- exp(predict(final_model_lm, df_test, interval = "prediction",level=0.95))
SalePrice <-predict.full[,1]

model_output <- cbind(df_test,SalePrice)
View(model_output)


