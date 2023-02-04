# Part 2 ### The fish-diet dataset ###
# Medical researchers followed 6272 Swedish men for 30 years to see whether there was any association between the amount of fish in their diet 
# and prostate cancer. 
# Conduct the appropriate tests and report your findings using the appropriate language.

getwd()
setwd("E:\\Certified Data Analyst\\R Assignment")

install.packages("gplots")
install.packages("corrplot")
install.packages("vcd")


# Load necessary libraries
library(dplyr)
library(tidyverse)
library("gplots")
library("graphics")
library("vcd")
library(corrplot)

#First load Dataset
read_fishdiet <- read.table(file.choose(),header = T ,sep="," ,row.names = 1,)
View(read_fishdiet)

# Get All columns
names(read_fishdiet)

# Get Data Types of Variables
class(read_fishdiet$fish_in_diet)
class(read_fishdiet$cancer)

# Since Amount of fish and prostate Cancer Both are Categorical Variables.
# We will use The chi-square test to evaluates whether there is a significant association between the categories of the two variables
# Null hypothesis is that the both Categorical variables  are independent or explanatory variables


#Get Distinct Values of both Categorical Variables
levels(c$fish_in_diet)
levels(read_fishdiet$cancer)

# Get Total Number of Rows in datasets
nrow(read_fishdiet)


# check if Prostate Cancer is independent or dependent on the type of fish-diet
# Create a contingency table/crosstab with the needed variables.

# Univariate Analysis
table(read_fishdiet$cancer)
table(read_fishdiet$fish_in_diet)


# Multivariate Analysis
summary(read_fishdiet)
tbl_relationship <- table(read_fishdiet$fish_in_diet,read_fishdiet$cancer)
tbl_relationship


# Percentage Wise Proportion of Variables Element Horizontally
round(prop.table(tbl_relationship,1),2) 

# Percentage Wise Proportion of Variables Element Horizontally
round(prop.table(tbl_relationship,2),2)


# 1. Convert the data as a table
dt <- as.matrix(tbl_relationship)
dt

# 2. BaloonPlot for Representing representation of every variable
balloonplot(t(dt), main ="read_fishdiet", xlab ="", ylab="",label = FALSE, show.margins = FALSE)

# 3. Visualization by  Mosaic Graph
mosaicplot(dt, shade = TRUE, las=2,main = "read_fishdiet")

# plot just a subset of the table
assoc(dt, shade = TRUE, las=3)

# Perform CHI-SQUARE TEST
chisq <- chisq.test(tbl_relationship)
chisq

# Observed Counts
chisq$observed

# Expected counts
round(chisq$expected,2)

# Residuals
round(chisq$residuals, 3)


# Contibution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

# Here 2 element contribute more where People Who never use fish in their diet have hgh percentage of Prostate Cancer(67%)
# and Who use Fish in their diet at moderate level have low chances of prostate cancer as 18.5% 
#.These two contribute more in these Case of Study.


# P-value  = 0.2985 so there is an association between both categorical variable so we reject null hypothesis as there is 
# a relationship probability of 29% among both dependent variable Fish diet and outcome variable Cancer.and Accept Alternate Hypothesis










