#Part-1
# 1 - Load the ToothGrowth data and perform some basic exploratory data analyses
# 2 - Provide a basic summary of the data.
# 3 - Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose.
# 4 - State your conclusions and the assumptions needed for your conclusions.

setwd("E:\\Certified Data Analyst\\R Assignment")

# Load necessary libraries
library(dplyr)
library(tidyverse)
library(openintro)
library(ggplot2)

# Load ToothGrowth Data
data(ToothGrowth)

# View ToothGrowth DataSet
View(ToothGrowth)

# View Table Columns and its Data Type
str(ToothGrowth)

# Number of Columns in Datasets
ncol(ToothGrowth)

# Number of Rows in Datasets
nrow(ToothGrowth)

# Get Unique Records of Supp and Dose
levels(ToothGrowth$supp)
unique(ToothGrowth$dose)

# 360 View By Exploratory Data Analysis on DataSet

# Analysis of toothGrowth by len
ggplot(ToothGrowth,aes(x = len)) + geom_density(binwidth =0.5,color='Brown') + facet_wrap(~ supp)
# By Viewing plot we can analyze that by using OJ Tooth grooth is larger than VC.


# Analyze Dose effect on len of tooth by Scatter Plot for Comparing Numeric Variables
ggplot(ToothGrowth, aes(x= dose, y= len)) + geom_point(aes(color=supp,alpha=0.2)) +  xlab(" Dose in Millis ") + ylab("Length of tooth")
# By Viewing plot we can analyze that by the more we increase dose the the greater the length of tooth will grow.
# On Dose wise VC have greter impact on tooth growth on taking same dose level as VC . but at dose level 2 Impact of both VC and OJ is almost same.


# Supplement Wise Analysis of Tooth length growth
ggplot(ToothGrowth,aes(supp,len)) +  geom_boxplot(aes(col='Red')) 
# By Viewing plot we can analyze OJ have greater impact on Tooth grooth as it have greater len mean than VC.


#Summary of len DataSet
summary(ToothGrowth$len)

# Summary of length of tooth  group by Supplement:
View(ToothGrowth %>%  group_by(supp) %>%  summarise(count = n(),min = min(len),mean = mean(len,na.rm=T) , sd = sd(len) ,variance = var(len), median = median(len), max = max(len)))


# Summary of length of tooth  group by Supplement and Dose:
View(ToothGrowth %>%  group_by(supp,dose) %>%  summarise(count = n(),min = min(len),mean = mean(len,na.rm=T) , sd = sd(len) ,variance = var(len), median = median(len), max = max(len)))


# First Compare Supplement(OJ,VC) at all dosage levels
# Null Hypothesis is mean of both OJ,VC are equals (difference = 0)
# Alternate Hypothesis is that  mean of both OJ,VC are not equals (difference != 0)
t.test(len ~ supp, data=ToothGrowth)
# Since p-value is 6% which is greater than our alpha value of 5% so we accept Null hypothesis that
# conclude that there is no Significant differene between Mean of OJ and VC at all dosage level 



# Compare  OJ and VC by 0.5 dosage level
# Null Hypothesis is mean of both OJ,VC by 0.5 dosage level are equals (difference = 0)
# Alternate Hypothesis is that  mean of both OJ,VC by 0.5 dosage level are not equals (difference != 0)
dose_05 <- ToothGrowth[ToothGrowth$dose==0.5,]
t.test(len ~ supp, data=dose_05)
# Since p-value is 0.6% which is less than our alpha value of 5% so we reject Null hypothesis that
# conclude that there is  Significant differene between Mean of OJ and VC by 0.5 dosage level  


# Compare OJ and VC by 1.0 dosage level
# Null Hypothesis is mean of both OJ,VC by 1.0 dosage level are equals (difference = 0)
# Alternate Hypothesis is that  mean of both OJ,VC by 1.0 dosage level are not equals (difference != 0)
dose_10 <- ToothGrowth[ToothGrowth$dose==1.0, ]
t.test(len ~ supp,  data=dose_10)
# Since p-value is 0.1% which is less than our alpha value of 5% so we reject Null hypothesis that
# conclude that there is Significant differene between Mean of OJ and VC by 1.0 dosage level  


 

# Compare  OJ and VC by 2.0 dosage level
# Null Hypothesis is mean of both OJ,VC by 2.0 dosage level are equals (difference = 0)
# Alternate Hypothesis is that  mean of both OJ,VC by 2.0 dosage level are not equals (difference != 0)
dose_20 <- ToothGrowth[ToothGrowth$dose==2.0, ]
t.test(len ~ supp,data=dose_20)
# Since p-value is 96% which is greater than our alpha value of 5% so we accept Null hypothesis that
# conclude that there is no Significant differene between Mean of OJ and VC by 2.0 dosage level .and both means are almost same.






