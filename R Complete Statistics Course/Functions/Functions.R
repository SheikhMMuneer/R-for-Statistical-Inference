# Get Current Working Directory
getwd()
setwd("E:/Certified Data Analyst/R Complete Statistics Course")

# Remove All  Variable caching in Workspace
rm(list=ls())

# Get All saved Variables
ls()



# Get All Packages
#install.packages("epiR")
# Use this epiR Functionality
library(epiR)
help(package = epiR)
remove.packages("epiR")


# -------------------------
temp <- Age>15
temp
# It will get 1 as True and 0 as False
temp1 <- as.numeric(Age>15)
temp1


# ================== Bind Additional Column in DataSet by cbind ==========================================
LungCapData <- read.delim("E:/Certified Data Analyst/R Complete Statistics Course/data/LungCapData.txt")
View(LungCapData)

attach(LungCapData)
# IsfemaleSmoker <- LungCapData[Gender=="female" & Smoke=='yes',] # it will get All columns which have female smokers
# IsfemaleSmoker <- Gender == "female" & Smoke == "yes"
IsfemaleSmoker <- as.numeric(Gender == "female" & Smoke == "yes")
IsfemaleSmoker


# Now Add this Column for Analysis with DataSet
FemaleSmokerAnalyticsData <- cbind(LungCapData,FemaleSmoker= IsfemaleSmoker)
FemaleSmokerAnalyticsData

rm(list=ls())

# =================== Apply Function =======================

# Apply function work in place of for-loop It apply function on every observation
?apply # Margin = 1 for row and 2 for column , fun for function apply  and ... for Additional Argument

# read in the "StockExample.csv" data, and attach it
StockData <- read.table(file="./data/StockExample.csv",sep=",", header=T, row.names=1)

# check the data
StockData

# calculate the mean price of each stock
avg_stocks <- apply(X=StockData, MARGIN=2, FUN=mean)
avg_stocks


# calculate the mean price of each stock, removing any NAs
avg_stocks <-apply(X=StockData, MARGIN=2, FUN=mean, na.rm=TRUE)

# Store Result of Stock Column in Last Row as Average Mean Stock Value
grand_Data <- rbind(StockData,"Average Stock " = avg_stocks)


# notice that we don't need to include "MARGIN", etc, as long
# as we enter info in the specified order
apply(StockData, 2, mean, na.rm=TRUE)


# ======================== Same Task using the ColMeans command ================================
?colMeans
colMeans(StockData, na.rm=TRUE)


# find the MAXIMUM stock price, for each stock
apply(X=StockData, MARGIN=2, FUN=max, na.rm=TRUE)

# find the 20th and 80th PERCENTILE, for each stock
quan_stocks <- apply(X=StockData, MARGIN=2, FUN=quantile, probs= c(0.2, .80),na.rm=TRUE)
grand_Data1 <- rbind(grand_Data,"Quantile " = quan_stocks)
grand_Data1


# create a plot of each column, using a "line"
apply(X=StockData, MARGIN=2, FUN=plot, type="l") # l mean line plot

# we can also send the plot function more arguments, such as titles, axes labels, and so forth...
apply(X=StockData, MARGIN=2, FUN=plot, type="l", main="Stock Price Summary", ylab="Price", xlab="Day")


# now let's calculate the SUM of each row (MARGIN=1)
Day1_Sum <- apply(X=StockData, MARGIN=1, FUN=sum, na.rm=TRUE)
cbind(StockData,OverAllStock = Day1_Sum)


# ================ Do Row Wise Sum with the rowSums command ================
rowSums(StockData, na.rm=TRUE)


# make a nice plot of these...
plot(apply(X=StockData, MARGIN=1, FUN=sum, na.rm=TRUE), type="l" ,ylab="Total Market Value", xlab="Day", main="Market Trend")


# and add in some nice coloured points...
points(apply(X=StockData, MARGIN=1, FUN=sum, na.rm=TRUE),pch=16, col="blue")

# ================= tapply ====================
# tapply used for filter specifc columns and Rows Data then apply function for Subsets

# read in the "LungCapData.csv" data, and attach it
LungCapData <- read.table(file.choose(), sep="\t", header=T)

# check the data
summary(LungCapData)
# and attach it
attach(LungCapData)


# get the help menu
?tapply

# calculate the mean Age for Smoker/NonSmoker
tapply(X=Age, INDEX=Smoke, FUN=mean, na.rm=T)

# you don't need to include "X", "INDEX",... as long as you  enter them in that order...
# we also don't need to include "na.rm=T" as no missing values
tapply(Age, Smoke, mean)


# we can save the output in a new "object"
m <- tapply(Age, Smoke, mean)
m

# also worth discussing is the use of the "SIMPLIFY" argument this is set to TRUE by default for formatting
tapply(Age, Smoke, mean, simplify=FALSE)


# note that we could get the same using [ ] although using "tapply" is more efficient
mean(Age[Smoke=="no"])
mean(Age[Smoke=="yes"])

# let's look at applying the "summary" function to groups
tapply(Age, Smoke, summary)


# or, applying the "quantile" function to the groups
tapply(Age, Smoke, quantile, probs=c(0.2, 0.8))


# we can "subset" based on multiple variables/vectors
# calculate the mean Age for Smoker/NonSmoker and male/female
tapply(X=Age, INDEX=list(Smoke, Gender), FUN=mean, na.rm=T)


# a less efficient way to get this done...
mean(Age[Smoke=="no" & Gender=="female"])
mean(Age[Smoke=="no" & Gender=="male"])
mean(Age[Smoke=="yes" & Gender=="female"])
mean(Age[Smoke=="yes" & Gender=="male"])

# a reminder of using 2 grouping variables
tapply(Age, list(Smoke, Gender), mean, na.rm=T)


# ============== Use by instead of tapply =====================
# Note that the "by" function is the same as tapply, except it presents the results similar to a vector
by(Age, list(Smoke, Gender), mean, na.rm=T)

# and we can subset the elements in the usual way
temp <- by(Age, list(Smoke, Gender), mean, na.rm=T)
temp
temp[4]


# and see the "class" of temp
class(temp)


# we can also convert it to a vector if we prefer
c(temp)
temp2 <- c(temp)
temp2

# and check it's class
class(temp2)














