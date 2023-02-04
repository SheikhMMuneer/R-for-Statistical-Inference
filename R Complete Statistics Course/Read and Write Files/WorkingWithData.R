# Read LungsCap DataSet
LungCapData <- read.csv(file.choose(), sep="")
View(LungCapData)

# Get Total Row and Total Columns of data
dim(LungCapData)  

length(LungCapData)      # No of Columns
length(LungCapData$Age)  # No of Age Rows


#Get first 5 Rows of Dataset
head(LungCapData)


#Get last 5 Rows of Dataset
tail(LungCapData)

# we have 1 Outcome variable LungCapacity and 5 Explanatory Variables 
# Get Row no (5,6,7,8,9) and All columns
LungCapData[c(5,6,7,8,9),]
LungCapData[5:9,]

# Exclude All row between 4 to 722 and get othersRows and Columns
LungCapData[-(4:722),]


# Get Variables Names 
names(LungCapData)


# Get Mean
mean(LungCapData$Age)

# To Remember  in Memory
attach(LungCapData)

# Now it will take Age column Directly
mean(Age)

# To Forgot Varaible in Memory
detach(LungCapData)
Age

# To use Varable names without $ sign use attach
attach(LungCapData)


# Get DataTypes of Dataset Variables
# Factors are Categorical Variables

class(LungCapData$Age)
class(LungCapData$LungCap)
class(LungCapData$Gender)

# Get Categoriacl Distinct Values
levels(Smoke)
levels(Gender)


# Get Summary
summary(LungCapData)
# Categorical Variables are summarized using count of distinct variabes values


# Mapped integer Data into Categorical and Categorical into Number Data Type
x <- c(1,0,0,0,1,1,0,0,0,1,1,1)
class(x)
summary(x)

# Change numeric into categorical Variable
x_categorical <- as.factor(x)
class(x_categorical)
summary(x_categorical)

# ----------------- Subsets Filtering Record from DataSet -------------------
# Mean of Male Age
mean(Age[LungCapData$Gender=="male"])  
# Mean of Female Age
mean(Age[LungCapData$Gender=="female"])

femData   <- LungCapData[Gender=="female",]
MaleData  <- LungCapData[Gender=="male",]

dim(femData)
dim(MaleData)

summary(Gender)

femData[1:4,]

# Filter all Rows where age is greater than 15 and get All Columns
MaleOver15 <- LungCapData[(Gender == "male" & Age > 15),] 
dim(MaleOver15)


MaleOver15[1:4,]







