library(tidyverse)
library("MASS")
data("Cars93")

#To describe Column Specifications
str(Cars93)

View(Cars93)

# table command to see frequency of occurrence of each level of a categorical variable
table(Cars93$Type)

View(table(Cars93$Price)) # using this function for a numeric variable is useless
#View(prop.table(Cars93$Price))

# distribution of prices
ggplot(Cars93, aes(x = Price)) +  geom_density() + geom_histogram()

ggplot(Cars93, aes(x = Price)) +  geom_histogram()

# check if airbags are independent or dependent on the type of car
# Create a contingency table/crosstab with the needed variables.
table(Cars93$AirBags, Cars93$Type) 

# Perform the Chi-Square test.
chisq.test(table(Cars93$Type, Cars93$AirBags))
# chi-square test can be done of a contingency table, as well as the actual columns

# contingency table from a survey data, containing responses from couples about
# performing household duties. We can see if tasks are independent of gender

file_path <- "http://www.sthda.com/sthda/RDoc/data/housetasks.txt"

# read.delim(file_path, row.names = 1)

housetasks <- read.delim(file_path, row.names = 1)
head(housetasks)

chisq.test(housetasks)

# help("chisq.test")

# the chisquare object can be stored to get some more info. Refer to documentations
# and further readings on the chisquare test of independence to learn the following 
chisq <- chisq.test(housetasks)

# Observed counts
chisq$observed
# Expected counts
round(chisq$expected,2)
# residuals
round(chisq$residuals, 3)

# Contibution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

# effect of treatment on improvement. treated vs non-treated
df <- read.csv("https://goo.gl/j6lRXD")
head(df)

table(df$treatment, df$improvement)

# chisq.test(df$treatment, df$improvement, correct=FALSE)
chisq.test(df$treatment, df$improvement)

# cyl vs carb in mtcars
data("mtcars")
table(mtcars$carb, mtcars$cyl)
chisq.test(mtcars$carb, mtcars$cyl)


