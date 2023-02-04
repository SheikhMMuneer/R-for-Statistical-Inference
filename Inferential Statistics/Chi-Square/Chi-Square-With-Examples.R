# http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r

# Chi-Square Test of Independence in R
# The chi-square test of independence is used to analyze the frequency table (i.e. contengency table) formed by two categorical variables.
# The chi-square test evaluates whether there is a significant association between the categories of the two variables.

setwd("E:\\Certified Data Analyst\\Inferential Statistics\\Chi-Square\\")
# Data format: Contingency tables
file_path  <- "housetasks.txt"
housetasks <- read.delim(file_path, row.names = 1)
head(housetasks)

# The data is a contingency table containing 13 housetasks and their distribution in the couple:
# rows are the different tasks
# values are the frequencies of the tasks done :
# by the wife only
# alternatively
# by the husband only
# or jointly


# Graphical display of contengency tables
install.packages("gplots")
install.packages("corrplot")

library("gplots")
# 1. convert the data as a table
dt <- as.table(as.matrix(housetasks))
dt
# 2. Graph
balloonplot(t(dt), main ="housetasks", xlab ="", ylab="",label = FALSE, show.margins = FALSE)


library("graphics")
# The argument shade is used to color the graph
# The argument las = 2 produces vertical labels
mosaicplot(dt, shade = TRUE, las=2,main = "housetasks")

install.packages("vcd")
library("vcd")
# plot just a subset of the table
assoc(head(dt, 5), shade = TRUE, las=3)


# Chi-square test examines whether rows and columns of a contingency table are statistically significantly associated.
# Null hypothesis (H0): the row and the column variables of the contingency table are independent.
# Alternative hypothesis (H1): row and column variables are dependent


# For a given cell, the expected value is calculated as follow:
# e= row.sum???col.sum/grand.total

# ??2= ???(o???e)2 /e
# o is the observed value
# e is the expected value

chisq <- chisq.test(housetasks)
chisq

# The observed and the expected counts can be extracted from the result of the test as follow:
# Observed counts
chisq$observed

# Expected counts
round(chisq$expected,2)

# If you want to know the most contributing cells to the total Chi-square score,
# you just have to calculate the Chi-square statistic for each cell this is called Residuals

# r=o???e/???e

# Pearson residuals can be easily extracted from the output of the function chisq.test():
round(chisq$residuals, 3)

library(corrplot)
corrplot(chisq$residuals, is.cor = FALSE)

# Positive residuals are in blue. Positive values in cells specify an attraction (positive association) between the corresponding row and column variables.
# In the image above, it's evident that there are an association between the column Wife and the rows Laundry, Main_meal.
# There is a strong positive association between the column Husband and the row Repair
# Negative residuals are in red. This implies a repulsion (negative association) between the corresponding row and column variables.
# For example the column Wife are negatively associated (~ "not associated") with the row Repairs. There is a repulsion between the column Husband and, the rows Laundry and Main_meal

# The contribution (in %) of a given cell to the total Chi-square score is calculated as follow:
# contrib=r2/??2

# Contibution in percentage (%)
contrib <- 100*chisq$residuals^2/chisq$statistic
round(contrib, 3)

# Visualize the contribution
corrplot(contrib, is.cor = FALSE)

# It can be seen that:
# The column "Wife" is strongly associated with Laundry, Main_meal, Dinner
# The column "Husband" is strongly associated with the row Repairs
# The column jointly is frequently associated with the row Holidays


# From the image above, it can be seen that the most contributing cells to the Chi-square are Wife/Laundry (7.74%), Wife/Main_meal (4.98%), Husband/Repairs (21.9%), Jointly/Holidays (12.44%).
# These cells contribute about 47.06% to the total Chi-square score and thus account for most of the difference between expected and observed values.

# Access to the values returned by chisq.test() function
# The result of chisq.test() function is a list containing the following components:

# statistic: the value the chi-squared test statistic.
# parameter: the degrees of freedom
# p.value: the p-value of the test
# observed: the observed count
# expected: the expected count

# printing the p-value
chisq$p.value
# printing the mean
chisq$estimate

