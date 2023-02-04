# Paired T-test and Confidence Interval
# Difference in Mean for 2 Different Populations that are Paired or dependent on one another

BloodPressure <- read.table(file.choose(),header=T,sep="\t")
View(BloodPressure)

attach(BloodPressure)
names(BloodPressure)

# Get Shape of Dataset
dim(BloodPressure)
BloodPressure[1:3,]


help("t.test")
boxplot(Before,After)
plot(Before,After)

abline(a=0,b=1)

# Null Hypothesis is mean difference = 0
# Alternative Hypothesis is mean difference != 0

two_Test <- t.test(Before,After,mu=0,alt = "two.sided",conf.level= 0.95,paired=T)
two_Test

#Attributes of Test
attributes(two_Test)





