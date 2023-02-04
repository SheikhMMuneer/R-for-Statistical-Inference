# Binomial Distribution
# pbinorm and dbinorm for Binomial Random variable and dbinorm is used to find values for the probability density function X,f(x)
# qbinorm for Finding quantiles for a Binomial Distribution


library(openintro)
library(UsingR)
library(tidyverse)
library(statsr)


# ========================= dbinorm (Exactly) =============================================
# Each question in a quiz is multiple choice question with 4 Possible Answer.
# Therefor if a Student guess a question the chance they guess correctly is 1/4
# Each question is a Bernouli Trial.Either they got the Question correct or Incorrect
# The series of 20 Question make x a Bernouli distribution with n = 20 and p = 1/4


# How likely is it that student will get exactly 10 question correct? P(x=10)
dbinom(10,20,0.25)


# How likely is it that student will get 20 % of the  question correct? P(x<=3)
# For using 3 or less than 3 use pbinorm it get Sum of All probabilities like 0+1+2+3
pbinom(3,20,0.25)    # 0.225156 


# It will get Seperately every Possible Probability.
dbinom(0,20,0.25) +  dbinom(1,20,0.25) +  dbinom(2,20,0.25) +  dbinom(3,20,0.25) # 0.225156
dbinom(3,20,0.25) # 0.1338956


# P(x=3) #  Probability of exactly 3 Success, Size of Sample is 20 Trial and  probability of Success is 1/6 
dbinom(x=3,size=20,prob=1/6) # This mean Probability of exactly 3 success on 20 Trials is 23.8%


# P(x= 0 to 3) 
dbinom(x=(0:3),size=20,prob=1/6)

# P(x<3) 
sum(dbinom(x=(0:3),size=20,prob=1/6))
# Probaility of less than or equal to 3
pbinom(3,20,1/6)


# =================== Pbinorm (Less than equal to ,more than equal to) cumulative probability =======================
# pbinorm give us Accumulated values for Probability Distribution Function
# To find Probability of equal to 3 or less then 3.It will get Accumulated Probability of having 0 ,1,and 2 Collectively
pbinom(q=3,size=20,prob=1/6,lower.tail=T)
sum(dbinom(x=(0:3),size=20,prob=1/6))



# ============================ rbinorm =================================
# rbinom for Random Sample from Binomial Distriution
rbi <- rbinom(10,	20,	0.2)
rbi

#It will generate 30 random numbers from size of 10
rbin <- rbinom(30, size=10, prob=0.2)
rbin



# ============================= ppois =========================================================
#  dpois() for knowing rate of density and ppois() for knowing rate of Accumulated Probability
#  The Poisson distribution is the probability distribution of independent event occurrences in an interval
dpois(x = 4,lambda=7)  #9.1% chance of exactly 4 occurances.

# p(x<4) Probability of less than or equal to 4
sum(dpois(x = 0:4,lambda=7))  

# ppois for return Accumulated Probability for Interval or rate
# Accumulated Probability of 4 or less
ppois(q=4,lambda=7,lower.tail = T)

# Probability of 12 or more
ppois(q=12,lambda=7,lower.tail = F)


# Problem: If there are twelve cars crossing a bridge per minute on average, 
# Solution:The probability of having sixteen or less cars crossing the bridge in a particular minute is given by the function ppois.
ppois(16, lambda=12)   # lower tail is default


# find the probability of having 16 or more cars crossing the bridge in a particular minute.
ppois(16, lambda=12,lower.tail = F)   # Upper tail
# Answer:If there are twelve cars crossing a bridge per minute on average, the probability of having 16
# or more cars crossing the bridge in a particular minute is 10.1%.



# ==================== Probaility for Normal Random Distribution ===========================
# In Pnorm we give Observation to find expected percentage of that Observation

# Problem:Assume that the test scores of a college entrance exam fits a normal distribution. Furthermore, the mean test score is 72,
# and the standard deviation is 15.2. What is the percentage of students scoring 84 or more in the exam?

# Solution:We apply the function pnorm of the normal distribution with mean 72 and standard deviation 15.2. 
# Since we are looking for the percentage of students scoring higher than 84, we are interested in the upper tail of the normal distribution.
pnorm(84,mean=72,sd=15.2,lower.tail = F)
# The percentage of students scoring 84 or more in the college entrance exam is 21.5%.

#Student scoring less than or equal to 85
pnorm(q=70,mean=72,sd=15.2,lower.tail = T)

# ================================== qnorm ===========================

# In qnorm we give Percentage to get Actual Observation.
# Find Q1 get observation at 25th place from lower tail
qnorm(p=0.25,mean=75,sd=5,lower.tail = T)

# Find Q1 get observation at 25th place from Upper tail
qnorm(p=0.25,mean=75,sd=5,lower.tail = F)

# ============================== dnorm ===========================
x <- seq(from=55,to=95, by=0.25)
x

# Find Density Function which comes in 1 to 10 Almost
dens <- dnorm(x,mean=75,sd=5)
dens

plot(x,dens,type="l",main="Normal Dist")
abline(v=75)

#  =====================  Random Normal Distribution ==================
# Draw 40 Random Sample observation from Normal Distributed 
rand <- rnorm(n=40,mean=75,sd=5)
rand

hist(rand)
# abline(v=75)

# ===========================  T-Distribution =======================
# probability using t distribution

?pt
# t-stat= 2.3 df (degree of freedom) = 25 
# one sided p-value
# (P > 2.3)
pt(q=2.3,df=25,lower.tail = F)


# two sided p-value (greater than 2.3 and less than 2.3)
pt(q=2.3,df=25,lower.tail = F) + pt(q=-2.3,df=25,lower.tail = T)


# t-value for 95% Confidence Interval
# value of t with 2.5% in each tail
# In qt we give % and it return observation
qt(p=0.025,df=25,lower.tail = T)









