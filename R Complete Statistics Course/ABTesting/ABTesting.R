# A/B testing has become the standard and popular way of optimizing user interface on Apps 
# and arrangement of shelving display on retail stores.

# A/B testing (sometimes called split testing) is comparing two versions user interface of an app design to see which one performs better. 
# You compare two Apps by showing the two variants (let’s call them A and B) to similar visitors at the same time. The one that gives a better conversion rate is a better version.

# We decide on two rival hypotheses – the Null Hypothesis and the Alternative Hypothesis. 
# The Null Hypothesis is the assumption that there is no difference between Version A and Version B, i.e. their mean conversion rates are equal. 
# The Alternative Hypothesis is the assumption that there is a difference between Version A and Version B.
# In most A/B tests it is usually enough to know the two values are different, i.e. we will use a Two-tailed alternative hypothesis. 
# A One-tailed alternative allows us to specify the direction of inequality.

# To reject the Null Hypothesis we need a p-value that is lower than the selected Significance Level(5%). i.e. if p < 0.05 for example, there is a difference between A and B versions.
# For example, to test A has 600 hits with 32 conversions, B has 400 with 54 conversions:

install.packages("pwr")
library(pwr)

prop.test(c(32, 44), c(600,400))
#Answer:The p-value is less than 0.05, so we can reject the hypothesis that conversion rates are equal & assume the second group has a higher rate


# For 2nd example, to test A has 400 hits with 32 conversions, B has 600 with 44 conversions:
prop.test(c(32,44),c(400,600))
#Answer:The p-value is greater than 0.05, so we need to accept the hypothesis that conversion rates are equal and both group has similar converstion rate.


# Is my sample size big enough?
# h – Effect size.(assume B need to be 20% different, 
# Use 0.2 for small, 0.5 for medium and 0.8 for large effects) n1 – Number of observations in the first sample (say, 1000) n2 – Number of observationsz in the second sample 
# (what is the minimum size do I need?) sig.level – Significance level (Type I error probability, typical 5%) power – Power of test (1 minus Type II error probability) alternative – a character string specifying the alternative hypothesis, must be one of “two.sided” (default), “greater” or “less”

pwr.2p2n.test(h=0.2, n1=1000, sig.level=0.05, power=0.8)
# Therefore, the sample size for B need to be at least 245


pwr.2p2n.test(h=0.5, n1=1000, sig.level=0.05, power=0.8)
