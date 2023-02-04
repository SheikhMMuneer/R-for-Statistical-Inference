# One way Analysis of Variance and Kruskal Wallis One way Analysis of Variance
# ANOVA is used for Comparing Means for 2 or more Independent Populations 

DietWeight <- read.table(file.choose(),header=T,sep="\t")
View(DietWeight)

attach(DietWeight)
names(DietWeight)

class(WeightLoss)
class(Diet)

?aov

boxplot(WeightLoss ~ Diet)

# ANOVA ASSUMPTION FOR MEAN OF ALL Population is same
# Ho = Mean Weight Loss is same for all diets.No Significant Difference
# H1 = Mean Weight Loss is not same for all diets.Significant Difference

ANOVA1 <- aov(WeightLoss ~ Diet)
summary(ANOVA1)
# Here p-value = 0.1 so we reject Null hypothesis that there is no Significant Difference among mean of All diets.

attributes(ANOVA1)
ANOVA1$coefficients

# Which mean diet is different from others by TukeyHSD
TukeyHSD(ANOVA1)
plot(TukeyHSD(ANOVA1),las=1)

# ==========Kruskal-Wallis rank sum test WeightLoss for Different Diet ================
kruskal.test(WeightLoss ~ Diet)











