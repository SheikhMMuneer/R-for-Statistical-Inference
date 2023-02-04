# Bar Chart and Pie Charts are usefult for Summarizing Categorical Data.

LungCapData <- read.table(file.choose(),sep="\t",header = T)
View(LungCapData)
attach(LungCapData)
dim(LungCapData)
names(LungCapData)
#Data Type of Gender
class(Gender)

?par # for parameters of plot

# =============== Barplot For Categorical Data ==================

# Frequency of each Category of Categorical Data
help("barplot")
?barplot

# Frequency Distribution of Gender Categorical Column
count <- table(Gender)
count
# Distribution in Percentage
percent <- table(Gender)/nrow(LungCapData)
percent


barplot(count)
barplot(percent,main="Gender Distribution",xlab = "Gender",ylab = "%",las=1)
barplot(percent,main="Gender Distribution",xlab = "Gender",ylab = "%",las=1,names.arg = c("Female","Male"))
barplot(percent,main="Gender Distribution",ylab = "Gender",xlab = "%",las=1,names.arg = c("Female","Male"),horiz = T)


# =============== PieCahrt For Categorical Data ==================
pie(count)
pie(count,main="Gender Distribution")
#It will add Box over Pie Chart
box()


# =============== BoxPlot For Summarizing Numerical Data ==================

class(LungCap)
class(Gender)

boxplot(LungCap,main="Boxplot",ylab="Lungs Capacity", ylim=c(0,16))
quantile(LungCap,probs = c(0,0.25,0.5,0.75,1))
boxplot(LungCap,main="Boxplot",ylab="Lungs Capacity", ylim=c(0,16),las=1) # las for rotate label values


# =============== Group BoxPlot For Summarizing Numerical Data on Category Group ==================

boxplot(LungCap ~ Gender)
# or we can do like this
boxplot(LungCap[Gender=="female"], LungCap[Gender=="male"],xlab= c("Female","Male"))


# =============== Stratified BoxPlot For  Numerical and Categorical Variable ======================
# Breaking Numeric Data into Categorical Variable for Examining Data

# create an AgeGroup Categorical Variables
# cut work like change numeric values into Categorical Variable
AgeGroup <- cut(Age,breaks = c(0,13,15,17,25),labels = c("<13","14-15","16-17","18+"))
AgeGroup

# It get Unique Values
levels(AgeGroup)

boxplot(LungCap,main="Lung Capacity",ylab="Lungs Capacity" ,las=1)
boxplot(LungCap ~ Smoke,main="Lung Capacity vs Smoker")

#LungCapacity for those who have 18+ 
boxplot(LungCap[Age>=18]~Smoke[Age>=18],main="Lung Capacity vs Smoker for 18+")


# ================== Stratis BoxPlot ==================
# 1 numeric vs two and more Categorical Variable Comparison
boxplot(LungCap ~ Smoke*AgeGroup,main="Lung Capacity vs Smoker by Age Group",col=c(4,2))


#=================== Histogram =======================
# to Summarize distribution of Numeric Variable
?hist
class(LungCap)

hist(LungCap)
hist(LungCap,freq = F)
hist(LungCap,prob = T)
hist(LungCap,prob = T,ylim=c(0,0.2),breaks = 7)
hist(LungCap,prob = T,ylim=c(0,0.2),breaks = 14)
hist(LungCap,prob = T,ylim=c(0,0.2),breaks = c(0,2,4,6,8,10,12,14,16))

# Line Over Histogram
hist(LungCap,prob = T,ylim=c(0,0.2),breaks = seq(from=0,to=16,by=2),xlab = "Lungs Capacity",ylab= "Proportion",main="Lings Capacity Distribution",las=1)
lines(density(LungCap),col=2,lwd=3)  # lwd for width of line


# ============== Stem and Leaf Plot Distributio of Numeric Variable  =======================
femleLungCap <- LungCap[Gender=="female"]
femleLungCap

?stem
stem(femleLungCap)
stem(femleLungCap,scale=2)



# ============== Stacked Bar Charts ==================================
# These are useful for relationship between Two Categorical Variables
class(Gender)
class(Smoke)

table1 <- table(Smoke,Gender)
table1

barplot(table1)
barplot(table1,beside = T,legend.text = T)
barplot(table1,beside = T,legend.text = c("Non Smoker","Smoker"),main="Gender Wise Smoking",xlab = "Gender",las=1)
barplot(table1,beside = T,legend.text = c("Non Smoker","Smoker"),main="Gender Wise Smoking",xlab = "Gender",las=1,col=c(2,4))



# ============== Mosaic Bar Charts ===============
# We can use Mosaic Plot for determining relation between 2 Categorical Variables
?mosaicplot

mosaicplot(table1)
mosaicplot(table1,col=c(4,2))



# ============== Scatter Plot for relation bt 2 Numeric Variables =================
class(Age)
class(Height)

# plot command is used for scatter plot
help(plot)
?plot

#Pearson Correlation is used to examine strnght between 2 nmeric variables
cor(Age,Height)
#We can say therei si 83% correlation between age and height variables
plot(Age,Height)

# Cex for size
plot(Age,Height,xlim=c(0,25),main="Scatterplot",cex=0.5,las=1,cex.lab=1.5,cex.main=2)

# Font Style
plot(Age,Height,main="Scatterplot",font.main=2,font.lab=2,font.axis=3)


# Color Style
plot(Age,Height,main="Scatterplot",col= 6,col.main=5,col.lab=2,col.axis=3)


#pch for plotting Character of Scatter Plot
plot(Age,Height,main="Scatterplot",pch=2)
plot(Age,Height,main="Scatterplot",pch="w")
plot(Age[Gender=="male"],Height[Gender=="male"],col=4,pch="m",xlab="Age",ylab="Height")
points(Age[Gender=="male"],Height[Gender=="male"],col=6,pch="f")


# use linear regression Predicting line by abline
plot(Age,Height,xlim=c(0,25),pch=8,las=1,col=2)  # pch 8 for *
abline(lm(Height~Age),col=4)

lines(smooth.spline(Age,Height),lty=2,lwd=5) # lty type of line and lwd for width


# =========================== Subplotting ==============================
par(mfrow=c(1,2))  # it will create 1 row and 2 columns
plot(Age[Gender=="male"],Height[Gender=="male"],col=4,pch="m",xlab="Age",ylab="Height",xlim=c(0,20),ylim=c(45,90),axes=F)
box()
plot(Age[Gender=="female"],Height[Gender=="female"],col=2,pch="f",xlab="Age",ylab="Height",xlim=c(0,20),ylim=c(45,90))


#====================== Setting Custom Axis=======================
# side 4 will place labels on right
plot(Age[Gender=="male"],Height[Gender=="male"],main="title",axes=F)
axis(side=1, at = c(7,12.5,15), labels= c("sev","mean","15"))
axis(side=2,at = c(55,65,75), labels= c(55,65,75))
axis(side=4,at = c(55,65,75), labels= c(55,65,75))


#==================== Text in Plots ====================
cor(Age,LungCap)
plot(Age,LungCap,las=1,main="Scatterplot")
# Add cusom text in Plot
text(x=6,y=11,labels = "corr=0.819")
# cex is for 0.5 percent font size
text(x=6,y=11,labels = "corr=0.819",adj=1,cex=1,col=4,font=5)



# ================= Mean and Median Line on Plot ===================

plot(Age,LungCap,las=1,main="Scatterplot")
text(x=6,y=11,labels = "corr=0.819",adj=1,cex=1,col=4,font=5)
abline(h=mean(LungCap),col=2,lwd=5)
text(x=3.5,y=8.5,labels = "Mean Lungs Caps",cex=0.65,col=2,font=4,adj=0)

#mtext for margin text and adf = 0,1, is used for left center and right
mtext(text="r= 0.81",side=1)
mtext(text="r= 0.81",side=2)
mtext(text="r= 0.81",side=3,adj=1,col=4,cex=1,font=4)


#=============== Customized Legends on Plots =========================
?legend

plot(Age[Smoke=="no"],LungCap[Smoke=="no"],las=1,main="Scatterplot",col=4)
points(Age[Smoke=="yes"],LungCap[Smoke=="yes"],las=1,col=2,pch=17)

#legend(x=3.5,y=14,legend = c("Smoker","Non-Smoker"),fill= c(4,2))
# for no box use bty = n
#legend(x=3.5,y=14,legend = c("Smoker","Non-Smoker"),col= c(4,2),pch = c(16,17),bty="n")

lines(smooth.spline(Age[Smoke=="no"],LungCap[Smoke=="no"]),lty=2,lwd=5,col=4) 
lines(smooth.spline(Age[Smoke=="yes"],LungCap[Smoke=="yes"]),lty=2,lwd=5,col=2) 
legend(x=3.5,y=14,legend = c("Smoker","Non-Smoker"),col= c(4,2), bty="n",lty= c(2,3))







