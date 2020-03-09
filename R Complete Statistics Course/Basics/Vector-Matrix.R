# Make Integer Vector  like Integer List
x1 <- c(1,3,5,7,9)
y1 = 1:5


# Make String Vector
gender <- c("Male","Female")
gender


# Make Sequence of Values using :
print(2:7)

# Using Sequence
seq(from=1,to=10,by=1)
seq(from=1,to=10,by=1/3)
seq(from=1,to=10,by=0.5)

#Repeating Something
rep(1,times=10)
rep("Muneer Sheikh",times=10)
rep(1:3,times=5)
rep(seq(from=2,to=5,by=0.5),times=5)
rep(c("m","f"),times=5)


# Operation on Vector
x1 + 10
x1-y1
y1[3]
y1[1:3]
# Get these elements from vector
y1[c(1,5)]
# Exclude these elements from vector
y1[-c(1,5)]
#Get All valuesfrom y1 which is less than 4
y1[y1<4]


#==================== Matrix =========================
matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,ncol = 3,byrow = FALSE)
mat <- matrix(c(1,2,3,4,5,6,7,8,9),nrow = 3,ncol = 3,byrow =  TRUE)
mat
mat[1,2] # Get element from Row 1 and Column 2
mat[c(1,3),2] # Get element from Row 1and 3  Column 2
mat[2,]
mat[,1]
mat * 10







