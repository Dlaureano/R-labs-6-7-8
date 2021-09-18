#1. imoport data
myData <- read.csv("house.csv", header = TRUE)

#2
head(myData)

#3
data <- myData[,-1]
head(data)

#4any missing data by counting total number of empty cells
is.na(data[,])

#5use summary() # condition and Grade
summary(data)

#6
attach(data)
plot(data$grade, data$condition,
     main = "Correlation",
     ylab = "condition", xlab = "grade")

attach(data)
plot(data$grade, data$price,
     main = "Correlation",
     ylab = "condition", xlab = "grade")

#7 For the first scatter plot I would not remove the outliers because it would not
#any diffeence however in the second 
#scatter plot I would remove the putliers from the left
#and right sides to have a better numbers
data$id[data$price>6e+06]

#8houses without banthrooms
data$id[data$bathrooms ==0]
#I would not remove them beacause some come in 
#decent condition with multiple floors
#which means renovation can be done

#9use pairs
pairs(data[c(3,6,15)])
