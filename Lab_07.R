#lab07 3309 Laureano_Daniel

myData <- read.csv("winequality-red.csv", sep = ';')
head(myData)


#set seed
set.seed(100)
trainIdx <- sample(nrow(myData),1000)
#split data 
train <- myData[-trainIdx, ]
test <- myData[-trainIdx, ]
#train and test labels
trainlabel <-myData[trainIdx, 12]
testlabel <-myData[-trainIdx,-12]

#apply Knn regrssion method using 3 nearest neighbors
install.packages("FNN")
library(FNN)

#apply KNN
Kpred <- knn.reg(test, train, trainlabel, k=3)
Kpred[1:5]
testlabel[1:5]


Kpred <- knn.reg(test, train, trainlabel, k=3)

mean((Kpred$pred-testlabel)^2)

#4
numFold <- 5 # change names
highestDeg <- 30
CV_MSE <- matrix( , nrow= numFold, ncol = highestDeg)

for(run in 1:numFold){
  oneSubsetIdx <- ((1+(run-1)*100):(run*100))
  oneSubset <- train[oneSubsetIdx, ]
  rest <- train[-oneSubsetIdx, ]
  
  for(deg in 1: highestDeg){
    m <- knn.reg(rest[, -12], oneSubset[,-12], rest[, 12], k =deg)
    MSE <- mean((m$pred - oneSubset[ ,12])^2)
    CV_MSE[run, deg] <- MSE
  }
}

colMeans(CV_MSE)

#5
plot( 1:30 , colMeans(CV_MSE), xlab = "Number of Neighbors", ylab = "Avg MSE")

#6
Kpred <- knn.reg(train[,-12], test[,-12], trainlabel, k =12)
MSE <- mean((Kpred$pred - test$quality)^2)
MSE

#7
model_1 <- lm(quality ~., data = train)
summary(model_1)

model_1 <- lm(quality ~., data = train)
summary(model_1)

m_pred <- predict(model_1, test)
MSE <- mean((m_pred - test$quality)^2)

#8
#I believe the linear model is the bst
#model out of all other options
#because you are able to refine the model by
#taking out insignificant variables
#and keeping the significant variables

