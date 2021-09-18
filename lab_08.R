#1. Import the data
adult <- read.table("adult.data", header=F, sep=",", stringsAsFactors = T);

#2. Remove the native country values
adult <- subset(adult, select = -c(V14))


#3. Assign Column Names
colnames(adult) <- c("age", "workclass", "fnlwgt", "education", "educationnum", 
                     "maritalstatus", "occupation", "relationship", 
                     "race", "Sex", "capitalgain", "capitalloss", 
                     "hoursperweek", "income")

#4. Check for outliers
head(adult)
summary(adult)

#5. Splitting the training data
set.seed(100)
trainIdx <- sample(nrow(adult), 20000)
train <- adult[trainIdx, ]
test <- adult[-trainIdx, ]

#6.Create a tree model
str(adult)
#install.packages("tree")
library(tree)
tree_model <- tree(income ~. , data = train)
summary(tree_model)
plot(tree_model)
text(tree_model, cex = 0.5)

#7.Cross Validation
cv_output <- cv.tree(tree_model, K = 5)
plot(cv_output) 

#8.Prune tree
tree_model2 <- prune.tree(tree_model, best = 6)
summary(tree_model2)

pred <- predict(tree_model2, test, type = "class")
table(test$income, pred)
error <- (473+1454)/nrow(test)
#error = 0.1534

#9. Bootstrap method
#install.packages("randomForest")
library(randomForest)
set.seed(100)
bagging_model <- randomForest(income ~ ., data = train, mtry = 13) #13 is all except income
bagging_pred <- predict(bagging_model, test)
table(test$income, bagging_pred)
error <- (674+1111)/nrow(test)
#error = 0.142

#10. Random Forest model
set.seed(100)
bagging_model2 <- randomForest(income ~ ., data = train, mtry = 4) #13 is all except income
bagging_pred2 <- predict(bagging_model2, test)
table(test$income, bagging_pred2)
error <- (666+1100)/nrow(test)
#error = 0.136

