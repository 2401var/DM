#Naïve Bayes, Hold Out (Training set = 75% Test set = 25%):
library(datasets)
library(e1071)
library(caTools)
data(iris)
# load the caret package for data splitting and preprocessing
library(caret)
# split the data into training and test sets using holdout method
set.seed(123) # for reproducibility

#hold out method : 75%-25%
spi <- 0.75 # split ratio
split <- sample.split(iris$Species, SplitRatio = spi)
train <- subset(iris, split == TRUE)
test <- subset(iris, split == FALSE)
# train a Naive Bayes classifier on the training set
nb_model <- naiveBayes(Species ~ ., data = train)
# make predictions on the test set
nb_pred <- predict(nb_model, test)
# evaluate the performance of the Naive Bayes classifier
confusionMatrix(nb_pred, test$Species)

#hold out method : 66-33: 
# split the data into training and test sets using holdout method
set.seed(123) # for reproducibility
spi <- 0.66 # split ratio
split <- sample.split(iris$Species, SplitRatio = spi)
train <- subset(iris, split == TRUE)
test <- subset(iris, split == FALSE)
# train a Naive Bayes classifier on the training set
nb_model <- naiveBayes(Species ~ ., data = train)
# make predictions on the test set
nb_pred <- predict(nb_model, test)
confusionMatrix(nb_pred, test$Species)

#naiveB random subsampling: 75-25
# set the number of iterations and split ratio
set.seed(123) # for reproducibility
n_iter <- 5 # number of iterations for random subsampling
spi <- 0.75 # split ratio

# create an empty vector to store the accuracy of each iteration
accuracy <- numeric(n_iter)

# run random subsampling multiple times and evaluate the performance
for (i in 1:n_iter) {
  # split the data into training and test sets using random subsampling
  split <- sample.split(iris$Species, SplitRatio = spi)
  train <- subset(iris, split == TRUE)
  test <- subset(iris, split == FALSE)
  
  # train a Naive Bayes classifier on the training set
  nb_model <- naiveBayes(Species ~ ., data = train)
  
  # make predictions on the test set
  nb_pred <- predict(nb_model, newdata = test)
  
  # calculate the accuracy of the classifier
  accuracy[i] <- mean(nb_pred == test$Species)
}
# print the mean and standard deviation of the accuracy across all iterations
cat(sprintf("Mean accuracy: %.3f\n", mean(accuracy)))
cat(sprintf("Standard deviation: %.3f\n", sd(accuracy)))

#random subsample: 66-33: 
# set the number of iterations and split ratio
set.seed(123) # for reproducibility
n_iter <- 5 # number of iterations for random subsampling
spi <- 0.66 # split ratio

# create an empty vector to store the accuracy of each iteration
accuracy <- numeric(n_iter)

# run random subsampling multiple times and evaluate the performance
for (i in 1:n_iter) {
  # split the data into training and test sets using random subsampling
  split <- sample.split(iris$Species, SplitRatio = spi)
  train <- subset(iris, split == TRUE)
  test <- subset(iris, split == FALSE)
  
  # train a Naive Bayes classifier on the training set
  library(e1071)
  nb_model <- naiveBayes(Species ~ ., data = train)
  
  # make predictions on the test set
  nb_pred <- predict(nb_model, newdata = test)
  
  # calculate the accuracy of the classifier
  accuracy[i] <- mean(nb_pred == test$Species)
}

# print the mean and standard deviation of the accuracy across all iterations
cat(sprintf("Mean accuracy: %.3f\n", mean(accuracy)))
cat(sprintf("Standard deviation: %.3f\n", sd(accuracy)))



#cross validation: 
set.seed(123) # for reproducibility
spi <- 0.75 # split ratio
# split the data into training and test sets using holdout method
split <- sample.split(iris$Species, SplitRatio = spi)
train <- subset(iris, split == TRUE)
test <- subset(iris, split == FALSE)
# train a Naive Bayes classifier using 10-fold cross-validation
nb_model <- train(Species ~ ., data = train, method = "nb", trControl = trainControl(method = "cv", number = 10))
# make predictions on the test set
nb_pred <- predict(nb_model, newdata = test)
# evaluate the performance of the Naive Bayes classifier
confusionMatrix(nb_pred, test$Species)

#cv with 66% split ratio
set.seed(123) # for reproducibility
spi <- 0.66 # split ratio
# split the data into training and test sets using holdout method
split <- sample.split(iris$Species, SplitRatio = spi)
train <- subset(iris, split == TRUE)
test <- subset(iris, split == FALSE)
# train a Naive Bayes classifier using 10-fold cross-validation
nb_model <- train(Species ~ ., data = train, method = "nb", trControl = trainControl(method = "cv", number = 10))
# make predictions on the test set
nb_pred <- predict(nb_model, newdata = test)
# evaluate the performance of the Naive Bayes classifier
confusionMatrix(nb_pred, test$Species)


#K-means, Hold Out (Training set = 75% Test set = 25%):

# set the split ratio
set.seed(123) # for reproducibility
spi <- 0.75 # split ratio
# split the data into training and test sets using holdout method
split <- sample.split(iris$Species, SplitRatio = spi)
train <- subset(iris, split == TRUE)
test <- subset(iris, split == FALSE)

# train a KNN classifier on the training set
library(class)
k <- 5 # number of neighbors to consider
knn_model <- knn(train[,1:4], test[,1:4], train$Species, k)

# evaluate the performance of the KNN classifier
library(caret)
confusionMatrix(knn_model, test$Species)

#66-33 ratio holout knn: 
# set the split ratio
set.seed(123) # for reproducibility
spi <- 0.66 # split ratio
# split the data into training and test sets using holdout method
split <- sample.split(iris$Species, SplitRatio = spi)
train <- subset(iris, split == TRUE)
test <- subset(iris, split == FALSE)

# train a KNN classifier on the training set
library(class)
k <- 5 # number of neighbors to consider
knn_model <- knn(train[,1:4], test[,1:4], train$Species, k)

# evaluate the performance of the KNN classifier
library(caret)
confusionMatrix(knn_model, test$Species)

#KNN Random SS 75-25:
accuracy<-c(0,0,0)
# set the split ratio
set.seed(123) # for reproducibility
spi <- 0.75 # split ratio

# repeat the process 3 times
for (i in 1:3) {
  # split the data into training and test sets using random subsampling
  split <- createDataPartition(iris$Species, p = spi, list = FALSE)
  train <- iris[split, ]
  test <- iris[-split, ]
  
  # train a KNN classifier on the training set
  library(class)
  k <- 5 # number of neighbors to consider
  knn_model <- knn(train[,1:4], test[,1:4], train$Species, k)
  
  # evaluate the performance of the KNN classifier
  accuracy[i] <- mean(knn_model == test$Species)
}

print(mean(accuracy))


#KNN Random SS 66-33:
accuracy<-c(0,0,0)
# set the split ratio
set.seed(123) # for reproducibility
spi <- 0.66 # split ratio

# repeat the process 3 times
for (i in 1:3) {
  # split the data into training and test sets using random subsampling
  split <- createDataPartition(iris$Species, p = spi, list = FALSE)
  train <- iris[split, ]
  test <- iris[-split, ]
  
  # train a KNN classifier on the training set
  library(class)
  k <- 5 # number of neighbors to consider
  knn_model <- knn(train[,1:4], test[,1:4], train$Species, k)
  
  # evaluate the performance of the KNN classifier
  accuracy[i] <- mean(knn_model == test$Species)
}
print(mean(accuracy))

#cross-validation: 75-25
# set the split ratio
#set.seed(123) # for reproducibility
spi <- 0.75 # split ratio

# split the data into training and test sets
split <- sample.split(iris$Species, SplitRatio = spi)
train <- subset(iris, split == TRUE)
test <- subset(iris, split == FALSE)

# set up the training control for 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# train a KNN classifier on the training set using 10-fold cross-validation
library(class)
k <- 5 # number of neighbors to consider
knn_model <- train(Species ~ ., data = train, method = "knn", trControl = ctrl)

# evaluate the performance of the KNN classifier on the test set
predictions <- predict(knn_model, test)

acc<-mean(predictions==test$Species)
print(acc)

#cross-validation: 66-33:
# set the split ratio
#set.seed(123) # for reproducibility
spi <- 0.66 # split ratio

# split the data into training and test sets
split <- sample.split(iris$Species, SplitRatio = spi)
train <- subset(iris, split == TRUE)
test <- subset(iris, split == FALSE)

# set up the training control for 10-fold cross-validation
ctrl <- trainControl(method = "cv", number = 10)

# train a KNN classifier on the training set using 10-fold cross-validation
library(class)
k <- 5 # number of neighbors to consider
knn_model <- train(Species ~ ., data = train, method = "knn", trControl = ctrl)

# evaluate the performance of the KNN classifier on the test set
predictions <- predict(knn_model, test)

acc<-mean(predictions==test$Species)
print(acc)


#decision tree
#hold out: 75-25
# load the rpart package for decision tree modeling
library(rpart)

# set the split ratio
set.seed(123) # for reproducibility
spi <- 0.75 # split ratio

# split the data into training and test sets
split <- sample.split(iris$Species, SplitRatio = spi)
train <- subset(iris, split == TRUE)
test <- subset(iris, split == FALSE)

# fit a decision tree on the training set
tree_model <- rpart(Species ~ ., data = train, method = "class")

# make predictions on the test set
predictions <- predict(tree_model, newdata = test, type = "class")

# evaluate the performance of the decision tree on the test set
library(caret)
confusionMatrix(predictions, test$Species)

#hold out: 66-33
# load the rpart package for decision tree modeling
#library(rpart)

# set the split ratio
set.seed(123) # for reproducibility
spi <- 0.66 # split ratio

# split the data into training and test sets
split <- sample.split(iris$Species, SplitRatio = spi)
train <- subset(iris, split == TRUE)
test <- subset(iris, split == FALSE)

# fit a decision tree on the training set
tree_model <- rpart(Species ~ ., data = train, method = "class")

# make predictions on the test set
predictions <- predict(tree_model, newdata = test, type = "class")

# evaluate the performance of the decision tree on the test set
library(caret)
confusionMatrix(predictions, test$Species)

#Dtree Random ss 75-25:
# load the rpart package for decision tree modeling
library(rpart)

# set the number of iterations and split ratio
num_iter <- 3
spi <- 0.75
acc<-c(0,0,0)
# perform random sub-sampling and train decision tree model for each iteration
for (i in 1:num_iter) {
  # split the data into training and test sets
  split <- sample.split(iris$Species, SplitRatio = spi)
  train <- subset(iris, split == TRUE)
  test <- subset(iris, split == FALSE)
  
  # fit a decision tree on the training set
  tree_model <- rpart(Species ~ ., data = train, method = "class")
  
  # make predictions on the test set
  predictions <- predict(tree_model, newdata = test, type = "class")
  
  # evaluate the performance of the decision tree on the test set
  acc[i]<-mean(predictions==test$Species)
}
print(mean(acc))

#Dtree RSS 66:

# set the number of iterations and split ratio
num_iter <- 3
spi <- 0.66
acc<-c(0,0,0)
# perform random sub-sampling and train decision tree model for each iteration
for (i in 1:num_iter) {
  # split the data into training and test sets
  split <- sample.split(iris$Species, SplitRatio = spi)
  train <- subset(iris, split == TRUE)
  test <- subset(iris, split == FALSE)
  
  # fit a decision tree on the training set
  tree_model <- rpart(Species ~ ., data = train, method = "class")
  
  # make predictions on the test set
  predictions <- predict(tree_model, newdata = test, type = "class")
  
  # evaluate the performance of the decision tree on the test set
  acc[i]<-mean(predictions==test$Species)
}
print(mean(acc))

#DT Cross Validation 75-25:
# Set the seed for reproducibility
set.seed(123)

# Split the data into a training set and a test set
library(caTools)
split = sample.split(iris$Species, SplitRatio = 0.75)
train = subset(iris, split == TRUE)
test = subset(iris, split == FALSE)

# Use cross-validation to choose the training set
library(caret)
ctrl = trainControl(method = "cv", number = 10)
set.seed(123)
fit = train(Species ~ ., data = train, method = "rpart", trControl = ctrl)
print(fit)

# Use the fitted model to classify the test set
pred = predict(fit, newdata = test)
confusionMatrix(pred, test$Species)
# print the average accuracy over all folds
cat("Average accuracy: ", mean(accuracy))

#DT Cross Validation 66-33:
# Set the seed for reproducibility
set.seed(123)

# Split the data into a training set and a test set
library(caTools)
split = sample.split(iris$Species, SplitRatio = 0.66)
train = subset(iris, split == TRUE)
test = subset(iris, split == FALSE)

# Use cross-validation to choose the training set
library(caret)
ctrl = trainControl(method = "cv", number = 10)
set.seed(123)
fit = train(Species ~ ., data = train, method = "rpart", trControl = ctrl)
print(fit)

# Use the fitted model to classify the test set
pred = predict(fit, newdata = test)

confusionMatrix(pred, test$Species)
# print the average accuracy over all folds
cat("Average accuracy: ", mean(accuracy))

irisscaled<-scale(iris[,-5]) 
View(irisscaled)

  
