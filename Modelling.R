---
  title: "Mid Term Project"
author: "Charitha"
date: "2023-11-07"
output: pdf_document
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Question 1

```{r}
# Setting working directory

mydata <- PARMSOct2020
head(mydata)
# Removing non-numeric information from data frame
mydata$X <- NULL
head(mydata)
# Handling outcome variable MH_PPDPR

# Mutating the classes 0(Always), 1(Often/ almost always) and 2(Sometimes) to 1
# Mutating the classes 4(Rarely), 5(Never), .B(Blank/DK) and .S(Skip) to 0
library(dplyr)
mydata <- mydata %>%
  mutate(MH_PPDPR = case_when(
    MH_PPDPR %in% 0:2 ~ 1,
    MH_PPDPR %in% 3:6 ~ 0,
    TRUE ~ MH_PPDPR 
  ))
head(mydata)
```
When we observe different classes of our outcome categorical variable MH_PPDPR, we could convert this multiclass condition to binary by grouping different classes together. For example, you could group, ALWAYS, ALMOST ALWAYS AND FREQUENTLY to class YES that is defined by binary variable 1 and rest of the classes to binary variable 0. Here is the code to do that and I am also sharing the head(mydata) for visualizing the change of variables.

## Dividing data into train and test

I also divided my data into train and test (70:30) before proceeding to further questions

```{r}
# Dividing data into train and test data sets
library(caret)
set.seed(123)
trainIndex <- createDataPartition(mydata$MH_PPDPR, p = .7, list = FALSE, times = 1)
Train <- mydata[trainIndex,]
test <- mydata[-trainIndex,]
```

# Question 2

```{r}
library(MASS)
# Fit the model
model <- glm(MH_PPDPR ~ ., data = Train, family = binomial)
summary(model)
model <- stepAIC(model, trace = FALSE)

# Summarize the final selected model
summary(model)

# Predict probabilities on the test set
probabilities <- predict(model, newdata = test, type = "response")

# Convert probabilities to predicted classes
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Model accuracy
# Convert factors or character to numeric if necessary
actual.classes <- if(is.factor(test$MH_PPDPR)) as.numeric(as.character(test$MH_PPDPR)) else test$MH_PPDPR
accuracy <- mean(predicted.classes == actual.classes)

print(accuracy)
```

The model has an AIC of 283519 after model selction. It have AIC score of 283521 in the glm model. 
The accuracy was predicted to be 0.7116358.

Which mean that the model correctly predicted the outcome only 71% of the time. This might be due to the fact that the distribution of outcome variable is not clear in the data set. The blank data in the data frame which was denoted by .B and skip .S are considered as ‘no’ inorder to handle the outcome variable. But, in some instances, it might not be valid. So, one way around is to delete the observations with such values and carrying out analysis or carrying out modelling using only variables that are most related to the outcome.

# Question 3

## Linear Discriminant Analysis

```{r}
# Loading the required library to perform LDA
library(MASS)
# Linear discriminant analysis
LDA <- lda(MH_PPDPR~., data=Train)
print(LDA)
# Predicting posterior probabilities for each class
head(predict(LDA, Train)$posterior, n=10)

# Storing predictions made on train and test data sets
mydata_LDA_trn_pred = predict(LDA, Train)$class
mydata_LDA_tst_pred = predict(LDA, test)$class
# Calculating the error in prediction for both test and train data
calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}
calc_class_err(predicted = mydata_LDA_trn_pred, actual = Train$MH_PPDPR)
calc_class_err(predicted = mydata_LDA_tst_pred, actual = test$MH_PPDPR)
# prediction table which can be used to calculate accuracy
table(predicted = mydata_LDA_tst_pred, actual = test$MH_PPDPR)
```

Here I used LDA as a classification model to separate the two classes of our outcome variable MH_PPDPR based on linear combination of predictors. Further, it could also aid us to select the best model to predict the outcome variable.

### Calculating accuracy

```{r}
# Calculating accuracy
# Confusion matrix
conf_matrix <- matrix(c(71630, 29029, 412, 1036), ncol=2)
# Extracting elements from the matrix
TN <- conf_matrix[1, 1]
FN <- conf_matrix[1, 2]
FP <- conf_matrix[2, 1]
TP <- conf_matrix[2, 2]
# Calculating accuracy
accuracy_LDA <- (TP + TN) / (TP + TN + FP + FN)
# Printing accuracy
print(accuracy_LDA)
```

By creating a confusion matrix, I caluclated the accuracy of LDA, which is 0.7116652 and almost similar to logistic regression. Hence there is no gain in accuracy by using this model.

## Quadratic Discriminant Analysis

```{r}
# Quadratic discriminant analysis
QDA = qda(MH_PPDPR~., data=Train)
print(QDA)
# Storing predictions made on train and test data sets
mydata_QDA_trn_pred = predict(QDA, Train)$class
mydata_QDA_tst_pred = predict(QDA, test)$class
# Calculating the error in prediction for both test and train data
calc_class_err(predicted = mydata_QDA_trn_pred, actual = Train$MH_PPDPR)
calc_class_err(predicted = mydata_QDA_tst_pred, actual = test$MH_PPDPR)
# prediction table which can be used to calculate accuracy
table(predicted = mydata_QDA_tst_pred, actual = test$MH_PPDPR)
```

I further also tried using quadratic discriminant analysis 

### Calculating accuracy

```{r}
# Calculating accuracy
# Confusion matrix
conf_matrix <- matrix(c(68371, 26740, 3671, 3325), ncol=2)

# Extracting elements from the matrix
TN <- conf_matrix[1, 1]
FN <- conf_matrix[1, 2]
FP <- conf_matrix[2, 1]
TP <- conf_matrix[2, 2]

# Calculating accuracy
accuracy_QDA <- (TP + TN) / (TP + TN + FP + FN)

# Printing accuracy
print(accuracy_QDA)
```

The accuracy of QDA is lower when comapred to logistic regression and LDA. 

# Question 4

```{r}
#libraries needed
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)


head(mydata) ## see the structure
##Generate a random number that is 90% of the total number of rows in dataset.
ran <- sample(1:nrow(mydata), 0.7 * nrow(mydata)) 
##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }
##Run nomalization on columns of dataset because they are the predictors
mydata_norm <- as.data.frame(lapply(mydata[,c(1,2,3,4,5,6,7,8,9,10)], nor))
summary(mydata_norm)

##training set
mydata_train <- mydata_norm[ran,] 
##extract testing set
mydata_test <- mydata_norm[-ran,] 
##extract 11th column of train dataset because it will be used as 'cl' argument in knn function.
mydata_target_category <- mydata[ran,11]
##extract 11th column of test dataset to measure the accuracy
mydata_test_category <- mydata[-ran,11]

##run knn function
inst_learning <- knn(mydata_train,mydata_test,cl=mydata_target_category,k=10)
summary(inst_learning)
##create confusion matrix
tab <- table(inst_learning,mydata_test_category)
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)
```

Since, normalization has to be carried out for KNN, I have to create a new training and test data sets that are different from before. The train data set here is denoted by mydata_train and test as mydata_test. in same fashion of 70:30. 

Out of 102108 observations of mydata_test data, the KNN model, with k = 10 classified 95730 observations as 0 (no depression) ans 6378 observations as 1 (depressed)

The accuracy of model was further estimated to be 69 % which is lower than previous models. Only 69 instances out of 100 were correctly predicted by this model. Again, I think this issue might be due to improper classification of classes in the outcome variable in the given imput data.

# Question 5

```{r}
# library
library(glmnet)
# Prepare the training data
# predictors
x_train <- as.matrix(Train[, -which(names(Train) == "MH_PPDPR")]) 
# binary outcome
y_train <- Train$MH_PPDPR 
# Prepare the test data
# predictors
x_test <- as.matrix(test[, -which(names(test) == "MH_PPDPR")])
# binary outcome
y_test <- test$MH_PPDPR 
# Use cross-validation to find the optimal lambda for Lasso regularization
set.seed(123) 
cv_fit <- cv.glmnet(x_train, y_train, nfolds= 10,aplha =1, family = "binomial", type.measure = "auc")
# Plot the cross-validation curve
plot(cv_fit)
cv_fit$lambda.1se
# Fit the Lasso model using the optimal lambda found
lasso_model <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = cv_fit$lambda.1se)
# Predict on the test set
probabilities <- predict(lasso_model, newx = x_test,lambda = cv_fit$lambda.1se, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
# Evaluate the model's performance on the test set
confusion_matrix <- table(Predicted = predicted_classes, True = y_test)
# Print the confusion matrix
print(confusion_matrix)
# You may also want to calculate the accuracy, sensitivity, etc.
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
```

I performed regularization analysis using Lasso regression. The model is applied to previous partioned data frame i.e., 70% Train and 30% test data. 

Library 'glmnet' was used for this model.

Cross-validation using k = 10 was carried out in order to find optimal lambda for lasso regression

The optimal lambda used was 0.003978221.

Later predictions were carried out on the test data set and accuracy of the model is estimated to be 71% similar to logistic regression. 

# Question 6

```{r}
library(caret)
# Create a re-sampling control object for k-fold cross-validation
train.control <- trainControl(method = "cv", number = 10, classProbs = TRUE)
# Rename the levels of the outcome variable to valid variable names to apply nb model
mydata$MH_PPDPR <- make.names(as.character(mydata$MH_PPDPR))
# Training naive beyes model using cross-validation
model <- train(MH_PPDPR ~ .,data = mydata, method = "naive_bayes", trControl = train.control)
# Print the cross-validated results
print(model)
# performance metrics
summary(model)
```

I conducted resampling analysis using cross validation techniques by taking k as 10. 

The model is trained on all observations of 10 predictors using naive beyes moel, since our outcome variable is categorical. 

Coming to results, when usekernal argument is set to true i.e., when kernal density estimation method was used, the model accuracy was found to be around 70.5% almost similar to multiple logistic regression. 

Overall, I come to conclusion that if proper classification of outcome variabe was done, the accuracy if all models could be improved.



