#Libraries needed
library(MASS)
library(glmnet)
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)
library(naivebayes)

#PART B

parms2020_2 <- imput1

# Question 1

head(parms2020_2)

# Removing non-numeric information from data frame
parms2020_2$X <- NULL
head(parms2020_2)

# Handling outcome variable MH_PPDPR

# Mutating the classes 0(Always), 1(Often/ almost always) and 2(Sometimes) to 1
# Mutating the classes 4(Rarely), 5(Never), .B(Blank/DK) and .S(Skip) to 0
library(dplyr)
parms2020_2 <- parms2020_2 %>%
  mutate(MH_PPDPR = case_when(
    MH_PPDPR %in% 0:2 ~ 1,
    MH_PPDPR %in% 3:6 ~ 0,
    TRUE ~ MH_PPDPR 
  ))
head(parms2020_2)


# Ensure all necessary libraries are loaded
library(dplyr)

# Applying mean (or median) imputation
parms2020_2 <- parms2020_2 %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Checking the first few rows of the dataset after imputation
head(parms2020_2)



# Dividing data into train and test data sets
library(caret)
set.seed(123)
trainIndex <- createDataPartition(parms2020_2$MH_PPDPR, p = .7, list = FALSE, times = 1)
train <- parms2020_2[trainIndex,]
test <- parms2020_2[-trainIndex,]








# Question 2

# Fit the model
model <- glm(MH_PPDPR ~ ., data = train, family = binomial)
summary(model)
model <- stepAIC(model, trace = FALSE)
summary(model)

# Predict probabilities on the test set
probabilities <- predict(model, newdata = test, type = "response")

# Convert probabilities to predicted classes
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)

# Model accuracy
# Convert factors or character to numeric if necessary
actual.classes <- if(is.factor(test$MH_PPDPR)) as.numeric(as.character(test$MH_PPDPR)) else test$MH_PPDPR
accuracy <- mean(predicted.classes == actual.classes)
accuracy


#Question 3 

#Linear Discriminant Analysis

# Check variances of each predictor within each class of MH_PPDPR
sapply(train[, -which(names(train) == "MH_PPDPR")], function(x) {
  by(x, train$MH_PPDPR, var)
})

# Example: Remove constant variables (adjust variable indices as necessary)
train_adjusted <- train[, -c(4, 7)]
test_adjusted <- test[, -c(4, 7)]


# Perform LDA on the adjusted dataset
library(MASS)
LDA <- lda(MH_PPDPR ~ ., data = train_adjusted)
LDA
# Predictions and error calculations can now proceed if LDA is successful
LDA_train_pred <- predict(LDA, train_adjusted)$class
LDA_test_pred <- predict(LDA, test_adjusted)$class



# Define the error calculation function
calc_class_err <- function(actual, predicted) {
  mean(actual != predicted)
}

# Calculate the error
# Assuming 'MH_PPDPR' is the response column in your train and test sets
train_error <- calc_class_err(train_adjusted$MH_PPDPR, LDA_train_pred)
test_error <- calc_class_err(test_adjusted$MH_PPDPR, LDA_test_pred)

# prediction table which can be used to calculate accuracy
table(predicted = LDA_test_pred, actual = test$MH_PPDPR)


### Calculating accuracy

# Calculating accuracy
# Confusion matrix
conf_matrix <- matrix(c(71558, 29762,  354 , 433), ncol=2)
# Extracting elements from the matrix
TN <- conf_matrix[1, 1]
FN <- conf_matrix[1, 2]
FP <- conf_matrix[2, 1]
TP <- conf_matrix[2, 2]
# Calculating accuracy
accuracy_LDA <- (TP + TN) / (TP + TN + FP + FN)
# Printing accuracy
print(accuracy_LDA)





# Quadratic discriminant analysis

# Calculating variance for each predictor within group 0
variances_group_0 <- sapply(train[train$MH_PPDPR == 0, -which(names(train) == "MH_PPDPR")], var)

# Identifying low variance predictors
low_variance_predictors <- names(variances_group_0[variances_group_0 < 0.5])  # Choose a reasonable threshold

# Update the training dataset
train_adjusted <- train[, !(names(train) %in% low_variance_predictors)]

QDA = qda(MH_PPDPR~., data=train_adjusted)
QDA

# Storing predictions made on train and test data sets
parms2020_2_QDA_trn_pred = predict(QDA, train)$class
parms2020_2_QDA_tst_pred = predict(QDA, test)$class

# Calculating the error in prediction for both test and train data
calc_class_err(predicted = parms2020_2_QDA_trn_pred, actual = train$MH_PPDPR)
calc_class_err(predicted = parms2020_2_QDA_tst_pred, actual = test$MH_PPDPR)

# prediction table which can be used to calculate accuracy
table(predicted = parms2020_2_QDA_tst_pred, actual = test$MH_PPDPR)


# Calculating accuracy
# Confusion matrix
conf_matrix <- matrix(c(66614, 26392, 5298, 3803), ncol=2)

# Extracting elements from the matrix
TN <- conf_matrix[1, 1]
FN <- conf_matrix[1, 2]
FP <- conf_matrix[2, 1]
TP <- conf_matrix[2, 2]

# Calculating accuracy
accuracy_QDA <- (TP + TN) / (TP + TN + FP + FN)

# Printing accuracy
print(accuracy_QDA)




#Question 4

#libraries needed
library(caret)
library(class)
library(dplyr)
library(e1071)
library(FNN) 
library(gmodels) 
library(psych)

head(parms2020_2) 
# Generate a random number that is 70% of the total number of rows in dataset.
random <- sample(1:nrow(parms2020_2), 0.7 * nrow(parms2020_2)) 

# The normalization function is created
nor <- function(x) { (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) }

# Run normalization on columns of dataset because they are the predictors
parms2020_2_norm <- as.data.frame(lapply(parms2020_2[,c(1,2,3,4,5,6,7,8,9,10)], nor))
summary(parms2020_2_norm)

# Training set
parms2020_2_train <- parms2020_2_norm[random,] 
# Testing set
parms2020_2_test <- parms2020_2_norm[-random,] 
# Target category for training set
parms2020_2_target_category <- parms2020_2[random,11]
# Target category for testing set
parms2020_2_test_category <- parms2020_2[-random,11]

# Running KNN only if lengths match
if (nrow(parms2020_2_train) == nrow(parms2020_2_target_category)) {
  inst_learning <- knn(train = parms2020_2_train, test = parms2020_2_test, cl = parms2020_2_target_category, k = 10)
  summary(inst_learning)
  
  # Create confusion matrix
  tab <- table(inst_learning, parms2020_2_test_category)
  # Function to calculate accuracy
  accuracy <- function(x) { sum(diag(x) / sum(rowSums(x))) * 100 }
  # Calculate accuracy
  print(accuracy(tab))
} else {
  print("Length mismatch between training data and target labels")
}





#Question 5
# Question 5

# library
library(glmnet)

# Prepare the training data
# predictors
x_train <- as.matrix(train[, -which(names(train) == "MH_PPDPR")]) 
# binary outcome
y_train <- train$MH_PPDPR 
# Prepare the test data
# predictors
x_test <- as.matrix(test[, -which(names(test) == "MH_PPDPR")])
# binary outcome
y_test <- test$MH_PPDPR 

# Use cross-validation to find the optimal lambda for Lasso regularization
set.seed(123) 
fit <- cv.glmnet(x_train, y_train, nfolds= 10,aplha =1, family = "binomial", type.measure = "auc")
# Plot the cross-validation curve
plot(fit)
fit$lambda.1se


# Fit the Lasso model using the optimal lambda found
lass_model <- glmnet(x_train, y_train, 
                     family = "binomial", 
                     alpha = 1, lambda = fit$lambda.1se)

# Predict on the test set
probabilities <- predict(lass_model, newx = x_test,lambda = fit$lambda.1se, type = "response")
predicted_classes <- ifelse(probabilities > 0.5, 1, 0)
# Evaluate the model's performance on the test set
confusion_matrix <- table(Predicted = predicted_classes, True = y_test)
# Print the confusion matrix
print(confusion_matrix)
# accuracy, sensitivity
acc <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("acc:", acc))




#Question 6 


library(caret)
# Create re-sampling control object for k-fold cross-validation
train_cont <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# Rename the levels of the outcome variable to valid variable names to apply nb model
parms2020_2$MH_PPDPR <- make.names(as.character(parms2020_2$MH_PPDPR))

# Training naive beyes model using cross-validation
model <- train(MH_PPDPR ~ .,
               data = parms2020_2,
               method = "naive_bayes", 
               trControl = train_cont)

model
summary(model)

