#Libraries 
library(dplyr)
library(tidyverse)
library(caret)
library(rpart)
library(ipred)
library(tree)
library(mlbench)
library(caret)
library(randomForest)
library(rpart)

#Assignment 6

#The data dictionary of the data-set:
# PatientID:  unique patient identifier

#Age: age in years

#Sex:  1 = male; 0 = female

#ChestPain: chest pain type

#RestBP:  resting blood pressure (in mm Hg on admission to the hospital)

#Chol: serum cholestoral in mg/dl

#Fbs: fasting blood sugar > 120 mg/dl (1 = true; 0 = false)

#RestECG: resting electrocardiographic results. 0: normal ; 
#1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV) ;  
#2: showing probable or definite left ventricular hypertrophy by Estes' criteria.

#MaxHR: maximum heart rate achieved

#ExAng: exercise induced angina (1 = yes; 0 = no)

#Oldpeak: ST depression induced by exercise relative to rest

#Slope: the slope of the peak exercise ST segment. 1: upsloping; 2: flat; 3: downsloping

#Ca: number of major vessels (0-3) colored by flourosopy

#Thal: Thallium stress test, 3 =normal; 6 = fixed (defect); 7 = reversable (defect)


#AHD (the predicted attribute): angiographic heart disease (0 : No, 1:Yes)
# Assuming 'heart_data' is your dataframe and 'AHD' is the column of interest


#Part - B

heart <- Heart
head(heart)
str(heart)
sum(is.na(heart))

# Drop rows with NA values
heart1 <- na.omit(heart)
sum(is.na(heart1))
heart1$PatientID <- NULL

#heart1$RestBP <- as.numeric(heart1$RestBP)

#Angiographic heart disease AHD : (0 = No, 1 = Yes)
heart1 <- heart1 %>%
  mutate(AHD = ifelse(AHD == "No",0,1))
str(heart1)
heart1$ChestPain <- factor(heart1$ChestPain,
                            levels = c("asymptomatic", "nonanginal", "nontypical", "typical"),
                            labels = c("0", "1", "2", "3"))
heart1$Thal <- factor(heart1$Thal,
                       levels = c("normal", "reversable", "fixed"),
                       labels = c("0", "1", "2"))



--------------------------------------------------------------------------------------------




# a) 1.	We would like to predict RestBP (resting blood pressure, 
#in mm Hg on admission to the hospital), using regression trees and related approaches, 
#treating the response RestBP (outcome) as a quantitative variable,
#and other variables are the predictors.
library(caret)

# Split the data into training and test set [70:30]
set.seed(150)

train_index <- heart1$RestBP %>%
  createDataPartition(p = 0.7, list = FALSE)

train  <- heart1[train_index, ]
test <- heart1[-train_index, ]



#) b

set.seed(150)
model <- train(
  RestBP ~., data = train, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
summary(model)
plot(model)



# Print the best tuning parameter cp that
# minimize the model RMSE
model$bestTune

# Plot the final tree model
par(xpd = NA) 
plot(model$finalModel)
text(model$finalModel, digits = 3)
# Decision rules in the model
model$finalModel

# Predictions on the test data
predictions <- model %>% predict(test)
head(predictions)
# Prediction error RMSE
RMSE(predictions, test$RestBP)
# Calculate MSE
testMSE <- mean((predictions - test$RestBP)^2)
print(testMSE)





#c)

# Pruning the tree
Cp1 <- model$bestTune$cp
pru_tree <- rpart(RestBP ~ ., data = train, cp = Cp1)
prediction <- pru_tree %>% predict(train)
head(prediction)
RMSE(prediction, train$RestBP)
testMSEpruned1<- mean((prediction - train$RestBP)^2)
print(testMSEpruned1)




#d)

data = train
train$PatientID <- NULL
test$PatientID <- NULL
modelBreast<-bagging(RestBP ~ Age+Sex+ChestPain+Chol+Fbs+RestECG+MaxHR+ExAng+Oldpeak+Slope+Ca+Thal+AHD, data=train, coob=TRUE)
summary(modelBreast)
predictions2 <- predict(modelBreast, newdata = test)
mse_bag <- mean((predictions - test$RestBP)^2)
print(mse_bag)



#e)

set.seed(2000)

train_zero <- caret::nearZeroVar(train, saveMetrics = TRUE)
table(train_zero$zeroVar)
train <- train[, train_zero$zeroVar == 'FALSE']
x <- as.matrix(train[, -4])
y <- train$RestBP
#table
set.seed(2000)
forest_fit <- randomForest::randomForest(x = x, y = y,ntree = 200)
forest_fit
#
which.min(forest_fit$err.rate[, 1]) 
forest_fit$err.rate[0]
randomForest::varImpPlot(forest_fit)
#MSE
text_x <- as.matrix(test[, -4])
text_y <- test$RestBP
# Predict : Trained model
predictions <- predict(forest_fit, newdata = text_x)
# Calculate MSE
mse <- mean((text_y - predictions)^2)
print(mse)


#PART 3 
#a
# Split the data into training and test set
set.seed(150)
train_samp <- heart1$AHD %>%
  createDataPartition(p = 0.7, list = FALSE)
train  <- heart1[train_samp, ]
test <- heart1[-train_samp, ]

#b
set.seed(150)
model1 <- rpart(AHD~., data = train, method = "class")
par(xpd=NA)
plot(model1)
text(model1, digits=3)
# Make predictions on test data
predicted.class <- model1 %>%
  predict(test, type="class")
# Accuracy of the model
mean(predicted.class == test$AHD)

#c
# Convert the outcome variable 'AHD' to a factor if it's not already
train$AHD <- factor(train$AHD)
test$AHD <- factor(test$AHD)
# Set the seed for reproducibility
set.seed(123)
# Fit the model using rpart for classification
model2 <- train(
  AHD ~., data = train, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(model2)
# best optimal level of Cp
model2$bestTune


#d
# Plotting  tree model
par(xpd=NA)
plot(model2$finalModel)
text(model2$finalModel, digits = 3)
# Decision rules in the model
model2$finalModel
# Make predictions on the test data
predicted.classes <- model2 %>% predict(test)
# Compute model accuracy rate on test data
mean(predicted.classes == test$AHD)

#f
library(randomForest)
trainZero <- caret::nearZeroVar(train, saveMetrics = TRUE)
table(trainZero$zeroVar)
train <- train[, trainZero$zeroVar == FALSE]
x <- as.matrix(train[, -14])
y<- as.factor(train$AHD)
set.seed(1999)
forest <- randomForest::randomForest(x=x, y=y, ntree=200)
forest
which.min(forest$err.rate[, 1]) 
forest$err.rate[71]
randomForest::varImpPlot(forest)
var_imp <- importance(forest)
var_imp_plot <- barplot(var_imp[, "MeanDecreaseGini"], names.arg = rownames(var_imp), las = 2, cex.names = 0.7, main = "Variable Importance")
# Rank the importance of variables
var_imp <- importance(forest)
print("Variable Importance for Random Forest Model:")
print(var_imp)
print("Gini Index for Predictors:")
print(importance(forest)[, "MeanDecreaseGini"])
# Using variable selection for model selection
best_model <- forest


#g
near_zero_var <- caret::nearZeroVar(test, saveMetrics = TRUE)
test <- test[, near_zero_var$zeroVar == FALSE]

# Convert test set for prediction
text_x <- as.matrix(test[, -14]) 
text_y <- as.factor(test$AHD)   
# Make predictions
predictions <- predict(fit_forest, newdata = text_x)
# Calculate accuracy
accuracy <- mean(predictions == text_y)
print(accuracy)
