#Assignment 5
#will try to predict BF5EVER (Breastfeed--ever, 1=No, 2=Yes), 
#using other variables (PGWT_GN: Pregnant weight gain/lost; 
#MOM_CM:  Moms height – centimeters; MOM_BMI: MOM BODY MASS INDEX;
#MOMCIG:   Number of Cigarettes Per Day; 
#CIG_PRIOR : No. of cigarettes smoked - prior to pregnant; and 
#MOM_LBKG : Moms weight -- before pregnant); 
#i.e: BF5EVER is the response (outcome), and the 
#predictors are PGWT_GN, MOM_CM, MOM_BMI, MOMCIG , CIG_PRIOR , and MOM_LBKG

library(mlbench)
library(dplyr)
library(e1071)
library(caret)

#Part- 2 
#Q1
#Import the imputed dataset from assignment 3
#Load the dataset
#predict BF5EVER (Breastfeed--ever, 1=No, 2=Yes)

df_imp_file<- imp_file %>%
  select(-...1)

# fit model
fit <- naiveBayes(BF5EVER~., data= df_imp_file)
# summarize the fit
print(fit)

# make predictions
predictions <- predict(fit,df_imp_file[,1:10])
# summarize accuracy
table(predictions, df_imp_file$BF5EVER)

#Q2
#a) Using “featurePlot“in “caret” to obtain Feature plots, Scatter Plot Matrix, and Box-Plots. 
data(df_imp_file)
names(df_imp_file)
set.seed(430)

#########Scatter Plot
#Step 1 : calculate the number of rows in the dataset 
df_obs = nrow(df_imp_file)

#Step 2 : perform a random sampling 
#randomly select a specified number of items from a sequence. In this case,
#select from the sequence of numbers from 1 to df_obs (the total number of rows in your dataset).
df_idx = sample(df_obs, size = trunc(0.50 * df_obs))
df_trn = df_imp_file[df_idx, ]
df_tst = df_imp_file[-df_idx, ]
library(caret)

featurePlot(x = df_imp_file[, c("PGWT_GN", "MOM_CM", 
                           "MOM_BMI", "MOMCIG", "CIG_PRIOR", "MOM_LBKG")], 
            y = df_imp_file$BF5EVER, 
            plot = "pairs", axis.text.cex=0.5,varname.cex=0.5,
            auto.key = list(columns = 2))

# Convert your binary numeric categorical variable to a factor
df_imp_file$BF5EVER <- as.factor(df_imp_file$BF5EVER)

#Feature plot
#PGWT_GN, MOM_CM, MOM_BMI, MOMCIG , CIG_PRIOR , and MOM_LBKG.
# Plot the density plot for PGWT_GN
featurePlot(x = df_imp_file[, "PGWT_GN", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "density",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Density Plot of PGWT_GN by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))
# Plot the density plot for MOMCIG
featurePlot(x = df_imp_file[, "MOMCIG", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "density",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Density Plot of MOMCIG by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))
# Plot the density plot for MOM_LBKG
featurePlot(x = df_imp_file[, "MOM_LBKG", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "density",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Density Plot of MOM_LBKG by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))

# Plot the density plot for MOM_CM
featurePlot(x = df_imp_file[, "MOM_CM", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "density",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Density Plot of MOM_CM by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))

# Plot the density plot for MOM_BMI
featurePlot(x = df_imp_file[, "MOM_BMI", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "density",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Density Plot of MOM_BMI by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))

# Plot the density plot for CIG_PRIOR
featurePlot(x = df_imp_file[, "CIG_PRIOR", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "density",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Density Plot of CIG_PRIOR by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))
#Box Plots
# Plot the box plot for PGWT_GN
featurePlot(x = df_imp_file[, "PGWT_GN", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Density Plot of PGWT_GN by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))
# Plot the box plot for MOMCIG
featurePlot(x = df_imp_file[, "MOMCIG", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Density Plot of MOMCIG by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))
# Plot the box plot for MOM_LBKG
featurePlot(x = df_imp_file[, "MOM_LBKG", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Box Plot of MOM_LBKG by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))

# Plot the Box plot for MOM_CM
featurePlot(x = df_imp_file[, "MOM_CM", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Box Plot of MOM_CM by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))

# Plot the Box plot for MOM_BMI
featurePlot(x = df_imp_file[, "MOM_BMI", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Box Plot of MOM_BMI by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))

# Plot the Box plot for CIG_PRIOR
featurePlot(x = df_imp_file[, "CIG_PRIOR", drop = FALSE], 
            y = df_imp_file$BF5EVER, 
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex = 0.7)),
            main = "Box Plot of CIG_PRIOR by BF5EVER",
            scales = list(x = list(relation = "free"), 
                          y = list(relation = "free")))

#2 b) Multiple Bayes Model

model <- naiveBayes(BF5EVER ~ PGWT_GN + MOM_CM + 
                      MOM_BMI + MOMCIG + CIG_PRIOR + 
                      MOM_LBKG, data = df_imp_file)
print(model)
summary(model)
pred <- predict(model,df_imp_file[,1:9])
# summarize accuracy
table(pred, df_imp_file$BF5EVER)









#3. Fit a linear discriminant model to predict the response using the six predictors. 
#PGWT_GN, MOM_CM,  MOM_BMI,   MOMCIG , CIG_PRIOR , and MOM_LBKG. 


library(MASS)
#Use the lda() function for linear discriminant model 

df_lda <- lda(BF5EVER~PGWT_GN+MOM_CM+MOM_BMI+MOMCIG+CIG_PRIOR+MOM_LBKG, data = df_imp_file)
df_lda

# Predicting posterior probabilities for each class
head(predict(df_lda, df_trn)$posterior, n=10)

# Storing predictions made on train and test data sets
df_lda_trn_pred = predict(df_lda, df_trn)$class
df_lda_tst_pred = predict(df_lda, df_tst)$class

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}

calc_class_err(predicted = df_lda_trn_pred, actual = df_trn$BF5EVER)
calc_class_err(predicted = df_lda_tst_pred, actual = df_tst$BF5EVER)

#Result : As expected, LDA performs well on both the train (0.1700483)  and test data (0.1690421).

# prediction table which can be used to calculate accuracy
table(predicted = df_lda_tst_pred, actual = df_tst$BF5EVER)

# Calculating accuracy
# Confusion matrix
conf_matrix <- matrix(c(928,14841, 1147, 77664), ncol=2)

# Extracting elements from the matrix
TN <- conf_matrix[1, 1]
FN <- conf_matrix[1, 2]
FP <- conf_matrix[2, 1]
TP <- conf_matrix[2, 2]

# Calculating accuracy
acc_lda<- (TP + TN) / (TP + TN + FP + FN)

# Printing accuracy
print(acc_lda)









#3.b. Quadratic discriminant model 

df_qda <- qda(BF5EVER~PGWT_GN+MOM_CM+MOM_BMI+MOMCIG+CIG_PRIOR+MOM_LBKG, data = df_imp_file)
df_qda


# Predicting posterior probabilities for each class
head(predict(df_qda, df_trn)$posterior, n=10)

# Storing predictions made on train and test data sets
df_qda_trn_pred = predict(df_qda, df_trn)$class
df_qda_tst_pred = predict(df_qda, df_tst)$class

calc_class_err = function(actual, predicted) {
  mean(actual != predicted)
}

calc_class_err(predicted = df_qda_trn_pred, actual = df_trn$BF5EVER)
calc_class_err(predicted = df_qda_tst_pred, actual = df_tst$BF5EVER)


# prediction table which can be used to calculate accuracy
table(predicted = df_qda_tst_pred, actual = df_tst$BF5EVER)


# Calculating accuracy
# Confusion matrix
conf_matrix_qda <- matrix(c(2958,12811, 4961 , 73850), ncol=2)

# Extracting elements from the matrix
TN <- conf_matrix[1, 1]
FN <- conf_matrix[1, 2]
FP <- conf_matrix[2, 1]
TP <- conf_matrix[2, 2]

# Calculating accuracy
acc_qda<- (TP + TN) / (TP + TN + FP + FN)

# Printing accuracy
print(acc_qda)









