#Data Dictionary of the dataset
#ID:  Subject Identifier
#ALIVE:  1--infant alive; 2—infant not alive;
#BF5EVER: Breastfeed--ever, 1=No, 2=Yes.
#BF5LNGTH: duration -- length of time in weeks
#PGWT_GN: Pregnant weight gain/lost
#MOM_CM:  Moms height – centimeters
#MOM_BMI:  MOM BODY MASS INDEX
#MOMCIG:   Number of Cigarettes Per Day
#CIG_PRIOR : No. of cigarettes smoked - prior to pregnant 
#MOM_LBKG : Moms weight -- before pregnant


library(dplyr)
library(tidyverse)
library(caret)

#Import Dataset
df <- PRAMSZZSept
length(df)
nrow(df)
table(df$ALIVE)
129522+1061 
129522/130583*100

#To replace all the NA values in df$ALIVE column
#df$ALIVE <- ifelse(is.na(df$ALIVE), 1, df$ALIVE)

#Near zero variables
df3 <-  nearZeroVar(df, saveMetrics=TRUE)
df3_nzv <- df3[df3$nzv == TRUE, ]

table(df$MOM_LBKG)

#Correlation between the variables
library(dplyr)
library(corrplot)
library(correlation)
library(Hmisc)
library(caret)

# Since df contains both numeric and non-numeric columns
numeric_df <- df %>% select_if(is.numeric)

# Calculate Pearson correlation
pearson_cor <- cor(numeric_df, use = "pairwise.complete.obs", method = "pearson")
pearson_cor
highCor1 <- findCorrelation(abs(pearson_cor), cutoff=0.95)
print(highCor1)
corrplot(descrCor, method = "square")



#Calculate Spearman correlation
descrCor2 <- cor(numeric_df, use = "pairwise.complete.obs", method = "spearman")
print(descrCor2)
highCor2 <- findCorrelation(abs(descrCor2), cutoff=0.95)
highCor2
corrplot(descrCor2, method = "square")



# Find % of missing value using complete.cases()
library(stats)
miss_val <- complete.cases(df)
miss_val
1-mean(miss_val)
percentage_missing <- 100 * (1 - mean(miss_val))
percentage_missing




# Using md.pattern function to identify which variables and what percentage of observations from each variable are missing
library(mice)
pattern <- md.pattern(df, plot = FALSE, rotate.names = TRUE)
pattern
missing_counts <- colSums(pattern)
total_observations <- nrow(df)
missing_percentage <- (missing_counts / total_observations) * 100
missing_percentage

missing_info <- data.frame(
  Variable = names(missing_counts),
  Missing_Count = missing_counts,
  Missing_Percentage = missing_percentage
)


# using plot.missing() function to get the visualization of missing values and to identify variables that may need attention. 
library(DataExplorer)
plot_missing(
  df,
  group = list(Good = 0.05, OK = 0.4, Bad = 0.8, Remove = 1),
  missing_only = FALSE,
  geom_label_args = list(),
  title = NULL,
  ggtheme = theme_gray(),
  theme_config = list(legend.position = c("bottom"))
)



#Using suitable method to conduct the imputation for handling missing values in the data set, and showing part of the imputed results. 
library(mice)
library(lattice)

df$ALIVE <- as.factor(df$ALIVE)
df$BF5EVER <- as.factor(df$BF5EVER)
summary(df)

#Simple imputation for numerical columns
df$BF5LNGTH[which(is.na(df$BF5LNGTH))] = mean(df$BF5LNGTH, na.rm = TRUE)
df$PGWT_GN[which(is.na(df$PGWT_GN))] = mean(df$PGWT_GN, na.rm = TRUE)
df$MOM_CM[which(is.na(df$MOM_CM))] = mean(df$MOM_CM, na.rm = TRUE)
df$MOM_BMI[which(is.na(df$MOM_BMI))] = mean(df$MOM_BMI, na.rm = TRUE)
df$MOMCIG[which(is.na(df$MOMCIG))] = mean(df$MOMCIG, na.rm = TRUE)
df$CIG_PRIOR[which(is.na(df$CIG_PRIOR))] = mean(df$CIG_PRIOR, na.rm = TRUE)
df$MOM_LBKG[which(is.na(df$MOM_LBKG))] = mean(df$MOM_LBKG, na.rm = TRUE)

#Mice impuation for categorical variables

data3 <- df
data3$ALIVE <- as.factor(data3$ALIVE)
data3$BF5EVER <- as.factor(data3$BF5EVER)

imp_1 <- mice(data3, m=5, method = c("","pmm","pmm","logreg", "pmm", "pmm", "logreg","pmm", "pmm" ,"pmm" ), maxit = 20)
head(imp_1)
custom_1 <- complete(imp_1, 1)
head(custom_1)

custom_2 <- complete(imp_1, 2)
head(custom_2)

custom_3 <- complete(imp_1, 3)
head(custom_3)

custom_4 <- complete(imp_1, 4)
head(custom_4)

custom_5 <- complete(imp_1, 5)
head(custom_5)

      