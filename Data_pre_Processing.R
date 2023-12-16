library(caret)
library(dplyr)
library(tidyverse)
library(stats)
library(corrplot)
library(mice)
library(VIM)
library(lattice)

parm2020 <- PARMSOct2020
parm2020 <- data.frame(parm2020)
summary(parm2020)
#Convert into data frame
parm2020 <-  data.frame(parm2020)

# Length, Data type, Names, components of the dataset
#Length
length(parm2020)
#Data Type 
str(parm2020)
#Names
names(parm2020)
#Components
nrow(parm2020)
ncol(parm2020)
head(parm2020)
dim(parm2020)

#ALIVE:  1--infant alive; 2—infant not alive;
#BF5EVER: Breastfeed--ever, 1=No, 2=Yes.
#BF5LNGTH: duration -- length of time in weeks
#PGWT_GN: Pregnant weight gain/lost
#MOM_CM:  Moms height – centimeters
#MOM_BMI:  MOM BODY MASS INDEX
#MOMCIG:   Number of Cigarettes Per Day
#CIG_PRIOR : No. of cigarettes smoked - prior to pregnant 
#MOM_LBKG : Moms weight -- before pregnant
#DDS: insurance 
#.A = NOT APPLICABLE
#.B = DK/BLANK
#.N = NOT RECORDED
#.U = UNKNOWN
#1 = NO
#2 = YES
#MH_PPDPR MH: depress since birth  .B = BLANK/DK
#.S = SKIP
#1 = ALWAYS
#2 = OFTEN/ALMOST ALWAYS
#3 = SOMETIMES
#4 = RARELY
#5 = NEVER

# Removing non-numeric information from data frame
parm2020$ID <- NULL
head(parm2020)

#Near Zero variance
nzv<- nearZeroVar(parm2020, saveMetrics = TRUE)
nzv$zeroVar ==TRUE
write.csv(nzv, "Near zero variance", row.names = TRUE, col.names = TRUE)

#Recode the table
# "alive": 1 and "not alive" : 0.
parm2020$ALIVE <- ifelse(parm2020$ALIVE == 1, 1, 0) 

# "Breastfed-YES" : 1 and "Breastfed-No" : 0.
parm2020$BF5EVER <- ifelse(parm2020$BF5EVER == 1, 0, 1) 

# DDD_INS 
parm2020$DDS_INS <- factor(parm2020$DDS_INS,
                         levels = c("1", "2", ".A", ".B", ".N", "U"),
                         labels = c("0", "1", "2", "3", "4", "5"))
# Changing the variable DDD_INS from form of factor to numeric
parm2020$DDS_INS <- as.numeric(as.character(parm2020$DDS_INS))

# MH_PPDPR variable
parm2020$MH_PPDPR <- factor(parm2020$MH_PPDPR,
                          levels = c("1", "2", "3", "4", "5", ".B", ".S"),
                          labels = c("0", "1", "2", "3", "4", "5", "6"))

# Changing the variable MH_PPDPR from form of factor to numeric
parm2020$MH_PPDPR <- as.numeric(as.character(parm2020$MH_PPDPR))
head(parm2020)
str(parm2020)



-----------------------------------------------------------------------------------------
# Remove non-numeric information from the data frame
parm2020$ID <- NULL

# Recode variables
parm2020$ALIVE <- ifelse(parm2020$ALIVE == 1, 1, 0)
parm2020$BF5EVER <- ifelse(parm2020$BF5EVER == 1, 0, 1)
parm2020$DDS_INS <- factor(parm2020$DDS_INS,
                           levels = c("1", "2", ".A", ".B", ".N", "U"),
                           labels = c("0", "1", "2", "3", "4", "5"))
parm2020$DDS_INS <- as.numeric(as.character(parm2020$DDS_INS))
parm2020$MH_PPDPR <- factor(parm2020$MH_PPDPR,
                            levels = c("1", "2", "3", "4", "5", ".B", ".S"),
                            labels = c("0", "1", "2", "3", "4", "5", "6"))
parm2020$MH_PPDPR <- as.numeric(as.character(parm2020$MH_PPDPR))

# Load the required library
library(caret)

# Estimate zero and nearZero variance
nzv <- nearZeroVar(parm2020, saveMetrics = TRUE)
print(nzv)

--------------------------------------------------------------------------------------
# Calculate Pearson correlation matrix
descrCor <- cor(parm2020, use = "pairwise.complete.obs", method = "pearson")
print(descrCor)

# Find pairs of variables with correlation greater than 0.95
highCorr <- findCorrelation(abs(descrCor), cutoff = 0.95)
print(highCorr)
# Plot
corrplot(descrCor, method = "square")

#Spearman correlation matrix
descrCor2 <- cor(parm2020, use = "pairwise.complete.obs", method = "spearman")
print(descrCor2)
# Find pairs of variables with Spearman correlation greater than 0.95
highCorr2 <- findCorrelation(abs(descrCor2), cutoff = 0.95)
print(highCorr2)
# Plot 
corrplot(descrCor2, method = "circle")




#Identifying variables that might need attention
#conducting the imputation for handling missing values 
#Showing part of imputed results

#Examine the missig-ness
# Examine missing values using md.pattern

md.pattern(PARMSOct2020)

library(VIM)
# Plot the missing values using aggr function
parm2020 <- aggr(PARMSOct2020, col = mdc(1:2), numbers = TRUE, sortVars = TRUE, labels = names(parm2020),
                 cex.axis = 0.7, gap = 3, ylab = c("Proportion of missing Variable", "Missing Pattern"))


# Calculate the number of missing values for each variable
library(dplyr)
PARMSOct2020 %>% 
  summarise_all(~sum(is.na(.)))
PARMSOct2020 %>% 
  summarise_all(~mean(is.na(.)) * 100)

#Data manipulation for imputation
PARMSOct2020$DDS_INS <- factor(PARMSOct2020$DDS_INS)
PARMSOct2020$MH_PPDPR <- factor(PARMSOct2020$MH_PPDPR)


PARMSOct2020$BF5EVER <- as.integer(PARMSOct2020$BF5EVER == "YES")
PARMSOct2020$ALIVE <- as.integer(PARMSOct2020$ALIVE == "ALIVE")

#setting custom imputation methods for specific variables
methods <- make.method(PARMSOct2020)
methods["ALIVE"] <- "logreg"
methods["BF5EVER"] <- "logreg"
methods["DDS_INS"] <- "polyreg"
methods["MH_PPDPR"] <- "polyreg"

methods

#Imputing
# Imputing missing values
imp_dat_custom1 <- mice(PARMSOct2020, method = methods, m = 5, maxit = 30)
head(imp_dat_custom1)

# Complete multiple imputations and save each dataset to a CSV file
imputed_data1 <- complete(imp_dat_custom1, 1)
head(imputed_data1)
write.csv(imputed_data1, file = "impdat1.csv")
--------------------------------------------------------------------------------------------------
  
  
#Part B
#We will try to predict MH_PPDPR (MH: depress since birth)
#using other variables
#(PGWT_GN: Pregnant weight gain/lost; 
#MOM_CM:  Moms height – centimeters;
#MOM_BMI: MOM BODY MASS INDEX; 
#MOMCIG:   Number of Cigarettes Per Day; 
#CIG_PRIOR : No. of cigarettes smoked - prior to pregnant; 
#and MOM_LBKG : Moms weight -- before pregnant); 
#i.e: MH_PPDPR (MH: depress since birth) is the response (outcome, or target), 
#and the predictors are PGWT_GN, MOM_CM, 
#MOM_BMI, MOMCIG , 
#CIG_PRIOR , 
#MOM_LBKG, and DDS_INS(DDS – insurance).  
#Note that you should depart the data into training set and testing set.


#1.	Propose a way to handle the outcome variable MH_PPDPR (MH: depress since birth);
# Mutate MH_PPDPR:  0(Always), 1(Often/ almost always) and 2(Sometimes) to 1
# Mutate MH_PPDPR: 4(Rarely), 5(Never), .B(Blank/DK) and .S(Skip) to 0

df <- parm2020 %>%
  mutate(MH_PPDPR = case_when(
    MH_PPDPR %in% 0:2 ~ 1,
    MH_PPDPR %in% 3:6 ~ 0,
    TRUE ~ MH_PPDPR 
  ))
head(df)

#Train and test (60:40) 
library(caret)
set.seed(120)
df_clean <- na.omit(df)
train_in <- createDataPartition(df_clean$MH_PPDPR, p = .6, list = FALSE, times = 1)
train <- df[train_in,]
test <- df[-train_in,]


#2 Conduct model selection procedure via suitable criteria
