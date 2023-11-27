#K-M fucntion libraries
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(dplyr)

install.packages("survminer")
library("survminer")
library("Rcpp")

#Rcox function libraries
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

#importing data
attach(BP)
names(BP)
head(BP)

#data visulaization
summary(BP)

#KM analysis for standard treatment == 0
#filter dataset
library(dplyr)
std_treat <- BP %>%
  select(ID, Treat,Time,Death,Gender,BMI,Triglycerides,Age,Smoking) %>%
  filter(BP$Treat == "0")

km1<-with(std_treat,Surv(Time,Death))
head(km,1000)

table(std_treat$Death)
km_fit1<-survfit(Surv(Time,Death)~0,data=std_treat)
summary(km_fit1,times = c(0.1,0.3,0.6,0.90*(1:20)))
autoplot(km_fit1)

#KM analysis for intensive treatment == 1
#filter dataset
library(dplyr)
int_treat <- BP %>%
  select(ID, Treat,Time,Death,Gender,BMI,Triglycerides,Age,Smoking) %>%
  filter(BP$Treat == "1")
km2<-with(BP,Surv(Time,Death))
head(km,1000)

table(int_treat$Death)
km_fit2<-survfit(Surv(Time,Death)~1,data=int_treat)
summary(km_fit2,times = c(0.1,0.3,0.6,0.90*(1:20)))
autoplot(km_fit2)

#overlay data
km_fit_comb<-survfit(Surv(Time,Death)~Treat,data=BP)
summary(km_fit_comb,times = c(0.1,0.3,0.6,0.90*(1:20)))
autoplot(km_fit_comb)


#univariate cox-regression
#Smoking
res.cox <- coxph(Surv(Time, Death) ~ Smoking, data = int_treat)
summary(res.cox)
km_fit3<-survfit(Surv(Time,Death)~Smoking,data=int_treat)
summary(km_fit3,times = c(0.1,0.3,0.6,0.90*(1:20)))
autoplot(km_fit3)

#Gender
res.cox <- coxph(Surv(Time, Death) ~ Gender, data = int_treat)
summary(res.cox)
km_fit3<-survfit(Surv(Time,Death)~Gender,data=int_treat)
summary(km_fit3,times = c(0.1,0.3,0.6,0.90*(1:20)))
autoplot(km_fit3)

#Gender
res.cox <- coxph(Surv(Time, Death) ~ Gender, data = int_treat)
summary(res.cox)
km_fit3<-survfit(Surv(Time,Death)~Gender,data=int_treat)
summary(km_fit3,times = c(0.1,0.3,0.6,0.90*(1:20)))
autoplot(km_fit3)



#multivariate cox-reg
mulres.cox <- coxph(Surv(Time, Death) ~ Age + Gender + Smoking + Triglycerides + BMI, data = std_treat) 
summary(mulres.cox)
km_fit3<-survfit(Surv(Time,Death)~Gender,data=int_treat)
summary(km_fit3,times = c(0.1,0.3,0.6,0.90*(1:20)))
autoplot(km_fit3)

#graphical rep
test.ph <-cox.zph(mulres.cox)
ggcoxzph (test.ph)
