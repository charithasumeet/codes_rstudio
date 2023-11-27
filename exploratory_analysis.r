d <- Heart
summary(d)
sd(d$Age)
summary(d$Age)
mean(d$Age)
median(d$Age)
sd(d$Age)
var(d$Age)
mad(d$Age)
sum(d$Age)


summary(d$Chol)
mean(d$Chol)
median(d$Chol)
sd(d$Chol)
var(d$Chol)
mad(d$Chol)
sum(d$Chol)


summary(d$MaxHR)
mean(d$MaxHR)
median(d$MaxHR)
sd(d$MaxHR)
var(d$MaxHR)
mad(d$MaxHR)
sum(d$MaxHR)


summary(d$Oldpeak)
mean(d$Oldpeak)
median(d$Oldpeak)
sd(d$Oldpeak)
var(d$Oldpeak)
mad(d$Oldpeak)
sum(d$Oldpeak)

table(d$Sex)
table(d$ChestPain)
table(d$Fbs)
table(d$RestECG)
table(d$ExAng)
table(d$Slope)
table(d$Ca)
table(d$Thal)
table(d$AHD)


#Histogram or Barplot

hist(d$Age, main = "Histogram of Age", xlab = "Age")

d1<- data.frame(d$Sex)

sex <-table(d$Sex)
sex1 <- data.frame(sex)
unique(sex1$Var1)

barplot(sex1$Freq, names.arg = sex1$Var1, col = c("red", "blue"),
        xlab = "Sex", ylab="Frequency", main= "Sex Frequency Barplot")
