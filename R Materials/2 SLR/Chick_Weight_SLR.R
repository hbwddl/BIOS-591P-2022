### SLR R Exercise

## Create Data
?seq
# The seq function creates a sequence between two numbers. The beginning number is the
# from = argument, the ending number is the to = argument 
# the by = argument tells the function how long each interval should be between the two numbers
# by = defaults to 1
seq(from=6,to=16)
seq(from=0,to=1,by=0.1)
    

## Create a Data Frame
chick_weight <- data.frame(age=c(6:16),
                           wgt=c(0.029,0.052,0.079,0.125,0.181,0.261,0.425,0.738,1.13,1.882,2.812))

chick_weight$age
chick_weight$wgt

## Initial Descriptive Statistics
mean(chick_weight$age)
sd(chick_weight$age)
median(chick_weight$age)
IQR(chick_weight$age)
range(chick_weight$age)
hist(chick_weight$age,main="Histogram of Age")

mean(chick_weight$wgt)
sd(chick_weight$wgt)
median(chick_weight$wgt)
IQR(chick_weight$wgt)
range(chick_weight$wgt)
hist(chick_weight$wgt,main="Histogram of Age")

plot(chick_weight$wgt~chick_weight$age,data=chick_weight)
plot(wgt~age,data=chick_weight)

# To calculate CI for correlation--cor.test
cor(chick_weight$age,chick_weight$wgt)
cor(chick_weight$age,chick_weight$wgt)^2

chick.r <- cor.test(chick_weight$age,chick_weight$wgt)
chick.r$conf.int

# Fit SLR model--LM function
# Uses the ~ notation for the model formula
# outcome (dependent) variable on the left of the ~
# Explanatory (independent) variable on the right of the ~
chick.lm <- lm(wgt ~ age,data=chick_weight)

# LM (linear model) object
str(chick.lm)

# Easy way to see summary
summary(chick.lm)

# Let's see the confidence intervals for estimated coefficients
confint(chick.lm)

# Predict weight at 11 days
# The newdata argument needs a data frame with the same column name as the independent variable
median.chick <- data.frame(age=11)
predict(chick.lm,newdata=median.chick,interval="confidence")

age.vals <- seq(from=6,to=16,by=0.01)
age.predict <- predict(chick.lm,newdata=data.frame(age=age.vals))
weight.ci <- predict(chick.lm,newdata=data.frame(age=age.vals),interval="confidence")
weight.pi <- predict(chick.lm,newdata=data.frame(age=age.vals),interval="predict")

#Plot data
plot(wgt~age,data=chick_weight,main="Weight vs. Age",
     xlab="Age (Days)",ylab="Chick Weight (lbs)",pch=19)

#Plot predicted values
lines(age.vals,age.predict)

# Plot confidence interval lines
lines(age.vals,weight.ci[,2],lty=2)
lines(age.vals,weight.ci[,3],lty=2)

# Plot prediction interval lines
lines(age.vals,weight.pi[,2],lty=2)
lines(age.vals,weight.pi[,3],lty=2)

# Let's fix the plot x and y axes
plot(wgt~age,data=chick_weight,main="Weight vs. Age",
     xlab="Age (Days)",ylab="Chick Weight (lbs)",pch=19,
     ylim=c(-2,3))

#Plot predicted values
lines(age.vals,age.predict,col="blue")

# Plot confidence interval lines
lines(age.vals,weight.ci[,2],lty=2)
lines(age.vals,weight.ci[,3],lty=2)

# Plot prediction interval lines
lines(age.vals,weight.pi[,2],lty=2)
lines(age.vals,weight.pi[,3],lty=2)
