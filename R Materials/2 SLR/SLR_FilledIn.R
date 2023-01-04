### SLR R Exercise

# Create Data
chick_weight <- data.frame("age"=seq(from=6,to=16),
                           "wgt"=c(0.029,0.052,0.079,0.125,0.181,0.261,0.425,0.738,1.13,1.882,2.812))

# Initial Descriptive Statistics
mean(chick_weight$age)
median(chick_weight$age)
sd(chick_weight$age)
var(chick_weight$age)
IQR(chick_weight$age)
range(chick_weight$age)

mean(chick_weight$wgt)
median(chick_weight$wgt)
sd(chick_weight$wgt)
var(chick_weight$wgt)
IQR(chick_weight$wgt)
range(chick_weight$wgt)

hist(chick_weight$age)
hist(chick_weight$wgt)

# Calculate correlation, CI
cor(chick_weight$age,chick_weight$wgt)
cor(chick_weight$age,chick_weight$wgt)^2

cor.test(chick_weight$age,chick_weight$wgt)

# Fit SLR model
chick.lm <- lm(wgt ~ age,data=chick_weight)

# Summarize results
summary(chick.lm)

# ANOVA table
anova(chick.lm)

# confidence intervals for estimated coefficients
confint(chick.lm)

# Predict weight at 11 days
median.chick <- data.frame("age"=11)
predict(chick.lm,newdata=median.chick)

predict(chick.lm,newdata=median.chick,interval="confidence")
predict(chick.lm,newdata=median.chick,interval="prediction")

# Plot data
plot(chick_weight$age,chick_weight$wgt,ylim=c(-2,3))

# Plot predicted values
age.values <- data.frame("age"=seq(from=6,to=16))
wgt.predict <- predict(chick.lm,newdata=age.values)
wgt.ci <- as.data.frame(predict(chick.lm,newdata=age.values,interval="confidence"))
wgt.pi <- as.data.frame(predict(chick.lm,newdata=age.values,interval="prediction"))

# Plot model predictions
lines(age.values$age,wgt.predict)

# Plot confidence interval lines
lines(age.values$age,wgt.ci$lwr)
lines(age.values$age,wgt.ci$upr)

# Plot prediction interval lines
lines(age.values$age,wgt.pi$lwr)
lines(age.values$age,wgt.pi$upr)

# Some ways to clean up the plot
plot(chick_weight$age,chick_weight$wgt,ylim=c(-2,3),xlab="Age (Days)",ylab = "Weight (lbs)",main="Weight vs. Age, chicks",pch=19)

lines(age.values$age,wgt.predict,col="blue",lwd=2)

lines(age.values$age,wgt.ci$lwr,lty=2)
lines(age.values$age,wgt.ci$upr,lty=2)

lines(age.values$age,wgt.pi$lwr,lty=3)
lines(age.values$age,wgt.pi$upr,lty=3)
