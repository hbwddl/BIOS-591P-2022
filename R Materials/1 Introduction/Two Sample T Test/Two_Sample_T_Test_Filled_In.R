### Two Sample T Test in R

## Set working directory
setwd("~/Work/BIOS 591P 2021/R Materials/1 Introduction/Two Sample T Test")

## Read in the data
strokestudy <- read.csv("strokestudy.csv")

## Check that data read in properly
View(strokestudy)
print(strokestudy)

### Check T Test assumptions

## Number of observations/variables (rows/columns)
dim(strokestudy)

## See variable names
names(strokestudy)

## Count the number of patients of each gender
table(strokestudy$gender)

## Mean, variance, and median of age
mean(strokestudy$age)
median(strokestudy$age)
var(strokestudy$age)
sd(strokestudy$age)

## Create boxplot of age
boxplot(strokestudy$age)

## Create histogram of age
hist(strokestudy$age)

## Create boxplot of age, divided by gender
boxplot(strokestudy$age ~ strokestudy$gender)

## Create histogram of age, divided by gender (more complicated)
strokemale <- strokestudy[strokestudy$gender==1,]
strokefemale <- strokestudy[strokestudy$gender==2,]

par(mfrow=c(1,2))
hist(strokemale$age)
hist(strokefemale$age)
par(mfrow=c(1,1))

## Draw Q-Q plot of age for males and females
qqnorm(strokemale$age)
qqline(strokemale$age)

qqnorm(strokefemale$age)
qqline(strokefemale$age)

### Perform T test

## Unpooled T test
t.test(strokestudy$age ~ strokestudy$gender)

## Pooled T test
t.test(strokestudy$age ~ strokestudy$gender,var.equal=TRUE)

## Conclusion: Fail to reject H0. 

## F test for equality of variances
var.test(strokestudy$age ~ strokestudy$gender)
