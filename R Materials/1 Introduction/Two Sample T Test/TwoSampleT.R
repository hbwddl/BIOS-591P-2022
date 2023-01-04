### Two Sample T Test in R

## Set working directory and read in our data
setwd("~/Work/BIOS 591P 2021/R Materials/1 Introduction/Two Sample T Test")
strokestudy <- read.csv("strokestudy.csv")

## Check our data
print(strokestudy)
View(strokestudy)

## View number of observations/variables (rows/columns)
dim(strokestudy)

## See variable names
names(strokestudy)

## Tabulate/count the number of patients of each gender
table(strokestudy$gender)

## Mean, variance, and median of age
mean(strokestudy$age)
var(strokestudy$age)
median(strokestudy$age)

## Create boxplot of age
boxplot(strokestudy$age)

## Create histogram of age
hist(strokestudy$age)

## Create boxplot of age, divided by gender
boxplot(strokestudy$age ~ strokestudy$gender)

## Create histogram of age, divided by gender (more complicated)
par(mfrow=c(1,2)) #Setting the graph area to include two graphs, in 1 row and 2 columns

strokemale <- strokestudy[which(strokestudy$gender==1),] #Copying the male patients to a new dataset
hist(strokemale$age) #histogram of male stroke patients age

strokefemale <- strokestudy[which(strokestudy$gender==2),] #Copying the female patients to a new dataset
hist(strokefemale$age) #histogram of female stroke patients age

par(mfrow=c(1,1)) #Setting the graph area to only include 1 plot

## Draw Q-Q plot of age for males and females
qqnorm(strokemale$age)
qqline(strokemale$age)

qqnorm(strokefemale$age)
qqline(strokefemale$age)

### Perform T test
## Unpooled T test
t.test(strokestudy$age ~ strokestudy$gender,var.equal=FALSE)

## Pooled T test
t.test(strokestudy$age ~ strokestudy$gender,var.equal=TRUE)

## F test for equality of variances
var.test(strokestudy$age ~ strokestudy$gender)

