### Recitation 1
## T Tests
## Research question: Is there a significant difference in the level of LDL
## cholesterol between the two arms (control vs. treated) in the stroke study?

# Set working directory
setwd("~/Work/BIOS 591P 2021/R Materials/1 Introduction/Two Sample T Test")

## Various ways to set working directory:
# "Files" pane in lower right of RStudio
# Session > Set Working Directory > Choose Directory

# Read in the data
strokestudy <- read.csv("strokestudy.csv")

## Check T Test Assumptions:
# Large sample OR both groups are normally distributed
table(strokestudy$arm)
boxplot(strokestudy$ldl ~ strokestudy$arm)

# Subsetting data to get Q-Q plot of each arm
qqnorm(strokestudy$ldl[strokestudy$arm==1]) # What is this piece of code doing? Explain lists/getting values from lists with []
qqline(strokestudy$ldl[strokestudy$arm==1])

qqnorm(strokestudy$ldl[strokestudy$arm==2])
qqline(strokestudy$ldl[strokestudy$arm==2])

# Pooled or unpooled test?
# Cannot use F test, data are approximately normal
# Conduct both tests, choose higher p-value to be conservative

## Perform T test

# Unpooled T test
t.test(strokestudy$ldl ~ strokestudy$arm)

# Pooled T test
t.test(strokestudy$ldl ~ strokestudy$arm,var.equal=TRUE)

## Chi-Square Test
## Testing whether two categorical variables are independent
## Example: We wish to investigate whether smoking (yes/no) is independent of education
## What does it mean if they are independent?

# The data: 43 smoke, did not graduate high school
#           73 smoke, graduated high school
#           50 do not smoke, did not graduate high school
#           190 do not smoke, graduated high school

# Create contingency table
smoke.hs <- matrix(data=c(43,73,50,190),nrow=2,byrow=T)

# Run chi-squared test
chisq.test(smoke.hs)

# Conclusion?

## SLR/correlation Example
# Built-in R dataset: Air quality data in New York, 1973
data(airquality)

# Research question: How are Ozone and Wind speed correlated?

# Explore the data
?airquality
head(airquality)
names(airquality)

plot(Ozone~Wind, data=airquality)

# Correlation between ozone and wind

cor(airquality$Ozone,airquality$Wind)
sum(is.na(airquality))

# Why do we have to use the argument use="complete.obs"?

cor(airquality$Ozone,airquality$Wind,use="complete.obs")

cor.test(x=airquality$Ozone,y=airquality$Wind)

fit1<-lm(Ozone~Wind, data=airquality)
summary(fit1)
