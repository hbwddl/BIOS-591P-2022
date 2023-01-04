### Multiple Linear Regression
### Initial Report

## Install sas7bdat package
install.packages("sas7bdat")
library(sas7bdat)

## Set working directory
setwd("~/OneDrive - Emory University/Documents/Work/Bios 591P 2022/R Materials/3 MLR/Data")

## Read in the data
bwt <- read.sas7bdat("birthweight.sas7bdat")

View(bwt)

## Descriptive Statistics
## Continuous/Numeric: Mean, SD, Quantiles
mean(bwt$BWT)
sd(bwt$BWT)
quantile(bwt$BWT)

mean(bwt$AGE)
sd(bwt$AGE)
quantile(bwt$AGE)

mean(bwt$WT)
sd(bwt$WT)
quantile(bwt$WT)

### Discrete/binary:
table(bwt$SMOKE)
table(bwt$SMOKE)/nrow(bwt) * 100

table(bwt$HT)
table(bwt$HT)/nrow(bwt) * 100

## Correlation

cor(bwt)
cor(bwt[,c("BWT","AGE","WT")])
cor(bwt$AGE,bwt$BWT)

cor.test(bwt$AGE,bwt$BWT)
cor.test(bwt$WT,bwt$BWT)

pairs(bwt)
pairs(bwt[,c("BWT","AGE","WT")])

## Fit MLR and see results
bwt.lm <- lm(BWT ~ AGE + WT + SMOKE + HT,data=bwt)
summary(bwt.lm)

# We are not checking assumptions yet, this will be next week
# ## Check normality of residuals
# hist(residuals(bwt.lm),main="Histogram of Residuals",xlab="Residuals")
# 
# qqnorm(residuals(bwt.lm))
# qqline(residuals(bwt.lm))
