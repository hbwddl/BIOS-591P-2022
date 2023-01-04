## Necessary libraries
library(car)
library(multcomp)
library(DescTools)
library(epiDisplay)

## Set working directory
setwd("~/Work/BIOS 591P 2021/R Materials/4 Logistic Regression")

## Read in the data
dengue <- read.table("dengue.dat")

## Hm, the first row is the column names
dengue <- read.table("dengue.dat",header=TRUE)

## Looks good
## Dummy variables in R: FACTORS!
dengue$SECTOR_F <- as.factor(dengue$SECTOR)

# How to tell if it's a factor?
dengue$SECTOR_F

is.factor(dengue$SECTOR_F)

## Problem...How to set the reference level?
dengue.logistic <- glm(DENGUE ~ MOSNET + SECTOR_F + AGE,data=dengue,family=binomial)

## Error: Dengue is coded as 1 or 2, not 0 and 1. We'll have to fix it.
dengue$DENGUE_F <- as.factor(dengue$DENGUE)

## Let's try again
dengue.logistic <- glm(DENGUE_F ~ MOSNET + SECTOR_F + AGE,data=dengue,family=binomial)

summary(dengue.logistic)

## 2 problems: Sector 1 is the reference category, and it is modeling probability that dengue=2. This means Dengue=1 is the reference category.
## Solution: relevel()
## Make sure you save the releveled column into dengue data!
dengue$SECTOR_F_RE <- relevel(dengue$SECTOR_F,ref=5)

dengue$DENGUE_F_RE <- relevel(dengue$DENGUE_F,ref=2)

## Let's try again
dengue.logistic <- glm(DENGUE_F_RE ~ MOSNET + SECTOR_F_RE + AGE,data=dengue,family=binomial)

summary(dengue.logistic)

## This is correct

## Now to get the useful stuff:
## Likelihood ratio test
## You gotta fit the reduced model
dengue.reduced <- glm(DENGUE_F_RE ~ 1,data=dengue,family=binomial)

lrtest(dengue.reduced,dengue.logistic)

## Type III Wald
Anova(dengue.logistic,type="III",test="Wald")

## Odds ratios for coefficients
exp(coefficients(dengue.logistic))
exp(confint.default(dengue.logistic))

## Sector odds ratios
dengue.sector.test <- glht(dengue.logistic,mcp(SECTOR_F_RE="Tukey"))

sector.or.est <- exp(coefficients(dengue.sector.test))
sector.or.est
sector.or.ci <- exp(confint.default(dengue.sector.test))
sector.or.ci

cbind(sector.or.est,sector.or.ci)

## C Statistic
Cstat(dengue.logistic)