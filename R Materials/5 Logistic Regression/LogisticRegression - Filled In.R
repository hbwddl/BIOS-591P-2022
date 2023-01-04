### Logistic regression in R
## Load Necessary packages
library(epiDisplay)
library(car)
library(multcomp)
library(DescTools)

## Set working directory and read in the data
setwd("~/Work/BIOS 591P 2021/R Materials/4 Logistic Regression")
dengue <- read.table("dengue.dat",header=TRUE)

## Deal with categorical variables
dengue$SECTOR_F <- as.factor(dengue$SECTOR)
dengue$DENGUE_F <- as.factor(dengue$DENGUE)

is.factor(dengue$DENGUE_F)

## Fit model
dengue.logistic <- glm(DENGUE_F ~ MOSNET + SECTOR_F + AGE, data=dengue, family=binomial)

## Change reference level
dengue$DENGUE_F_RE <- relevel(dengue$DENGUE_F,ref=2)
dengue$SECTOR_F_RE <- relevel(dengue$SECTOR_F,ref=5)

dengue.logistic <- glm(DENGUE_F_RE ~ MOSNET + SECTOR_F_RE + AGE, data=dengue, family=binomial)

## Likelihood ratio test
dengue.reduced <- glm(DENGUE_F_RE ~ 1, data=dengue, family=binomial)

lrtest(dengue.reduced,dengue.logistic)

## Type III test
Anova(dengue.logistic,type=3,test="Wald")

## C Statistic
Cstat(dengue.logistic)

## Odds Ratios
exp(coefficients(dengue.logistic))

exp(confint.default(dengue.logistic))

dengue.sector.test <- glht(dengue.logistic,mcp(SECTOR_F_RE="Tukey"))

exp(coefficients(dengue.sector.test))

exp(confint.default(dengue.sector.test))
