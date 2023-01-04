### Regression Diagnostics
## Load the sas7bdat package to read in a SAS dataset
library(sas7bdat)

## Set working directory
setwd("~/Work/BIOS 591P 2021/MLR/")

## Read in the data
bwt <- read.sas7bdat("birthweight.sas7bdat")

## Fit MLR model
bwt.lm <- lm(BWT ~ AGE + WT + SMOKE + HT,data=bwt)

## Load packages for regression diagnostics
library(car)
library(MASS)

## Partial plots: crPlots()
crPlots(bwt.lm,line=FALSE,smooth=FALSE)

## Leverage: hatvalues()
lev.vals <- hatvalues(bwt.lm)

# Threshold: 2*(k+1)/n
lev.threshold <- 2*(4+1)/length(lev.vals)

hist(lev.vals)
abline(v=lev.threshold)

lev.vals > lev.threshold

which(lev.vals > lev.threshold)

bwt[which(lev.vals > lev.threshold),]

## Cook's distance: cooks.distance()

cooksd <- cooks.distance(bwt.lm)

# Threshold: 4/n
cooks.threshold <- 4/length(cooksd)

cooksd > cooks.threshold

which(cooksd > cooks.threshold)

bwt[which(cooksd > cooks.threshold),]

sum(cooksd > cooks.threshold)

hist(cooksd)
abline(v=cooks.threshold)

## Jackknife Residuals: studres()
jackres <- studres(bwt.lm)

# Threshold: absolute value > 2
abs(jackres) > 2

which(abs(jackres) > 2)

bwt[which(abs(jackres) > 2),]

hist(jackres)
abline(v=2)
abline(v=-2)

## Correlation
cor(bwt)
cor(bwt[,2:4])

cor.test(bwt[,3],bwt[,4])

## VIFs
vif(bwt.lm)