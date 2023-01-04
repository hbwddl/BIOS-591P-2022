#Regression Diagnostics
#Read in the libraries we need
library(car)
library(MASS)
library(sas7bdat)

#Set working directory
setwd("~/Work/BIOS 591P 2021/R Materials/3 MLR/Data")
bwt <- read.sas7bdat("birthweight.sas7bdat")

#Fit MLR model
bwt.lm <- lm(BWT ~ AGE + WT + SMOKE + HT,data=bwt)

## Partial plots: crPlots()
crPlots(bwt.lm,line=FALSE,smooth=FALSE)

## Leverage: hatvalues()
lev.vals <- hatvalues(bwt.lm)

# Threshold: 2*(k+1)/n
lev.threshold <- 2*(4+1)/length(lev.vals)

abline(v=lev.threshold)

lev.vals > lev.threshold

which(lev.vals > lev.threshold)

bwt[which(lev.vals > lev.threshold),]

## Cook's distance: cooks.distance()
cooksd <- cooks.distance(bwt.lm)

# Threshold: 4/n
cooksd > 4/length(cooksd)

which(cooksd > 4/length(cooksd))

bwt[which(cooksd > 4/length(cooksd)),]

## Jackknife Residuals: studres()
jackres <- studres(bwt.lm)

# Threshold: 2
jackres > 2

which(jackres > 2)

bwt[which(jackres > 2),]

## Correlation
cor(bwt)

## VIFs
vif(bwt.lm)

