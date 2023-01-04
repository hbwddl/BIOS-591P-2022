#Regression Diagnostics
library(car)
library(MASS)
library(sas7bdat)

setwd("~/Work/BIOS 591P 2021/R Materials/3 MLR/Data")
bwt <- read.sas7bdat("birthweight.sas7bdat")

bwt.lm <- lm(BWT ~ AGE + WT + SMOKE + HT,data=bwt)

crPlots(bwt.lm,line=FALSE,smooth=FALSE)

## Leverage: hatvalues()
lev.vals <- hatvalues(bwt.lm)

lev.threshold <- 2*(4+1)/length(lev.vals)

abline(v=lev.threshold)

lev.vals > lev.threshold

which(lev.vals > lev.threshold)

bwt[which(lev.vals > lev.threshold),]

## Cook's distance
cooksd <- cooks.distance(bwt.lm)

cooksd > 4/length(cooksd)

which(cooksd > 4/length(cooksd))

bwt[which(cooksd > 4/length(cooksd)),]

## Jackknife Residuals
jackres <- studres(bwt.lm)

jackres > 2

which(jackres > 2)

bwt[which(jackres > 2),]

## Correlation
cor(bwt)

## VIFs
vif(bwt.lm)

