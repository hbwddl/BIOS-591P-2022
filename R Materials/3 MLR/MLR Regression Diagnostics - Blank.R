### Regression Diagnostics
## Load the sas7bdat package to read in a SAS dataset
library(sas7bdat)

## Set working directory

## Read in the data
bwt <- read.sas7bdat("birthweight.sas7bdat")

## Fit MLR model
bwt.lm <- lm(BWT ~ AGE + WT + SMOKE + HT,data=bwt)

## Load packages for regression diagnostics

## Partial plots: crPlots()

## Leverage: hatvalues()

# Threshold: 2*(k+1)/n

## Cook's distance: cooks.distance()

# Threshold: 4/n

## Jackknife Residuals: studres()

# Threshold: absolute value > 2

## Correlation

## VIFs