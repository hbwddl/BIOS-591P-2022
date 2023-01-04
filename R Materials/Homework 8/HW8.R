#### BIOS 591P Spring 2022 Homework 8
# Set working directory in order to read in the data (set to be the folder on your own computer where you have the titanic data)
setwd("/path/to/titanic/file/")

# Read in the titanic.csv data
titanic <- read.csv("Titanic Spring 2022.csv")

# View the data and make sure it read in OK
View(titanic)

########################################
### Filling in the blanks in the report:
########################################
## Number of passengers who survived
# 1 = survived and 0 = didn't survive, so we can just add the data to get the count of who survived
sum(titanic$Survived)

## Percent of passengers who survived
# The percent is just the number of survivors over the total passengers, or the mean of all the 0's (died) and 1's (survived), multiplied by 100 to convert it to percent.
mean(titanic$Survived)*100
# Verify:
# The nrow() function gets the number of rows (number of passengers) in the titanic dataset
sum(titanic$Survived)/nrow(titanic)*100

## Number of males
# Similar to survival, since 0 = female and 1 = male, we can just sum the 'sex' column
sum(titanic$Sex)

## Percent of males
mean(titanic$Sex)*100

## Count under 15
# We will create a new column in the titanic dataset, and assign it a TRUE/FALSE value if the passenger was under 15
titanic$Child <- titanic$Age < 15

View(titanic)

# We add TRUE and FALSE values as if they are 1 for true and 0 for false

sum(titanic$Child)

# What happened!?
# There are some missing values for age so the sum returns a missing value (NA). We have to set an option (na.rm or 'NA remove') to remove missing values
sum(titanic$Child,na.rm=TRUE)

# Percent of passengers under 15, make sure you set the na.rm value again
mean(titanic$Child,na.rm=T)

# Percent of passengers over 15,
1 - mean(titanic$Child,na.rm=T)

# How to see how many passengers had age available?
# The function is.na() returns TRUE if the value is NA or FALSE if it is not NA (not missing)
nrow(titanic) - sum(is.na(titanic$Age)) # Total passengers minus number with missing age

# Another way to do this--the ! operator, placed in front of a TRUE/FALSE statement, inverts TRUE and FALSE. So FALSE becomes TRUE and TRUE becomes FALSE
sum(!is.na(titanic$Age))

## Number of first and second class passengers
# The table() function returns a list which contains the count of each value in an R list
table(titanic$TicketClass)

## Percent of first and second class passengers
# Now we can get the percent for each, by dividing by the total.
# If you divide a list of values in R by a single number, it detects that you want to divide each value of the list by the single number
table(titanic$TicketClass)/nrow(titanic)

###################################
## Code dummy variables/factors
###################################
# Recall that R can handle dummy variables itself, if you put the data into a 'factor'
# We convert a list into a factor with the as.factor() function
# Let's add each variable as a factor in a new column in the titanic dataset
# You can name the new columns whatever you want, I just added the _f at the end of the name

titanic$Survived_f <- as.factor(titanic$Survived)
titanic$Sex_f <- as.factor(titanic$Sex)
titanic$Ticket_f <- as.factor(titanic$TicketClass)
titanic$Child_f <- as.factor(titanic$Child)

View(titanic)

# These new factor columns look the same, but if you view each column, you will see they are factors
titanic$Survived_f

# You can tell it is a factor by the "Levels:" list at the bottom when you print it out.
# The first value listed after "Levels:" is the reference value. So this is saying that for the 'Survived_f' variable, the reference value is 0. R assigns reference alphabetically by default.

titanic$Child_f

# We've got a problem here! Observe in the report that X2 = 1 if >= 15, and 0 if < 15
# We've got it reversed--the reference value is FALSE (Age >= 15, according to the way we coded it)
# We want TRUE to be the reference value, so we use the relevel() function to reassign the reference value, using the 'ref=' option
# Put quotes around the value of 'TRUE', since it's been converted to a factor
# We are going to replace the existing Child_f column. Not necessarily advisable but not the worst thing to do with the relevel() function
titanic$Child_f <- relevel(titanic$Child_f,ref='TRUE')

titanic$Child_f
# It's fixed!

titanic$Sex_f

# The reference looks okay for Sex

# Now we move on to fix the reference value for the ticket class. The reference value should be third class
titanic$Ticket_f

titanic$Ticket_f <- relevel(titanic$Ticket_f,ref='3')

titanic$Ticket_f # Looks better

## Conduct chi-square tests for each binary variable associated with survival
chisq.test(titanic$Sex_f,titanic$Survived_f)
chisq.test(titanic$Child_f,titanic$Survived_f)
chisq.test(titanic$Ticket_f,titanic$Survived_f)

######################################
## Fitting the logistic regression model
######################################
# We use the glm() function (generalized linear model)
# This function takes in the regression formula and data as before, but now we specify family="binomial" in order to tell glm() to fit a logistic regression model
titanic_logistic <- glm(Survived_f ~ Sex_f + Child_f + Ticket_f, data=titanic, family="binomial")
summary(titanic_logistic)

## Summary output:
#                   Estimate      Std. Error    z value     Pr(>|z|)    
#   (Intercept)     1.3585        0.2573        5.279       1.30e-07 ***
#   Sex_f1          -2.5172       0.1653        -15.233     < 2e-16 ***
#   Child_fFALSE    -1.1283       0.2550        -4.425      9.64e-06 ***
#   Ticket_f1       1.8908        0.1976        9.570       < 2e-16 ***
#   Ticket_f2       0.9112        0.1963        4.643       3.44e-06 ***

# (Intercept) is Beta_0
# Sex_f1 is saying that this is the log-odds for Sex_f = 1 versus reference (Sex_f = 0)
# Same for Child_f and Ticket_f

# Since these are the log-odds, we have to take the exponent to get the odds ratios
exp(coefficients(titanic_logistic))

# We can do a little calculation to get the log-odds for 1 vs. 2
# We use the [] brackets to get the 4th and 5th coefficients from the coefficient list
exp(coefficients(titanic_logistic)[4] - coefficients(titanic_logistic)[5])

# Now to get the 95% ci's of the log odds ratios
confint(titanic_logistic)

# Exponentiate to get the 95% CI of the odds ratios
exp(confint(titanic_logistic))

# Another way to get the odds ratio and CI's of 1st class vs 2nd class, and get the 95% CIs, without additional packages, etc, or calculations
# Just relevel the ticket class variable and quickly re-do the logistic regression
titanic$Ticket_f2 <- relevel(titanic$Ticket_f,ref="2")
titanic_logistic2 <- glm(Survived_f ~ Sex_f + Child_f + Ticket_f2, data=titanic, family="binomial")
exp(coefficients(titanic_logistic2))
exp(confint(titanic_logistic2))

# Another way to get the odds ratios and CI's: the multcomp (multiple comparison) package
install.packages("multcomp")
library(multcomp)
titanic_class_test <- glht(titanic_logistic,mcp(Ticket_f="Tukey")) # Telling us to do all pairwise comparisons (tukey) on the Ticket variable

exp(coefficients(titanic_class_test))
# Invert odds ratio
1/exp(coefficients(titanic_class_test))

exp(confint.default(titanic_class_test))
# Invert 95% CIs 
1/exp(confint.default(titanic_class_test))

# To get the C statistic, we use a package: DescTools ('Descriptive tools')
install.packages("DescTools")
library(DescTools)

Cstat(titanic_logistic)

# Desctools also has a nice function for odds ratios
OddsRatio(titanic_logistic)
