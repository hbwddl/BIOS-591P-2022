### One-Way ANOVA in R

### Input the data

## To save time entering the data we will use the rep() function. This takes a number as its first argument and the number of replicates as its second.
## Replicate 1, 10 times
rep(1,10)

### Save a list of the group data and a list of the score data
group <- c(rep(1,11),rep(2,13),rep(3,13))

## Recall the reference cell coding/dummy variables that we must do when we have a categorical variable. 
## R can do this automatically for us by treating group as a "factor"
## This is like the CLASS statement in PROC GLM. It takes each unique value in the list and treats it like a category
## We use the as.factor() function with our group list to make the group into a class, with a reference value
group <- as.factor(c(rep(1,11),rep(2,13),rep(3,13)))

group
## We can tell that it's a factor, because when we print it, it tells us the unique levels. Can also use the is.factor function to test it.
is.factor(group)

score <- c(156,119,107,108,100,170,130,154,107,137,107,132,105,144,136,136,132,159,152,117,89,142,151,82,110,117,124,106,113,94,113,121,101,119,77,90,66)

## Now we create our data frame with the group and score data

efficacy <- data.frame(group,score)

## We don't strictly need a data frame here, but they can be nice to work with and it is good practice to use them.

## Create a boxplot divided by group

boxplot(score ~ group,data=efficacy)

## Add a title

boxplot(score ~ group,data=efficacy,main="Distribution of score")

### Fit the one way ANOVA
## To fit the one-way ANOVA model in R, we use the aov() function.
## The arguments for the aov() function will be a model formula, with the dependent (continuous) variable, a tilde (~), then the independent (categorical) variable
## Then we specify our data frame for the second argument, data=
## Save the aov() results into an object

efficacy_anova <- aov(score ~ group,data=efficacy)

## To get the ANOVA table
summary(efficacy_anova)

## To get the means of each group
## We use the aggregate() function. This takes a formula as its first argument, with continuous variable on the left of the ~, then it takes the data= argument
## The last argument in aggregate() is the function that we want to calculate within each group. In this case, we want to calculate the mean
aggregate(score ~ group,data=efficacy,mean)

### Perform Tukey's Method for All Pairwise Comparisons
TukeyHSD(efficacy_anova)
## Output has the difference between groups (2 minus 1, 3 minus 1, and 3 minus 2), confidence intervals, and p-values
