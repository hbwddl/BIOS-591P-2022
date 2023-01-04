### One-Way ANOVA in R

### Input the data
group <- as.factor(c(rep(1,11),rep(2,13),rep(3,13)))
score <- c(156,119,107,108,100,170,130,154,107,137,107,132,105,144,136,136,132,159,152,117,89,142,151,82,110,117,124,106,113,94,113,121,101,119,77,90,66)
efficacy <- data.frame(group,score)

### Create a boxplot divided by group

### Fit the one way ANOVA

### Get the ANOVA table

### Perform Tukey's Method for All Pairwise Comparisons
