### Chi-squared test in R

### Chi-squared test with contingency table
# Create contingency table
fluvac.table <- matrix(data=c(25,31,75,49),nrow=2,ncol=2,byrow=T)

# Check assumption (>5 expected in at least 80% of the cells)
chisq.test(fluvac.table)

fluchi.table <- chisq.test(fluvac.table)

str(fluchi.table)

fluchi.table$expected

### Chi-squared test with dataset
## Set working directory
setwd("~/Work/BIOS 591P 2021/R Materials/1 Introduction/Chi-Square Test")

# Read in the dataset
fluvac.data <- read.csv("fluvaccine.csv")
View(fluvac.data)

# Conduct chi-squared test
chisq.test(fluvac.data$Flu,fluvac.data$Vaccine)

fluchi.data <- chisq.test(fluvac.data$Flu,fluvac.data$Vaccine)

# Check assumption (>5 expected in at least 80% of the cells)
fluchi.data$expected
