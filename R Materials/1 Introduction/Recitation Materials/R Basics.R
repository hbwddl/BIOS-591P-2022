### R intro/R basics

# Comment lines with the # sign at the beginning of the line

# Run code in the console by hitting enter (or return)
# Run code from your R script by hitting ctrl+enter (or cmd+return) or clicking the "run" button

# Math
4+5 # R doesn't end lines with ;
4-5
3*6
2/3
4^2
sqrt(9)
exp(2)
log(4)

### Functions
print("Hello World") # Arguments for a function go inside the parentheses
prod(2,3,4) # Some functions take multiple arguments
rnorm(n=100,mean=4,sd=3) # Generating 100 random values from a normal distribution with mean of 4 and standard deviation of 3

?rnorm #Get help for a function


### R Objects/Variables
# Assign/save an object
x <- 4
x + 7

x <- 5

### Variable naming rules
Avariable <- 5 # Any letters allowed
a.variable <- 4 # Periods allowed
a_variable <- 4 # Underscores allowed
a-variable <- 4 # Dashes not allowed
1object <- 4 # Cannot start with number
object1 <- 4 # Can end with a number

### Make sure the variable doesn't already exist
p # Doesn't exist--okay to use
t # Already exists--bad to use

### Types of data
# Numeric
a.number <- 4

# Character
a.string <- "abcde" # 'string' is another word for text
mean(a.string) # Functions for numbers don't work on text

# Missing data
NA + 4 # NA is missing data in R (Recall in SAS it was . )

# Vector
# A vector is like a list of numbers
a.vector <- c(3,4,5,6,8,5,2) # Create a vector with c()
a.vector + 5 # R does math on each member of the vector

# Data frames
vector1 <- c(1,2,3,4,5,6)
vector2 <- c(2,3,4,5,6,7)
vector3 <- c(8,3,4,5,2,5)
vector.df <- data.frame(vector1,vector2,vector3) # Create a data frame from vectortors

dim(vector.df) # Get number of rows and columns

# Get values from data frame
vector.df[4,2] # Get value in fourth row, second column
vector.df[2,3] # Get value in second row, third column
vector.df[5,] # Get entire fifth row
vector.df[,3] # Get entire third column
vector.df$vector1 # Get column 'vector1'
vector.df$vector3 # Get column 'vector3'

## Correlation coefficient
cor(vector.df$vector1,vector.df$vector3)

## Scatterplot
plot(vector.df$vector1,vector.df$vector3)