### Two Sample T Test

setwd("~/Work/BIOS 591P 2021/R Materials/1 Introduction/Two Sample T Test")
strokestudy <- read.csv("strokestudy.csv")
print(strokestudy)

View(strokestudy)
dim(strokestudy)
names(strokestudy)
strokestudy$gender
table(strokestudy$gender)
mean(strokestudy$age)
var(strokestudy$age)
median(strokestudy$age)
boxplot(strokestudy$age)
hist(strokestudy$age)
boxplot(strokestudy$age ~ strokestudy$gender)


par(mfrow=c(1,2)) #Setting the graph area to include two graphs, in 1 row and 2 columns

strokemale <- strokestudy[which(strokestudy$gender==1),] #Copying the male patients to a new dataset
hist(strokemale$age) #histogram of male stroke patients age

strokefemale <- strokestudy[which(strokestudy$gender==2),] #Copying the female patients to a new dataset
hist(strokefemale$age) #histogram of female stroke patients age

par(mfrow=c(1,1)) #Setting the graph area to only include 1 plot


qqnorm(strokemale$age)
qqnorm(strokefemale$age)

qqnorm(strokemale$age)
qqline(strokemale$age)