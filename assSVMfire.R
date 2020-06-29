library(readr)
library(kernlab)
library(caret)
library(plyr)
forest_fire <- read.csv(choose.files())###forest fire data
View(forest_fire)
class(forest_fire)
str(forest_fire)
summary(forest_fire)

##histogram on burned area
hist(forest_fire$area)

##apply normalization technique 
norm <- function(x){
  return ((x-min(x))-(max(x)-min(x)))
}
###we need to predict the burned area so that we have to take the values
##of only selected column temp,RH,Wind,rain
forest_fire$temp <- norm(forest_fire$temp)
forest_fire$RH <- norm(forest_fire$RH) 
forest_fire$wind <- norm(forest_fire$RH)
forest_fire$rain <- norm(forest_fire$rain)
attach(forest_fire)

###partition of data into test and train data
set.seed(123)
pd <- sample(2,nrow(forest_fire),replace = T,prob = c(0.7,0.3))
train <- forest_fire[pd==1,]
test <- forest_fire[pd==2,]

## to train model
###ksvm() function usesgassian RBF kernel
#building model
model1 <- ksvm(size_category~temp+RH+wind+rain,data = forest_fire,kernel = "vanilladot")
model1

##predition
area_pred <- predict(model1,test)
plot(area_pred)
table(area_pred,test$size_category)

agreement <- area_pred == test$size_category
prop.table(table(agreement))####False is 31.50% and True is 68.49%
mean(area_pred==test$size_category)#### 68.49%
