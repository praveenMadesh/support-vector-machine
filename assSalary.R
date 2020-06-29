library(readr)
library(kernlab)
library(caret)
###train data 
train_data <- read.csv(choose.files())####train salary data 
View(train_data)
str(train_data)

###test data
test_data <- read.csv(choose.files())####test Salary data
View(test_data) 
str(test_data)

####ggplot
library(ggplot2)
ggplot(data = train_data,aes(x = train_data$educationno,fill = train_data$Salary))+
  geom_density(alpha = 0.9,color = 'red')
  
ggplot(data = train_data,aes(x = train_data$maritalstatus,fill = train_data$Salary))+
  geom_density(alpha = 0.8,color = "orange")

####plot
plot(train_data$workclass,train_data$Salary)
plot(train_data$education,train_data$Salary)
plot(train_data$race,train_data$Salary)

###building model
# using Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

#kernel using rbfdot
model_rbfdot <- ksvm(train_data$Salary~.,data = train_data,kernel = "rbfdot")
pred_rbfdot <- predict(model_rbfdot,newdata = test_data)
mean(pred_rbfdot == test_data$Salary)###85.46%
table(pred_rbfdot == test_data$Salary)

##kernel using vanilladot
model_vanilladot <- ksvm(train_data$Salary~.,data = train_data,kernel = "vanilladot")
pred_vanilladot <- predict(model_vanilladot,newdata = test_data)
mean(pred_vanilladot == test_data$Salary)##84.62%
table(pred_vanilladot==test_data$Salary)

##kernel usingn besseldot
model_besseldot <- ksvm(train_data$Salary~.,data = train_data,kelnel = "besseldot")
pred_besseldot <- predict(model_besseldot,newdata = test_data)
mean(pred_besseldot == test_data$Salary)##85.53% this is the best model 
table(pred_besseldot==test_data$Salary)

##kernel using polydot
model_polydot <- ksvm(train_data$Salary~.,data = train_data,kernel = "polydot")
pred_polydot <- predict(model_polydot,newdata = test_data)
mean(pred_polydot == test_data$Salary)##84.62%
table(pred_polydot==test_data$Salary)

