library(tidyverse)
setwd("C:/Users/Jordan Lian/OneDrive - Northeastern University/Spring 2021/DA 5030/Practice 5")

# Problem 1 (20 Points)
### Step 2 - exploring and preparing the data
credit <- read.csv("credit.csv")
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)

#### Data preparation - creating random training and test datasets
set.seed(123)
train_sample <- sample(1000, 900)
str(train_sample)

credit_train <- credit[train_sample, ]
credit_test  <- credit[-train_sample, ]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

### Step 3 - training a model on the data
library(C50)
credit_train$default<-as.factor(credit_train$default)
credit_model <- C5.0(credit_train[-17], credit_train$default)
credit_model

summary(credit_model)

### Step 4 - evaluating model performance
credit_pred <- predict(credit_model, credit_test)

library(gmodels)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

### Step 5 - improving model performance

#### Boosting the accuracy of decision trees
credit_boost10 <- C5.0(credit_train[-17], credit_train$default,
                       trials = 10)
credit_boost10

summary(credit_boost10)

credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#### Making mistakes more costliers than others
matrix_dimensions <- list(c("no", "yes"), c("no", "yes"))
names(matrix_dimensions) <- c("predicted", "actual")
matrix_dimensions

error_cost <- matrix(c(0, 1, 4, 0), nrow=2)
error_cost

credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# Problem 2 (10 Points)
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)
mushrooms$veil_type <- NULL
table(mushrooms$type)


### Step 3 - training a model on the data
# Sys.setenv('JAVA_HOME'="C:/Program Files/Java/jdk-11.0.1/")
library(rJava)
library(RWeka)
mushroom_1R <- OneR(type ~ ., data = mushrooms)
mushroom_1R

### Step 4 - evaluating model performance
summary(mushroom_1R)

### Step 5 - improving model performance
mushroom_JRip <- JRip(type ~ ., data = mushrooms)
mushroom_JRip
# Problem 3 (35 Points)


# Problem 4 (35 Points)
