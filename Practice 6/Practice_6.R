library(tidyverse)
library(psych)
setwd("C:/Users/Jordan Lian/OneDrive - Northeastern University/Spring 2021/DA 5030/Practice 6")

# Problem 1 (60 Points)

  ' Download the data set on student achievement in secondary education math education of two Portuguese schools 
   (use the data set Students Math). Using any packages you wish, complete the following tasks:'
  origin_math <- read_csv('student-mat.csv')
  math <- origin_math

  ' 1. (10 pts) Create scatter plots and pairwise correlations between age, absences, G1, and G2 and final grade (G3) 
  using the pairs.panels() function in R.'
  pairs.panels(math[,c(3,30:33)])
  
  '2. (10 pts) Build a multiple regression model predicting final math grade (G3) using as many features as you like 
  but you must use at least four. Include at least one categorical variables and be sure to properly convert it to dummy
  codes. Select the features that you believe are useful -- you do not have to include all features.'
  math$activities[math$activities == 'yes'] <- 1
  math$activities[math$activities == 'no'] <- 0
  math$activities <- factor(math$activities)
  
  multi_model <- lm(G3 ~ G1 + G2 + studytime + activities + absences + health, data = math)
  multi_model
  
  ' 3. (20 pts) Using the model from (2), use stepwise backward elimination to remove all non-significant variables and 
  then state the final model as an equation. State the backward elimination measure you applied (p-value, AIC, Adjusted R2).
  This tutorial shows how to use various feature elimination techniques.'

  # Stepwise regression model
  library(MASS)
  step_model <- stepAIC(multi_model, direction = "both", 
                        trace = FALSE)
  summary(step_model)
  
  # 4. (10 pts) Calculate the 95% confidence interval for a prediction -- you may choose any data you wish for some new student.
  t.test(math$G3)
  
  '5. (10 pts) What is the RMSE for this model -- use the entire data set for both training and validation. You may find the 
  residuals() function useful. Alternatively, you can inspect the model object, e.g., if your model is in the variable m, 
  then the residuals (errors) are in m\$residuals and your predicted values (fitted values) are in m\$fitted.values.'
  library(caret)
  # Make predictions
  predictions <- multi_model %>% predict(math)
  # Model performance
  # (a) Prediction error, RMSE
  RMSE(predictions, math$G3)
  # (b) R-square
  R2(predictions, math$G3)
  
# Problem 2 (40 Points)
  
  ' 1. (5 pts) Using the same data set as in Problem (1), add another column, PF -- pass-fail. Mark any student whose final grade
  is less than 10 as F, otherwise as P and then build a dummy code variable for that new column. Use the new dummy variable column
  as the response variable.'
  math$PF <- math$G3
  math$PF[math$G3 < 10] <- 'F'
  math$PF[math$G3 >= 10] <- 'P'
  
  ' 2. (10 pts) Build a binomial logistic regression model classifying a student as passing or failing. Eliminate any non-significant
  variable using an elimination approach of your choice. Use as many features as you like but you must use at least four -- choose the
  ones you believe are most useful.'
  math$PF[math$PF == 'F'] <- 0
  math$PF[math$PF == 'P'] <- 1
  math$PF <- as.factor(math$PF)
  mylogit <- glm(PF ~ G3 + G2 + G1 + studytime + absences + health, data = math, family = "binomial")
  mylogit <- glm(PF ~ G3 + famrel + reason + traveltime, data = math, family='binomial')
  # 3. (5 pts) State the regression equation.
  mylogit
  summary(mylogit)
  
  # 4. (20 pts) What is the accuracy of your model? Use the entire data set for both training and validation.
  library(gmodels)
  CrossTable(math$PF, accuracy,
             prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
             dnn = c('actual default', 'predicted default'))

  # Make predictions
  accuracy <- mylogit %>% predict(math)
  
  # Model performance
  table(accuracy, math$PF)
  
  # (a) Prediction error, RMSE
  RMSE(accuracy, as.integer(math$PF))
  
  # (b) R-square
  R2(accuracy, as.integer(math$PF))
  
  
# Problem 3 (10 Points)
  # 1. (8 pts) Implement the example from the textbook on pages 205 to 217 for the data set on white wines.
  # Load dataset
  wine <- read.csv("whitewines.csv")
  
  # Histogram
  hist(wine$quality)
  
  # Divide into training/test datasets
  wine_train <- wine[1:3750, ]
  wine_test <- wine[3751:4898, ]
  
  ### Step 3 - training a model on the data
  library(rpart)
  m.rpart <- rpart(quality ~ ., data = wine_train)
  m.rpart
  
  # Visualizing decision trees
  library(rpart.plot)
  rpart.plot(m.rpart, digits = 3)
  
  # change parameters
  rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE,
             type = 3, extra = 101)
  
  ### Step 4 - evaluating model performance
  p.rpart <- predict(m.rpart, wine_test)
  summary(p.rpart)
  summary(wine_test$quality)
  cor(p.rpart, wine_test$quality)
  

  # Measuring performance with the mean absolute error
  MAE <- function(actual, predicted) {
    mean(abs(actual - predicted))
  }
  MAE(p.rpart, wine_test$quality)
  mean(wine_train$quality)
  MAE(5.87, wine_test$quality)

  library(RWeka)
  m.m5p <- M5P(quality ~ ., data = wine_train)
  summary(m.m5p)
  
  p.m5p <- predict(m.m5p, wine_test)
  summary(p.m5p)
  cor(p.m5p, wine_test$quality)
  MAE(wine_test$quality, p.m5p)
  # 2. (2 pts) Calculate the RMSE for the model.
library(rpart.plot)
  
  names(summary(m.m5p))
  summary(m.m5p)$details[[3]]
  