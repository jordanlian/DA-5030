# Problem 2 
library(tidyverse)
setwd("C:/Users/Jordan Lian/OneDrive - Northeastern University/Spring 2021/DA 5030/Practicum 2")  
# Load dataset
  origin_cars <- read_csv('used_car_sales_ebay.csv')
  cars.df <- origin_cars
  
  # Exclude the following columns
  cars.df <- select(cars.df, -one_of('ID', 'zipcode', 'Trim', 'Engine', 'Model', 
                                     'BodyType'))
  
  # Remove NA values for NumCylinders and DriveType
  cars.df <- cars.df[!is.na(cars.df$NumCylinders), ]
  cars.df <- cars.df[!is.na(cars.df$DriveType), ]
  
  # Remove 0 values for NumCylinders and DriveType
  cars.df <- cars.df[cars.df$NumCylinders != 0, ]
  cars.df <- cars.df[cars.df$DriveType != 0, ]
  
  head(cars.df)
  
  # Convert DriveType to one-hot dummy code
  library(fastDummies)
  cars.df <- fastDummies::dummy_cols(cars.df, select_columns = "DriveType")
  knitr::kable(cars.df)
  install.packages('kernlab')
library(kernlab)  
  
  cars.no.df <- cars.df[abs(cars.df$pricesold - mean(cars.df$pricesold)) <= 3*sd(cars.df$pricesold), ]
  cars.no.df
  
  
  training$income <- as.factor(training$income)
  log_reg <- glm(income~age_bins+education+workclass+sex+race+native_country, data=training, family="binomial")
  log_reg
  # Use your model to make predictions, in this example newdata = training set, but replace with your test set
  validation$income <- as.factor(validation$income)
  pdata <- predict(log_reg, newdata = validation, type = "class")
  pdata
  # use caret and compute a confusion matrix
  pdata <- as.factor(pdata)
  confusionMatrix(data = pdata, reference = validation$income)
