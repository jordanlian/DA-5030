# libraries, set working directory
library(tidyverse)
setwd("C:/Users/Jordan Lian/OneDrive - Northeastern University/Spring 2021/DA 5030/Practice 3")

# 1. Download the data set for the tutorial.
origin_prostate <- read.csv("prostate_cancer.csv", stringsAsFactors = FALSE)
prostate <- origin_prostate

' 2. Follow this tutorial on applying kNN to prostate cancer detection and implement all of the steps in an R Notebook. 
Make sure to explain each step and what it does. (Note: The data set provided as part of this assignment has been slightly 
modified from the one used in the tutorial, so small deviations in the result can be expected.)'
  # Step 1 - Data collection, already done above
  
  # Step 2 - Preparing and exploring the data
  prostate <- prostate[-1]  #removes the first variable(id) from the data set.
  table(prostate$diagnosis_result)  # it helps us to get the numbers of patients
  
    # Rename B and M to Benign and Malignant
    prostate$diagnosis <- factor(prostate$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))
    round(prop.table(table(prostate$diagnosis)) * 100, digits = 1)  # get percentage of B/M diagnoses
  
    # Normalize Numeric Data
    normalize <- function(x) {
      return ((x - min(x)) / (max(x) - min(x))) }
    norm_prostate <- as.data.frame(lapply(prostate[2:9], normalize))
    summary(norm_prostate$radius)
    
    # Create test and training data set
    prostate_train <- norm_prostate[1:65,]
    prostate_test <- norm_prostate[66:100,]
  
    # A blank value in each of the above statements indicate that all rows and columns should be included.
    # Our target variable is 'diagnosis_result' which we have not included in our training and test data sets.
    
    train_labels <- prostate[1:65, 1]
    test_labels <- prostate[66:100, 1]
    
  # Step 3 - Train the model on data
  library(class)
  prc_test_pred <- knn(train = prostate_train, test = prostate_test, cl = train_labels, k=10)
  
  # Step 4 - Evaluate the model performance
  library(gmodels)
  CrossTable(x = test_labels, y = prc_test_pred, prop.chisq = FALSE)
  
  # Step 5 - Improve the performance of the model, change the k-value, repeat steps 3-4
  prc_test_pred <- knn(train = prostate_train, test = prostate_test, cl = train_labels, k=8)
  CrossTable(x = test_labels, y = prc_test_pred, prop.chisq = FALSE)
  
  prc_test_pred <- knn(train = prostate_train, test = prostate_test, cl = train_labels, k=9)
  CrossTable(x = test_labels, y = prc_test_pred, prop.chisq = FALSE)
  
  prc_test_pred <- knn(train = prostate_train, test = prostate_test, cl = train_labels, k=11)
  CrossTable(x = test_labels, y = prc_test_pred, prop.chisq = FALSE)
  
  prc_test_pred <- knn(train = prostate_train, test = prostate_test, cl = train_labels, k=12)
  CrossTable(x = test_labels, y = prc_test_pred, prop.chisq = FALSE)
  
' 3. Once you have complete the tutorial, try another kNN implementation from another 
package, such as the caret package. Compare the accuracy of the two implementations.'
library(caret)
  
  # Sampling
    # Splitting data as training and test set. Using createDataPartition() function from caret
    indxTrain <- createDataPartition(y = prostate$diagnosis_result,p = 0.75,list = FALSE)
    training <- prostate[indxTrain,]
    testing <- prostate[-indxTrain,]
    
    # Checking distibution in origanl data and partitioned data
    prop.table(table(prostate$diagnosis_result)) * 100
  
    prop.table(table(testing$diagnosis_result)) * 100
    prop.table(table(prostate$diagnosis_result)) * 100
  
  # Preprocessing
  trainX <- training[,names(training) != "diagnosis_result"]
  preProcValues <- preProcess(x = trainX, method = c("center", "scale"))
  preProcValues
  
  # Training and train control
    set.seed(400)
    ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
    knnFit <- train(diagnosis_result ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
    
    #Output of kNN fit
    knnFit
    
    #Plotting yields Number of Neighbours Vs accuracy (based on repeated cross validation)
    plot(knnFit)
    
# 4. Try the confusionMatrix function from the caret package to determine the accuracy of both algorithms.
    knnPredict <- predict(knnFit, newdata = testing)
    
    #Get the confusion matrix to see accuracy value and other parameter values
    confusionMatrix(knnPredict, as.factor(testing$diagnosis_result))
    mean(knnPredict == testing$diagnosis_result)
    
    #Now verifying 2 class summary function
    ctrl <- trainControl(method="repeatedcv",repeats = 3,classProbs=TRUE,summaryFunction = twoClassSummary)
    knnFit <- train(diagnosis_result ~ ., data = training, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
  
    knnFit    
    plot(knnFit, print.thres = 0.5, type="S")
  
    knnPredict <- predict(knnFit,newdata = testing)
    
    #Get the confusion matrix to see accuracy value and other parameter values
    confusionMatrix(knnPredict, as.factor(testing$diagnosis_result))
    mean(knnPredict == testing$diagnosis_result)    

    # Trying to plot ROC curve to check specificity and sensitivity
    library(pROC)
    knnPredict <- predict(knnFit,newdata = testing , type="prob")
    knnPredict
    knnROC <- roc(testing$diagnosis_result, knnPredict[,"Down"], levels = rev(testing$diagnosis_result))
    knnROC
    
    plot(knnROC, type="S", print.thres= 0.5)
    
    