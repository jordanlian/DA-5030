DA 5030 Final Project
========================================================
author: Jordan Lian
date: April 28, 2021
autosize: true

Overview
========================================================
Structure of this Video/Presentation

- Overview of the data and where you obtained it
- Business problems, i.e., what I am trying to predict
- How I explored the data
- Data Transformations
- What models I built and why
- How the models performed
- How I evaluated, validated the models
- How I built an ensemble model and how well the ensemble performed
- Summary and key lessons learned

Overview of the Data and Business Problem
========================================================
- Obesity Data
- Can we predict the obesity levels of individuals in Colombia, Peru, or Mexico?
- Mainly considers eating habits and physical condition
- Obtained from University of California Irvine (UCI) Machine Learning Repository (citation in report)

### Data Set

```r
library(tidyverse)
origin <- read_csv('ObesityDataSet_raw_and_data_sinthetic.csv')
obesity <- origin
head(obesity)
```

```
# A tibble: 6 x 17
  Gender   Age Height Weight family_history_~ FAVC   FCVC   NCP CAEC  SMOKE
  <chr>  <dbl>  <dbl>  <dbl> <chr>            <chr> <dbl> <dbl> <chr> <chr>
1 Female    21   1.62   64   yes              no        2     3 Some~ no   
2 Female    21   1.52   56   yes              no        3     3 Some~ yes  
3 Male      23   1.8    77   yes              no        2     3 Some~ no   
4 Male      27   1.8    87   no               no        3     3 Some~ no   
5 Male      22   1.78   89.8 no               no        2     1 Some~ no   
6 Male      29   1.62   53   no               yes       2     3 Some~ no   
# ... with 7 more variables: CH2O <dbl>, SCC <chr>, FAF <dbl>, TUE <dbl>,
#   CALC <chr>, MTRANS <chr>, NObeyesdad <chr>
```

Data Exploration, Part 1
========================================================


```r
# Count Obesity Types 
table(obesity$NObeyesdad)
```

```

Insufficient_Weight       Normal_Weight      Obesity_Type_I     Obesity_Type_II 
                272                 287                 351                 297 
   Obesity_Type_III  Overweight_Level_I Overweight_Level_II 
                324                 290                 290 
```

```r
# Age vs Height vs Weight
library(GGally)
ggpairs(obesity[2:4])
```

![plot of chunk Data Exploration](Presentation-figure/Data Exploration-1.png)

```r
# Weight vs other factors
ggpairs(obesity[c(4, 7)])
```

![plot of chunk Data Exploration](Presentation-figure/Data Exploration-2.png)

Data Exploration, Part 2
========================================================


```r
# Outliers for Continuous variables
hist(obesity$Age)
```

![plot of chunk unnamed-chunk-1](Presentation-figure/unnamed-chunk-1-1.png)

```r
hist(obesity$Height)
```

![plot of chunk unnamed-chunk-1](Presentation-figure/unnamed-chunk-1-2.png)

```r
hist(obesity$Weight)
```

![plot of chunk unnamed-chunk-1](Presentation-figure/unnamed-chunk-1-3.png)

Data Transformations, Part 1
========================================================

### Age Bins

```r
  # Age
summary(NB$Age)
NB$Age <- cut(NB$Age, breaks = 3, labels = c("Young", "Middle-Aged", "Old"))
```

### Normalization

```r
# Normalization Function
normalize <- function(x) {
return ((x - min(x)) / (max(x) - min(x))) }

# Get normalized data
norm_df <- as.data.frame(lapply(kNN[c(2:4, 7:8, 11, 13:14)], normalize))

# Remove Initial Columns
kNN <- subset(kNN, select = -c(2:4, 7:8, 11, 13:14))

# Replace with normalized data
kNN <- cbind(kNN, norm_df)

# Remove norm_df for memory's sake
remove(norm_df)
```

### Binary Code

```r
# Gender
unique(kNN$Gender)
kNN$Gender <- ifelse(kNN$Gender == "Male", 1, 0)

# Family History - Overweight
unique(kNN$family_history_with_overweight)
kNN$family_history_with_overweight <- ifelse(kNN$family_history_with_overweight == "yes", 1, 0)

# FAVC (Do you frequently consume high caloric food)
unique(kNN$FAVC)
kNN$FAVC <- ifelse(kNN$FAVC == "yes", 1, 0)

# SMOKE (Do you smoke?)
unique(kNN$SMOKE)
kNN$SMOKE <- ifelse(kNN$SMOKE == "yes", 1, 0)

# SCC (Do you monitor your calorie consumption?)
unique(kNN$SCC)
kNN$SCC <- ifelse(kNN$SCC == "yes", 1, 0)
```

Data Transformations, Part 2
========================================================

### One Hot Encoding

```r
# Create dummy code
library(caret)
dmy <- dummyVars(" ~ .", data = kNN)

# Create new data frame based off the dummy code
oneHot <- data.frame(predict(dmy, newdata = kNN))

# Remove the one-hot encoding for the obesity type (predicted variable)
oneHot <- subset(oneHot, select = -c(19:26))

# Add the original obesity type column, and rename
kNN <- cbind(oneHot, kNN$NObeyesdad)
names(kNN)[26] <- "NObeyesdad"

# Remove oneHot and dmy from memory
remove(dmy, oneHot)
```

### Convert obesity types to numeric values

```r
unique(Reg$NObeyesdad)
Reg$NObeyesdad <- factor(Reg$NObeyesdad)
Reg$NObeyesdad <- as.numeric(Reg$NObeyesdad)
unique(Reg$NObeyesdad)
```

What models I built and why
========================================================
- Naive Bayes
- k-nearest-neighbors (kNN)
- Regression

Naive Bayes and kNN, are good for categorical classification problems. They both show little to no bias in their models due to their simplicity. Categorical regression is normally used for binary problems, but I wanted to see how a regression model would run with a non-binary problem.

Naive Bayes Results
========================================================








```r
# Confusion Matrix
library(caret)
confusionMatrix(as.factor(NB_pred), as.factor(NB_validation$NObeyesdad))
```

```
Confusion Matrix and Statistics

                     Reference
Prediction            Insufficient_Weight Normal_Weight Obesity_Type_I
  Insufficient_Weight                  42            18              6
  Normal_Weight                         6            32              3
  Obesity_Type_I                        1             1             47
  Obesity_Type_II                       0             0             21
  Obesity_Type_III                      0             0              0
  Overweight_Level_I                    4            11             10
  Overweight_Level_II                   3            11              4
                     Reference
Prediction            Obesity_Type_II Obesity_Type_III Overweight_Level_I
  Insufficient_Weight               0                0                  3
  Normal_Weight                     0                0                  9
  Obesity_Type_I                   12                0                  8
  Obesity_Type_II                  71                0                  9
  Obesity_Type_III                  0               81                  0
  Overweight_Level_I                1                0                 26
  Overweight_Level_II               3                0                 15
                     Reference
Prediction            Overweight_Level_II
  Insufficient_Weight                   4
  Normal_Weight                        10
  Obesity_Type_I                        9
  Obesity_Type_II                      17
  Obesity_Type_III                      0
  Overweight_Level_I                    4
  Overweight_Level_II                  26

Overall Statistics
                                          
               Accuracy : 0.6155          
                 95% CI : (0.5725, 0.6572)
    No Information Rate : 0.1723          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.5499          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: Insufficient_Weight Class: Normal_Weight
Sensitivity                             0.75000              0.43836
Specificity                             0.93432              0.93846
Pos Pred Value                          0.57534              0.53333
Neg Pred Value                          0.96923              0.91239
Prevalence                              0.10606              0.13826
Detection Rate                          0.07955              0.06061
Detection Prevalence                    0.13826              0.11364
Balanced Accuracy                       0.84216              0.68841
                     Class: Obesity_Type_I Class: Obesity_Type_II
Sensitivity                        0.51648                 0.8161
Specificity                        0.92906                 0.8934
Pos Pred Value                     0.60256                 0.6017
Neg Pred Value                     0.90222                 0.9610
Prevalence                         0.17235                 0.1648
Detection Rate                     0.08902                 0.1345
Detection Prevalence               0.14773                 0.2235
Balanced Accuracy                  0.72277                 0.8548
                     Class: Obesity_Type_III Class: Overweight_Level_I
Sensitivity                           1.0000                   0.37143
Specificity                           1.0000                   0.93450
Pos Pred Value                        1.0000                   0.46429
Neg Pred Value                        1.0000                   0.90678
Prevalence                            0.1534                   0.13258
Detection Rate                        0.1534                   0.04924
Detection Prevalence                  0.1534                   0.10606
Balanced Accuracy                     1.0000                   0.65296
                     Class: Overweight_Level_II
Sensitivity                             0.37143
Specificity                             0.92140
Pos Pred Value                          0.41935
Neg Pred Value                          0.90558
Prevalence                              0.13258
Detection Rate                          0.04924
Detection Prevalence                    0.11742
Balanced Accuracy                       0.64641
```

kNN Results
========================================================









```r
# Confusion Matrix
confusionMatrix(as.factor(kNN_test_pred), as.factor(kNN_validation_labels))
```

```
Confusion Matrix and Statistics

                     Reference
Prediction            Insufficient_Weight Normal_Weight Obesity_Type_I
  Insufficient_Weight                  50            15              2
  Normal_Weight                         4            25              2
  Obesity_Type_I                        1             7             72
  Obesity_Type_II                       0             0              4
  Obesity_Type_III                      0             1              1
  Overweight_Level_I                    0             6              3
  Overweight_Level_II                   1            19              7
                     Reference
Prediction            Obesity_Type_II Obesity_Type_III Overweight_Level_I
  Insufficient_Weight               2                0                  6
  Normal_Weight                     0                0                  4
  Obesity_Type_I                    5                0                 10
  Obesity_Type_II                  79                0                  4
  Obesity_Type_III                  1               81                  0
  Overweight_Level_I                0                0                 44
  Overweight_Level_II               0                0                  2
                     Reference
Prediction            Overweight_Level_II
  Insufficient_Weight                   4
  Normal_Weight                         5
  Obesity_Type_I                        9
  Obesity_Type_II                       6
  Obesity_Type_III                      1
  Overweight_Level_I                    8
  Overweight_Level_II                  37

Overall Statistics
                                        
               Accuracy : 0.7348        
                 95% CI : (0.695, 0.772)
    No Information Rate : 0.1723        
    P-Value [Acc > NIR] : < 2.2e-16     
                                        
                  Kappa : 0.6896        
                                        
 Mcnemar's Test P-Value : NA            

Statistics by Class:

                     Class: Insufficient_Weight Class: Normal_Weight
Sensitivity                              0.8929              0.34247
Specificity                              0.9386              0.96703
Pos Pred Value                           0.6329              0.62500
Neg Pred Value                           0.9866              0.90164
Prevalence                               0.1061              0.13826
Detection Rate                           0.0947              0.04735
Detection Prevalence                     0.1496              0.07576
Balanced Accuracy                        0.9157              0.65475
                     Class: Obesity_Type_I Class: Obesity_Type_II
Sensitivity                         0.7912                 0.9080
Specificity                         0.9268                 0.9683
Pos Pred Value                      0.6923                 0.8495
Neg Pred Value                      0.9552                 0.9816
Prevalence                          0.1723                 0.1648
Detection Rate                      0.1364                 0.1496
Detection Prevalence                0.1970                 0.1761
Balanced Accuracy                   0.8590                 0.9381
                     Class: Obesity_Type_III Class: Overweight_Level_I
Sensitivity                           1.0000                   0.62857
Specificity                           0.9911                   0.96288
Pos Pred Value                        0.9529                   0.72131
Neg Pred Value                        1.0000                   0.94433
Prevalence                            0.1534                   0.13258
Detection Rate                        0.1534                   0.08333
Detection Prevalence                  0.1610                   0.11553
Balanced Accuracy                     0.9955                   0.79573
                     Class: Overweight_Level_II
Sensitivity                             0.52857
Specificity                             0.93668
Pos Pred Value                          0.56061
Neg Pred Value                          0.92857
Prevalence                              0.13258
Detection Rate                          0.07008
Detection Prevalence                    0.12500
Balanced Accuracy                       0.73263
```

Regression Results
========================================================





```r
# Confusion Matrix
confusionMatrix(as.factor(round(reg_pred)), as.factor(reg_validation$NObeyesdad))
```

```
Confusion Matrix and Statistics

          Reference
Prediction  1  2  3  4  5  6  7
         1  7  4  0  0  0  1  0
         2 22 18  1  0  0  0  1
         3 26 25  2  1  0 14  3
         4  1 19 44  9  0 43 45
         5  0  6 43 75 80 12 20
         6  0  1  1  0  1  0  1
         7  0  0  0  2  0  0  0

Overall Statistics
                                          
               Accuracy : 0.2197          
                 95% CI : (0.1851, 0.2575)
    No Information Rate : 0.1723          
    P-Value [Acc > NIR] : 0.003021        
                                          
                  Kappa : 0.0745          
                                          
 Mcnemar's Test P-Value : NA              

Statistics by Class:

                     Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6
Sensitivity           0.12500  0.24658 0.021978  0.10345   0.9877 0.000000
Specificity           0.98941  0.94725 0.842105  0.65533   0.6510 0.991266
Pos Pred Value        0.58333  0.42857 0.028169  0.05590   0.3390 0.000000
Neg Pred Value        0.90504  0.88683 0.805252  0.78747   0.9966 0.866412
Prevalence            0.10606  0.13826 0.172348  0.16477   0.1534 0.132576
Detection Rate        0.01326  0.03409 0.003788  0.01705   0.1515 0.000000
Detection Prevalence  0.02273  0.07955 0.134470  0.30492   0.4470 0.007576
Balanced Accuracy     0.55720  0.59691 0.432042  0.37939   0.8193 0.495633
                     Class: 7
Sensitivity          0.000000
Specificity          0.995633
Pos Pred Value       0.000000
Neg Pred Value       0.866920
Prevalence           0.132576
Detection Rate       0.000000
Detection Prevalence 0.003788
Balanced Accuracy    0.497817
```

Ensemble Function
========================================================

I created the ensemble all within a function, that I would call later on. The function would take in a validation dataset, and then take the respective prediction vectors for the algorithm to produce a final prediction vector.

```r
ensemble <- function(obesity) {
  obesity_ensemble <- obesity
  obesity_ensemble$NB <- as.vector(NB_pred)
  obesity_ensemble$kNN <- kNN_test_pred
  obesity_ensemble$Reg <- round(reg_pred)
  
  # Substitute Numeric Values for Regression
  obesity_ensemble$Reg[obesity_ensemble$Reg == 1] <- "Insufficient_Weight"
  obesity_ensemble$Reg[obesity_ensemble$Reg == 2] <- "Normal_Weight"
  obesity_ensemble$Reg[obesity_ensemble$Reg == 3] <- "Obesity_Type_I"
  obesity_ensemble$Reg[obesity_ensemble$Reg == 4] <- "Obesity_Type_II"
  obesity_ensemble$Reg[obesity_ensemble$Reg == 5] <- "Obesity_Type_III"
  obesity_ensemble$Reg[obesity_ensemble$Reg == 6] <- "Overweight_Level_I"
  obesity_ensemble$Reg[obesity_ensemble$Reg == 7] <- "Overweight_Level_II"
  
  for(i in 1:length(obesity_ensemble$NB)){
    pred_vec <- rep(NA, 3)
    pred_vec[1] <- obesity_ensemble$NB[i]
    pred_vec[2] <- obesity_ensemble$kNN[i]
    pred_vec[3] <- obesity_ensemble$Reg[i]
    
    if (pred_vec[1] == pred_vec[2] | pred_vec[2] == pred_vec[3]) {
      obesity_ensemble$best_pred[i] <- pred_vec[2]
    }
    else if (pred_vec[1] == pred_vec[3] | pred_vec[2] == pred_vec[3]) {
      obesity_ensemble$best_pred[i] <- pred_vec[3]
    } 
    else {
      # Get Numerical Values, and get the medium value
      replace(pred_vec, pred_vec == "Insufficient_Weight", 1)
      replace(pred_vec, pred_vec == "Normal_Weight", 2)
      replace(pred_vec, pred_vec == "Obesity_Type_I", 3)
      replace(pred_vec, pred_vec == "Obesity_Type_II", 4)
      replace(pred_vec, pred_vec == "Obesity_Type_III", 5)
      replace(pred_vec, pred_vec == "Overweight_Level_I", 6)
      replace(pred_vec, pred_vec == "Overweight_Level_II", 7)
      
      new_val <- median(pred_vec)
      
      # Replace back the numerical value with the actual prediction
      replace(new_val, new_val == 1, "Insufficient_Weight")
      replace(new_val, new_val == 2, "Normal_Weight")
      replace(new_val, new_val == 3, "Obesity_Type_I")
      replace(new_val, new_val == 4, "Obesity_Type_II")
      replace(new_val, new_val == 5, "Obesity_Type_III")
      replace(new_val, new_val == 6, "Overweight_Level_I")
      replace(new_val, new_val == 7, "Overweight_Level_II")  
      
      obesity_ensemble$best_pred[i] <- new_val
    }
    
  # Overall Prediction
  outcome <- confusionMatrix(as.factor(obesity_ensemble$best_pred), as.factor(obesity_ensemble$NObeyesdad))
  return(outcome)
  }
}
```

Ensemble Results
========================================================


















### Call the function

```r
ensemble(NB_validation)
```

```
Confusion Matrix and Statistics

                     Reference
Prediction            Insufficient_Weight Normal_Weight Obesity_Type_I
  Insufficient_Weight                   0             0              0
  Normal_Weight                         0             0              0
  Obesity_Type_I                        0             0              0
  Obesity_Type_II                       0             0              0
  Obesity_Type_III                     56            73             91
  Overweight_Level_I                    0             0              0
  Overweight_Level_II                   0             0              0
                     Reference
Prediction            Obesity_Type_II Obesity_Type_III Overweight_Level_I
  Insufficient_Weight               0                0                  0
  Normal_Weight                     0                0                  0
  Obesity_Type_I                    0                0                  0
  Obesity_Type_II                   0                0                  0
  Obesity_Type_III                 87               81                 70
  Overweight_Level_I                0                0                  0
  Overweight_Level_II               0                0                  0
                     Reference
Prediction            Overweight_Level_II
  Insufficient_Weight                   0
  Normal_Weight                         0
  Obesity_Type_I                        0
  Obesity_Type_II                       0
  Obesity_Type_III                     70
  Overweight_Level_I                    0
  Overweight_Level_II                   0

Overall Statistics
                                         
               Accuracy : 0.1534         
                 95% CI : (0.1237, 0.187)
    No Information Rate : 0.1723         
    P-Value [Acc > NIR] : 0.8881         
                                         
                  Kappa : 0              
                                         
 Mcnemar's Test P-Value : NA             

Statistics by Class:

                     Class: Insufficient_Weight Class: Normal_Weight
Sensitivity                              0.0000               0.0000
Specificity                              1.0000               1.0000
Pos Pred Value                              NaN                  NaN
Neg Pred Value                           0.8939               0.8617
Prevalence                               0.1061               0.1383
Detection Rate                           0.0000               0.0000
Detection Prevalence                     0.0000               0.0000
Balanced Accuracy                        0.5000               0.5000
                     Class: Obesity_Type_I Class: Obesity_Type_II
Sensitivity                         0.0000                 0.0000
Specificity                         1.0000                 1.0000
Pos Pred Value                         NaN                    NaN
Neg Pred Value                      0.8277                 0.8352
Prevalence                          0.1723                 0.1648
Detection Rate                      0.0000                 0.0000
Detection Prevalence                0.0000                 0.0000
Balanced Accuracy                   0.5000                 0.5000
                     Class: Obesity_Type_III Class: Overweight_Level_I
Sensitivity                           1.0000                    0.0000
Specificity                           0.0000                    1.0000
Pos Pred Value                        0.1534                       NaN
Neg Pred Value                           NaN                    0.8674
Prevalence                            0.1534                    0.1326
Detection Rate                        0.1534                    0.0000
Detection Prevalence                  1.0000                    0.0000
Balanced Accuracy                     0.5000                    0.5000
                     Class: Overweight_Level_II
Sensitivity                              0.0000
Specificity                              1.0000
Pos Pred Value                              NaN
Neg Pred Value                           0.8674
Prevalence                               0.1326
Detection Rate                           0.0000
Detection Prevalence                     0.0000
Balanced Accuracy                        0.5000
```

Summary and key lessons learned
========================================================
- Regression was a poor choice, use decision tree or something else next time
- Look to use bagging or boosting in the ensemble? The equal vote gave the regression model too much a say in determining the ultimate prediction
- Incorporate cross-fold validation if possible? (Not enough time)
- Maybe don't incorporate all variables in your analysis? Find the key components (PCA)
