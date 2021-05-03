# libraries
library(tidyverse)
library(ggpubr)

# 2. Import Dataset, Set Column Names
setwd("C:/Users/Jordan Lian/OneDrive - Northeastern University/Spring 2021/DA 5030/Week 1")
data <- read.csv(file = "customertxndata.csv", header = FALSE)
names(data) <- c('Visits','Transactions','Op_Sys', 'Gender', 'Revenue')

# 3. Calculate the following summative statistics
  # Total Transaction Amount (Revenue)
  sum(data$Revenue)
  
  # Mean Number of Visits
  mean(data$Visits)
  
  # Median Revenue
  median(data$Revenue)
  
  # Standard Deviation of Revenue
  sd(data$Revenue)
  
  # Most Common Gender. (Exclude any cases where there is a missing value)
  gender <- data[!is.na(data$Gender),]
  max(gender$Gender)
 
" 4. Create a bar/column chart of gender (x-axis) versus revenue (y-axis). 
Omit missing values, i.e., where gender is NA or missing."

  # Aggregate the total revenue based on gender
  total_revenue <- aggregate(Revenue~Gender, gender, sum)
  names(total_revenue)[2] <- 'Revenue'
  
  # Mutate the data frame using %>%
  total_revenue <- gender %>%
    group_by(Gender) %>%
    summarise(Revenue=sum(Revenue))
  
  # Both methods get the same result
  total_revenue

  # Plot: Revenue vs Gender
  barplot(total_revenue$Revenue, main="Revenue vs Gender",
            xlab="Gender",
            ylab="Total Revenue",
            names.arg=c("Female", "Male"))
  
# 5. What is the Pearson Moment of Correlation between number of visits and revenue? Comment on the correlation.
  
  # Correlation Calculation 
  cor.test(data$Visits, data$Revenue)
  
  # Plot: Revenue vs Visits
  ggplot(data, aes(x=Visits, y=Revenue)) + 
    geom_point() +
    geom_smooth()+
    stat_cor(method = "pearson", label.x = 5, label.y = 1500) + 
    ggtitle("Revenue vs Visits") +
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) 

# 6. Which columns have missing data? How did you recognize them? How would you impute missing values?
colnames(data)[colSums(is.na(data)) > 0] # Used stack overflow https://stackoverflow.com/questions/20364450/find-names-of-columns-which-contain-missing-values

" 7. Impute missing transaction and gender values. Use the mean for transaction 
(rounded to the nearest whole number) and the mode for gender."

  # Mean Transaction
  mean_trans <- mean(data$Transactions, na.rm = TRUE)
  mean_trans
  
  # Round Value
  round(mean_trans)
  
  # Mode for Gender
  max(gender$Gender)

  # Insert the mean and mode into the respective columns using %>%
  mod_data <- data %>% replace_na(list(Transactions = round(mean_trans), Gender = max(gender$Gender))) #https://tidyr.tidyverse.org/reference/replace_na.html
  head(mod_data)
  
" 8. Split the data set into two equally sized data sets where one can be used for training a model and the other for validation. 
Take every odd numbered case and add them to the training data set and every even numbered case and add them to the validation 
data set, i.e., row 1, 3, 5, 7, etc. are training data while rows 2, 4, 6, etc. are validation data."
  
# https://www.xspdf.com/help/50165482.html
  
  # Training Data
  training <- mod_data %>% dplyr::filter(row_number() %% 2 == 1) ## Odd rows
  
  # Validation Data
  validation <- mod_data %>% dplyr::filter(row_number() %% 2 == 0) ## Even rows

# 9. Calculate the mean revenue for the training and the validation data sets and compare them. Comment on the difference.

  # Training Data
  mean(training$Revenue)
  
  # Validation Data
  mean(validation$Revenue)
  
  # Mean of the two means
  0.5 * (mean(training$Revenue) + mean(validation$Revenue))
  
" 10. For many data mining and machine learning tasks, there are packages in R. 
 Use the sample() function to split the data set, so that 60% is used for training and 20% is used for testing, 
and another 20% is used for validation. To ensure that your code is reproducible and that everyone gets the same result, 
 use the number 77654 as your seed for the random number generator. Use the code fragment below for reference: "

  # Set Seed so that same sample can be reproduced in future also
  set.seed(77654)
    
  # Get 60% of data as the training set 
  sample_1 <- sample.int(n = nrow(mod_data), size = floor(.60*nrow(mod_data)), replace = F)
  train <- mod_data[sample_1, ]
  test_val  <- mod_data[-sample_1, ]
  
  # Split the Remaining 40%
  sample_2 <- sample.int(n = nrow(test_val), size = floor(.50*nrow(test_val)), replace = F)
  test <- test_val[sample_2, ]
  valid <- test_val[-sample_2, ]