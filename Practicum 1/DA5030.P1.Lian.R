library(tidyverse)
library(dplyr)
library(ggpubr)
library(lubridate)
library(tidyr)
library(Metrics)
setwd('C:/Users/Jordan Lian/OneDrive - Northeastern University/Spring 2021/DA 5030/Practicum 1')

# Problem 1 (60 Points)
  # 1. Download the data set Taxi Fare NYC (May 2020) (source: NYC Taxi & Limousine Commission (Links to an external site.)). 
  origin_taxi <- read_csv('yellow_tripdata_2020-05.csv')
  taxi <- origin_taxi 
  
  # 2. Explore the data set to get a sense of the data and to get comfortable with it.
  summary(taxi)
  taxi <- taxi %>% drop_na()
  
  # 3. Create a histogram of column 5 (trip distance) and overlay a normal curve. 
    # Original Plot
    hist(taxi$trip_distance)
    
    # Edit
    mod_taxi <- taxi[taxi$trip_distance <= 25, ]
    hist(mod_taxi$trip_distance,
         main = "Histogram of Trip Distance",
         xlab = "Trip Distance",
         border = "blue",
         col = "green",
         freq=FALSE)
    lines(density(mod_taxi$trip_distance))
  
    ' 4. Test normality of column 5 by performing either a Shapiro-Wilkor
    or Kolmogorov-Smirnof test. Describe what you found.'
    mod_taxi <- taxi[sample(nrow(taxi), 5000), ]
    shapiro.test(mod_taxi$trip_distance)
    
  ' 5. Identify any outliers for the columns using a z-score deviation approach, i.e., consider any values that are more than 2 standard deviations
  from the mean as outliers. Which are your outliers for each column? What would you do? Summarize potential strategies in your notebook.'
    # Trip Distance
      # Get z-scores
      mod_taxi <- taxi %>% 
        mutate(zscore = (trip_distance - mean(trip_distance))/sd(trip_distance))
      
      # Get values with magnitudes greater than 2
      mod_taxi <- mod_taxi[abs(mod_taxi$zscore) >= 2, ]
      head(mod_taxi$zscore)
    # Fare Amount
      # Get z-scores
      mod_taxi <- taxi %>% 
        mutate(zscore = (trip_distance - mean(trip_distance))/sd(trip_distance))
      
      # Get values with magnitudes greater than 2
      mod_taxi <- mod_taxi[abs(mod_taxi$zscore) >= 2, ]
      head(mod_taxi$zscore)    
      
    # Extra
      # Get z-scores
      mod_taxi <- taxi %>% 
        mutate(zscore = (extra - mean(extra))/sd(extra))
      
      # Get values with magnitudes greater than 2
      mod_taxi <- mod_taxi[abs(mod_taxi$zscore) >= 2, ]
      head(mod_taxi$zscore)
    
    # MTA Tax
      # Get z-scores
      mod_taxi <- taxi %>% 
        mutate(zscore = (mta_tax - mean(mta_tax))/sd(mta_tax))
      
      # Get values with magnitudes greater than 2
      mod_taxi <- mod_taxi[abs(mod_taxi$zscore) >= 2, ]
      head(mod_taxi$zscore)

    # Tip Amount
      # Get z-scores
      mod_taxi <- taxi %>% 
        mutate(zscore = (tip_amount - mean(tip_amount))/sd(tip_amount))
      
      # Get values with magnitudes greater than 2
      mod_taxi <- mod_taxi[abs(mod_taxi$zscore) >= 2, ]
      head(mod_taxi$zscore)
    
    # Tolls Amount
      # Get z-scores
      mod_taxi <- taxi %>% 
        mutate(zscore = (tolls_amount - mean(tolls_amount))/sd(tolls_amount))
      
      # Get values with magnitudes greater than 2
      mod_taxi <- mod_taxi[abs(mod_taxi$zscore) >= 2, ]
      head(mod_taxi$zscore)
    
    # Improvement Surcharge
      # Get z-scores
      mod_taxi <- taxi %>% 
        mutate(zscore = (improvement_surcharge - mean(improvement_surcharge))/sd(improvement_surcharge))
      
      # Get values with magnitudes greater than 2
      mod_taxi <- mod_taxi[abs(mod_taxi$zscore) >= 2, ]
      head(mod_taxi$zscore)
    
    # Total Amount
      # Get z-scores
      mod_taxi <- taxi %>% 
        mutate(zscore = (total_amount - mean(total_amount))/sd(total_amount))
      
      # Get values with magnitudes greater than 2
      mod_taxi <- mod_taxi[abs(mod_taxi$zscore) >= 2, ]
      head(mod_taxi$zscore)
    
    # Congestion Surcharge
      # Get z-scores
      mod_taxi <- taxi %>% 
        mutate(zscore = (congestion_surcharge - mean(congestion_surcharge))/sd(congestion_surcharge))
      
      # Get values with magnitudes greater than 2
      mod_taxi <- mod_taxi[abs(mod_taxi$zscore) >= 2, ]
      head(mod_taxi$zscore)
    
  ' 6. Add a new column to the data set called trip_time that is the time of the trip
  in minutes, calculated from the tpep_pickup_datetime and tpep_dropoff_datetime columns.'
  taxi$trip_time <- difftime(taxi$tpep_dropoff_datetime, taxi$tpep_pickup_datetime, units="mins")
  
  # 7. Remove any negative values for the column fare_amount.
  taxi <- taxi[taxi$fare_amount >= 0, ]
  
  ' 8. Create a new new data set (taxi_data_full) only containing columns (in this order): 
  tip_amount, fare_amount, trip_distance, trip_time, congestion_surcharge, payment_type.'
  taxi_data_full <- select(taxi, tip_amount, fare_amount, trip_distance, trip_time, congestion_surcharge, payment_type)
  
  # 9. Standardize the scales of the numeric columns, except the first one (tip_amount), using z-score standardization. 
  standard <- taxi_data_full %>%
    mutate_at(c(2:5), funs(c(scale(.))))
  head(standard)
  summary(standard)
  
  ' 10. The data set is sorted, so creating a validation data set requires random selection of elements. Create a stratified sample where you randomly 
  select 15% of each of the cases for each payment type to be part of the validation data set. The remaining cases will form the training data set.'
  mod_taxi <- taxi_data_full[!is.na(taxi_data_full$payment_type), ]
  pay_one <- mod_taxi[mod_taxi$payment_type == 1, ]
  pay_two <- mod_taxi[mod_taxi$payment_type == 2, ]
  pay_three <- mod_taxi[mod_taxi$payment_type == 3,]
  pay_four <- mod_taxi[mod_taxi$payment_type == 4,]
  
  set.seed(123)
  sample_1 <- sample.int(n = nrow(pay_one), size = floor(0.15*nrow(pay_one)), replace = F)
  val_1 <- pay_one[sample_1, ]
  train_1  <- pay_one[-sample_1, ]
  
  sample_2 <- sample.int(n = nrow(pay_two), size = floor(0.15*nrow(pay_two)), replace = F)
  val_2 <- pay_two[sample_2, ]
  train_2 <- pay_two[-sample_2, ]
  
  sample_3 <- sample.int(n = nrow(pay_three), size = floor(0.15*nrow(pay_three)), replace = F)
  val_3 <- pay_three[sample_3, ]
  train_3 <- pay_three[-sample_3, ]
  
  sample_4 <- sample.int(n = nrow(pay_four), size = floor(0.15*nrow(pay_four)), replace = F)
  val_4 <- pay_four[sample_4, ]
  train_4 <- pay_four[-sample_4, ]
  
  validation <- rbind(val_1, val_2, val_3, val_4)
  training <- rbind(train_1, train_2, train_3, train_4)
  
  remove(pay_one, pay_two, pay_three, pay_four)
  remove(sample_1, sample_2, sample_3, sample_4)
  remove(val_1, val_2, val_3, val_4)
  remove(train_1, train_2, train_3, train_4)
  
  ' 11. Implement the k-NN algorithm in R (do not use an implementation of k-NN from a package) 
  and use your algorithm with a k=5 to predict the tip amount for the following new case:
  fare_amount = 17.5, trip_distance = 4.8, trip_time = 28, congestion_surcharge = 2.5, payment_type = 1'
  
  'Use only the training data set. Note that you need to normalize the values of the new cases the same way as you normalized the original data.
  If the data set is too large to handle on your computer, then create a smaller training data set by randomly sampling the original data set.'

  # create normalization function
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x))) }
  
  # Normalize data
  unknown <- c(17.5, 4.8, 28, 2.5, 1)
  training$trip_time <- as.numeric(training$trip_time)
  df <- as.data.frame(lapply(rbind(unknown, training[, 2:6]), normalize))
  
  pred_values <- c(0)
  for(i in 2:244770){
    mod_df <- na.omit(df[c(1, i), ])
    pred_values <- c(pred_values, dist(mod_df))
  }
  closest <- order(b)

  training[c(closest[2:6]) - 1, 1]
  sum(training[c(closest[2:6]) - 1, 1])
    
  # 12. Apply the knn function from the class package with k=5 and redo the cases from Question (11). Compare your answers.
  
  # training and test labels
  test_labels <- validation[, 1]
  train_labels <- train_class[, 1]
  
  library(class)
  prc_test_pred <- knn(train = train_class, test = validation, cl = train_labels, k=5)
  
  library(gmodels)
  CrossTable(x = test_labels, y = prc_test_pred, prop.chisq = FALSE)
  
  # 13. Using kNN from the class package, create a plot of k (x-axis) from 2 to 8 versus accuracy (percentage of correct classifications) using ggplot.
  
# Problem 2 (30 Points)
  # 1. Investigate this data set of home prices in King County (USA).
  origin_home <- read_csv('kc_house_data.csv')
  home <- origin_home
  
  ' 2. Save the price column in a separate vector/dataframe called target_data. Move all of the columns except the ID, date, price, 
  yr_renovated, zipcode, lat, long, sqft_living15, and sqft_lot15 columns into a new data frame called train_data.'
  target_data <- home$price
  train_data <- select(home, -one_of('id', 'date', 'price', 'yr_renovated', 'zipcode', 'lat', 'long', 'sqft_living15', 'sqft_lot15'))
  home <- select(home, one_of('id', 'date', 'price', 'yr_renovated', 'zipcode', 'lat', 'long', 'sqft_living15', 'sqft_lot15'))
  
  # 3. Normalize all of the columns (except the boolean columns waterfront and view) using min-max normalization.
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x))) }
  norm_home <- as.data.frame(lapply(train_data[-c(6:7)], normalize))
  norm_home <- cbind(norm_home, train_data$waterfront, train_data$view)
  norm_home <- norm_home %>% 
    rename(waterfront = "train_data$waterfront", view = "train_data$view")
  summary(norm_home)
  
  ' 4. Build a function called knn.reg that implements a regression version of kNN that averages the prices of the k nearest neighbors
  using a weighted average where the weight is 3 for the closest neighbor, 2 for the second closest and 1 for the remaining neighbors
  (recall that a weighted average requires that you divide the sum product of the weight and values by the sum of the weights).'
  'It must use the following signature: knn.reg (new_data, target_data, train_data, k), where new_data is a data frame with new cases, 
  target_data is a data frame with a single column of prices from (2), train_data is a data frame with the features from (2) that correspond 
  to a price in target_data, and k is the number of nearest neighbors to consider. It must return the predicted price.'
    # knn.reg function
    knn.reg <- function(new_data, target_data, train_data, k){
      train_data <- cbind(target_data, train_data)
      df <- as.data.frame(lapply(rbind(new_data, train_data[,2:12]), normalize))
      pred_values <- c(0)
      n <- as.numeric(count(train_data))
      # Iterate through data frame and get all the distances and predicted values
      for(i in 2:n){
        # Omit NA values
        mod_df <- na.omit(df[c(1, i), ])
        
        # Store values in list
        pred_values <- c(pred_values, dist(mod_df))
      }
      
      # Get the closest distances and print the top 3 (k=3)
      closest <- order(pred_values)
      train_data[c(closest) - 1, 1]
    
      # get top 2 values
      top_2 <- train_data[c(closest[2:k]) - 1, 1]
      top_2
      
      # get the rest of the values
      rest <- mean(train_data[c(closest[k:n]) - 1, 1])
      top_3 <- c(top_2, rest)
      top_3
      
      # Get the average of the 5 values
      weights <- c(3, 2, 1)
      pred_price <- weighted.mean(top_3, weights)
      return (pred_price)
    }
  
  ' 5. Forecast the price of this new home using your regression kNN using k = 3:
  bedrooms = 3 | bathrooms = 3 | sqft_living = 4850 | sqft_lot = 11240 | floors = 3 | waterfront = 1 | view = 1 | condition = 3 | grade = 11
  sqft_above = 2270 | sqft_basement = 820 | yr_built = 1986'
  
  new_data <- c(3, 3, 4850, 11240, 3, 1, 1, 3, 11, 2270, 820, 1986)
  k <- 3
  knn.reg(new_data, target_data, train_data, k)
  
  # 6. Calculate the Mean Squared Error (MSE) using a random sample of 10% of the data set as test data.
  set.seed(1234)
  test_data <- cbind(target_data, train_data)
  sample <- sample.int(n = nrow(test_data), size = floor(0.10*nrow(test_data)), replace = F)
  test <- test_data[sample, ]
  
  demand <- test$target_data
  n <- as.numeric(count(test))
  prediction <- rep(NA, n)
  for(i in 1:n){
    new_data <- as.numeric(test[i, 2:13])
    prediction[i] <- knn.reg(new_data, target_data, train_data, k)
  }
  
  MSE(prediction, demand)
# Problem 3 (10 Points)
  
  ' 1. Build a new data set with the four columns: tper, year, month, avg_price_sq_ft where tper is the time period starting
  at 1 (e.g., 1, 2, 3, 4, ...), year and month are the year and month of the sale of a property extracted from the column date 
  and avg_price_sq_ft is the average price per square foot of living space for all properties sold that month. The data set should 
  contain the sales in order from least recent to most recent, i.e., the first column after the header column should be the property 
  sold the furthest in the past.'
  home$mon_yr <- format(as.POSIXct(origin_home$date, format="%Y/%m/%d"),"%Y-%m")
  
  price_sqft <- home %>% 
    group_by(mon_yr) %>%
    summarise(avg_price = mean(price/sqft_living))
  
  convert_year <- year(as.POSIXct(home$date, format="%Y/%m/%d"))
  convert_month <- month(as.POSIXct(home$date, format="%Y/%m/%d"))
  
  problem_3 <- data.frame("tper" = c(1:21613), 
                          "year" = convert_year, 
                          "month" = convert_month,
                          "avg_price_sq_ft" = price_sqft$avg_price[match(home$mon_yr,price_sqft$mon_yr)])

  # 2. Plot the average sales price per month as a time series line graph.
  ggplot(data = price_sqft, aes(mon_yr, avg_price, group = 1)) +
    geom_line(color="black", size = 2) +
    geom_point(shape=21, fill="#69b3a2", size=6, color = "#00AFBB") +
    ggtitle("Average Sales per Month") +
    xlab("Year-Month") + ylab("Average Sales") + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
    scale_x_discrete(breaks = price_sqft$mon_yr[seq(1, length(price_sqft$mon_yr), by = 2)])

  ' 3. Forecast the average sales price for the next month is the time series using 
  a weighted moving average of the most recent 3 months with weights of 2, 1.5, and 1.'
  weights <- c(1, 1.5, 2)
  recent <- tail(price_sqft$avg_price, 3)
  forecast <- weighted.mean(recent, weights)
  
  