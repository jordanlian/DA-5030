library(tidyverse)
library(ggpubr)

' 1. The built-in dataset USArrests contains statistics about violent crime rates in the US States. 
Determine which states are outliers in terms of murders. Outliers, for the sake of this question, 
are defined as values that are more than 1.5 standard deviations from the mean.'
  # New Data Frame
  df <- USArrests
  
  # Reference Statistics
  mean(df$Murder)
  1.5 * sd(df$Murder)
  
  # Use row.names()
  row.names(df)[abs(df$Murder - mean(df$Murder)) > (1.5*sd(df$Murder))]

' 2. For the same dataset as in (1), is there a correlation between urban population and murder, 
i.e., as one goes up, does the other statistic as well? Comment on the strength of the correlation. 
Calculate the Pearson coefficient of correlation in R.'
  
  # Correlation Calculation
  cor.test(df$Murder, df$UrbanPop)
  
  # Plot: Urban Population vs Murder
  ggplot(df, aes(x=Murder, y=UrbanPop)) + 
    geom_point() +
    geom_smooth() +
    stat_cor(method = "pearson", label.x = 7.5, label.y = 35) +
    ylab("Urban Population") +
    ggtitle("Urban Population vs Murder") + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5)) 

' 3. Based on the data on the growth of mobile phone use in Brazil (you will need to copy the data 
and create a CSV that you can load into R or use the gsheet2tbl() function from the gsheet package), 
forecast phone use for the next time period using a 2-year weighted moving average (with weights of 
5 for the most recent year, and 2 for other), exponential smoothing (alpha of 0.4), and linear regression trendline.'

  # Load Library, Import Dataset
  library(gsheet)
  url <- 'https://docs.google.com/spreadsheets/d/1tOnM9XceK4Ak8tzWQ2vDelWlJexzJiS3LbT6MN6_rW0/edit?usp=sharing'
  brazil <- gsheet2tbl(url)
  brazil

  # 2-year weighted moving average
    # New Data Frame, Initialize New Column
    weight_MA <- brazil
    weight_MA$Forecast <- rep(NA, 12)
  
    # Store Weights Into a Vector
    weights <- c(2, 5)

    # Use weighted.mean() to get the weighted averages
    for(i in 3:12){
      prev_2 <- c(brazil$Subscribers[(i-2): (i-1)])
      weight_MA$Forecast[i] <- weighted.mean(prev_2, weights)
    }
    
    # Forecast Data
    weight_MA
    
    # Forecast for the Next Time Period
    weight_MA$Forecast[12]
  
  # Exponential Smoothing (alpha = 0.4)
    # New Data Frame, Initialize New Column
    expo_smooth <- brazil
    expo_smooth$Forecast <- rep(NA, 12)
    
    # Initialize First Forecast Value using Weighted Moving Average at Year 3
    expo_smooth$Forecast[3] = weight_MA$Forecast[3]
    
    # Get Forecast Values
    for(i in 4:12){
      expo_smooth$Forecast[i] <- expo_smooth$Forecast[i-1] + 0.4*(brazil$Subscribers[i-1] - expo_smooth$Forecast[i-1])
    }
    
    # Forecast Data
    expo_smooth
    
    # Forecast for the Next Time Period
    expo_smooth$Forecast[12]
  
  # Linear Regression Trendline
    # Get Regression Equation for Forecast Values
    reg <- lm(Subscribers ~ Year, data = brazil)
    
    # Plot the Data to Visualize
    ggscatter(brazil, x = "Year", y = "Subscribers", add = "reg.line") +
      stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
      stat_regline_equation(label.y = 1.9*100000000)
    
    # New Data Frame, Initialize New Column
    lin_reg <- brazil
    lin_reg$Forecast <- rep(NA, 12)
    
    # Get Forecast Values
    for(i in 1:12){
      lin_reg$Forecast[i] <- reg$coefficients[1] + (reg$coefficients[2] * i)
    }
    
    # Forecast Data
    lin_reg
    
    # Forecast for the Next Time Period
    lin_reg$Forecast[12]

' 4. Calculate the squared error for each model, i.e., use the model to calculate a forecast 
for each given time period and then the squared error. Finally, calculate the average (mean) squared 
error for each model. Which model has the smallest mean squared error (MSE)?'

    # Weighted Moving Average 
      # Create Error and Square Error Columns
      weight_MA$Error <- weight_MA$Forecast - weight_MA$Subscribers
      weight_MA$Sq_Error <- weight_MA$Error^2
      
      # Square Error for Time Periods
      weight_MA
      
      # MSE
      weight_MA_MSE <- mean(weight_MA$Sq_Error, na.rm = T)
      weight_MA_MSE
      
    # Exponential Smoothing
      # Create Error and Square Error Columns
      expo_smooth$Error <- expo_smooth$Forecast - expo_smooth$Subscribers
      expo_smooth$Sq_Error <- expo_smooth$Error^2
      
      # Square Error for Time Periods
      expo_smooth
      
      # MSE
      expo_smooth_MSE <- mean(expo_smooth$Sq_Error, na.rm = T)
      expo_smooth_MSE
      
    # Linear Regression
      # Create Error and Square Error Columns
      lin_reg$Error <- lin_reg$Forecast - lin_reg$Subscribers
      lin_reg$Sq_Error <- lin_reg$Error^2
      
      # Square Error for Time Periods
      lin_reg
      
      # MSE
      lin_reg_MSE <- mean(lin_reg$Sq_Error, na.rm = T)
      lin_reg_MSE
      
      # Smallest MSE
      min(weight_MA_MSE, expo_smooth_MSE, lin_reg_MSE)
      
' 5. Calculate a weighted average forecast by averaging out the three forecasts 
calculated in (3) with the following weights: 4 for trend line, 2 for exponential smoothing, 
1 for weighted moving average. Remember to divide by the sum of the weights in a weighted average.'
      
  # New Data Frame, Initialize New Column
  weight_AF <- brazil
  weight_AF$Forecast <- rep(NA, 12)
  
  # Store Weights Into a Vector      
  forecast_weights <- c(4, 2, 1)
        
  # Use weighted.mean() to get the weighted averages
  for(i in 3:12){
    forecast_vals <- c(lin_reg$Forecast[i], expo_smooth$Forecast[i], weight_MA$Forecast[i])
    weight_AF$Forecast[i] <- weighted.mean(forecast_vals, forecast_weights)
  }
  
  # Forecast Data
  weight_AF
