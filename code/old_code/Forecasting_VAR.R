##################### Forecasting VAR ##################### 

# Loading packages --------------------------------------------
rm(list=ls())

pacman::p_load(
  forecast,
  tidyverse,
  urca,
  readxl,
  ggplot2,
  lubridate,
  vars,
  dplyr,
  MCMCpack,
  magic,
  coda,
  OOS
)

if(suppressWarnings(!require(bvarsv))){
  install.packages("bvarsv")
}

if(suppressWarnings(!require(MCMCpack))){
  install.packages("MCMCpack")
}

if(suppressWarnings(!require(magic))){
  install.packages("magic")
}

require(bvarsv)



# Loading data --------------------------------------------

data <- read_excel("./data/data_com_fin.xlsx") 

colnames(data)[2] <- "p_wheat" # Renaming wheat price and s&p
colnames(data)[7] <- "sp"

data$month <- gsub("M", "", data$month) # Transforming col "month" into date format
data$month <- ym(data$month, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))

# Setting up data --------------------------------------------
p_wheat_log_diff<-diff(log(data$p_wheat))
p_oil_log_diff<-diff(log(data$p_oil))
p_cotton_log_diff<-diff(log(data$p_cotton))               
reer_log<-log(data$reer)
ind_prod_log_diff<-diff(log(data$ind_prod))
sp_log_diff<-diff(log(data$sp))
ir_log_diff<-diff(log(data$ir))
cpi_log_diff<-diff(log(data$cpi))
exports_total_log_diff<-diff(log(data$exports_total))
netlong_mm <- data$netlong_mm[-1]/1000 # Dividing by 1000 to make numbers comparable
netlong_swap<-data$netlong_swap[-1]/1000 # otherwise there was an error 
reer_log<-reer_log[-1]
merged_data<-data.frame(p_wheat_log_diff,p_oil_log_diff,reer_log,ind_prod_log_diff
                        ,sp_log_diff,ir_log_diff,cpi_log_diff,exports_total_log_diff,
                        netlong_mm,netlong_swap)

date <- data$month[-1]
merged_data<-data.frame(date,merged_data)
subset_data <- merged_data[merged_data$date >= "2006-06-01" & merged_data$date <= "2012-07-01", ]




# Selecting desired VAR #######################

type <- c("mm") # Choose this for Money Managers
#type <- c("sd") # Choose this for Swap Dealers
if(type == "mm") {
  df <- data.frame(subset_data[c(1,5,9,10,2)]) 
  df <- na.omit(df)
  Traw <- nrow(df)
  Yraw <- df
}

if(type == "sd"){
  df <- data.frame(subset_data[c(1,5,9,11,2)]) 
  df <- na.omit(df)
  Traw <- nrow (df)
  Yraw <- df
}



# Selecting lag-order----------------------------------------------------
lag_orders <- 0:11
rmse_values <- vector("numeric", length = length(lag_orders))
for (i in 1:length(lag_orders)) {
  lag_order <- lag_orders[i]
  
  # Perform the forecast with the specific lag order
  forecasts_var <- forecast_multivariate(
    Data = Yraw,
    forecast.dates = tail(Yraw$date, 12),
    target = "p_wheat_log_diff",
    horizon = 12,
    method = "var",
    lag.n = lag_order,
    freq = 12
  )
  
  # Calculate forecast errors and RMSE
  forecasted_values <- forecasts_var$forecast
  actual_values <- tail(Yraw$p_wheat_log_diff, 12)
  forecast_errors <- forecasted_values - actual_values
  rmse <- sqrt(mean(forecast_errors^2))
  
  # Store the RMSE value in the vector
  rmse_values[i] <- rmse
}

# Print the summary of forecasts_var
summary(forecasts_var)

# Print the RMSE values
cat("Root Mean Squared Error (RMSE) for different lag orders:\n")
for (i in 1:length(lag_orders)) {
  cat("Lag Order", lag_orders[i], ": ", rmse_values[i], "\n")
}


# Forecasting -------------------------------------------------------------
lag_order <- 10
forecasts_var <- forecast_multivariate(
  Data = Yraw,
  forecast.dates = tail(subset_data$date, 12),
  target = "p_wheat_log_diff",
  horizon = 12,
  method = "var",
  lag.n = lag_order,
  freq = 12
)

summary(forecasts_var)
forecasted_values <- forecasts_var$forecast
actual_values <- tail(subset_data$p_wheat_log_diff, 12)
forecast_errors <- forecasted_values - actual_values

accuracy_var <- accuracy(forecasts_var$forecast, actual_values)
print(accuracy_var)

plot(actual_values, type = "l", col = "blue", ylim = range(c(actual_values, forecasted_values)),
     xlab = "Time")
lines(forecasted_values, col = "red")
legend("topright", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)


# Theil’s U-statistic -----------------------------------------------------
rmse_proposed <- sqrt(mean((forecasted_values - actual_values)^2))
rmse_naive <- sqrt(mean((actual_values - mean(actual_values))^2))
U <- rmse_proposed / rmse_naive
list(U)
#Theil's U-statistic compares the RMSE of the proposed forecasting method 
#(rmse_proposed) to the RMSE of the no-change model (rmse_naive).
#The value of Theil's U-statistic is less than 1 indicating that the proposed 
#forecasting model performs better than the no-change (naïve) model. 


