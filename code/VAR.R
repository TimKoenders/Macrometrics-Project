##################### IRFs and FEVDS of VAR ##################### 

# Clean-up and installing packages--------------
  rm(list = ls())
gc()

pacman::p_load(
  tidyverse,
  urca,
  readxl,
  ggplot2,
  lubridate,
  vars,
  dplyr,
  OOS,
  forecast
)

# Read Data ------------------------------------------------------------
data <- read_excel("./data/data_com_fin.xlsx") 
colnames(data)[2] <- "p_wheat" # Renaming wheat price and s&p
colnames(data)[7] <- "sp"

data$month <- gsub("M", "", data$month) # Transforming col "month" into date format
data$month <- ym(data$month, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))


# Data inspection------------------------------------------------------------
section_data <- data[data$month >= as.Date("2006-06-01") & data$month <= as.Date("2012-10-31"), ]
summary(section_data$p_wheat)
summary(section_data$p_wheat_2)
summary(section_data$p_oil)
summary(section_data$p_cotton)
summary(section_data$reer)
summary(section_data$ind_prod)
summary(section_data$sp)
summary(section_data$ir)
summary(section_data$cpi)
summary(section_data$netlong_mm)
summary(section_data$netlong_swap)
summary(section_data$exports_total)

#plotting time series
plot(data$month, data$p_wheat, type = "l")
plot(data$month, data$p_oil, type = "l")
plot(data$month, data$p_cotton, type = "l")
plot(data$month, data$reer, type = "l")
plot(data$month, data$ind_prod, type = "l")
plot(data$month, data$sp, type = "l")
plot(data$month, data$ir, type = "l")
plot(data$month, data$cpi, type = "l")
plot(data$month, data$netlong_mm, type = "l")
plot(data$month, data$netlong_swap, type = "l")
plot(data$month, data$exports_total, type = "l")

# Transforming the data------------------------------------------

p_wheat_log_diff<-diff(log(data$p_wheat))
p_oil_log_diff<-diff(log(data$p_oil))
p_cotton_log_diff<-diff(log(data$p_cotton))               
reer_log<-log(data$reer)
ind_prod_log_diff<-diff(log(data$ind_prod))
sp_log_diff<-diff(log(data$sp))
ir_log_diff<-diff(log(data$ir))
cpi_log_diff<-diff(log(data$cpi))
exports_total_log_diff<-diff(log(data$exports_total))
netlong_mm <- data$netlong_mm[-1]/100000 # Dividing by 100000 to make numbers comparable to BVAR
netlong_swap<-data$netlong_swap[-1]/100000 # "" 
reer_log<-reer_log[-1]
merged_data<-data.frame(p_wheat_log_diff,p_oil_log_diff,reer_log,ind_prod_log_diff
                   ,sp_log_diff,ir_log_diff,cpi_log_diff,exports_total_log_diff,
                   netlong_mm,netlong_swap)

date <- data$month[-1]
merged_data<-data.frame(date,merged_data)


# Estimating the VARs------------------------------

##VAR for Money Managers
df_var_mm<-(data.frame(ind_prod_log_diff,exports_total_log_diff,netlong_mm,p_wheat_log_diff))
is.na(df_var_mm)
var_mm<-VAR(na.omit(df_var_mm), lag.max = 10, ic = "AIC", type = "const")
summary(var_mm)
coefficients <- coef(var_mm)
residuals <- resid(var_mm)
print(coefficients)

##VAR for Swap Dealers
df_var_sd<-(data.frame(ind_prod_log_diff,exports_total_log_diff,netlong_swap,p_wheat_log_diff))
is.na(df_var_sd)
var_sd<-VAR(na.omit(df_var_sd), lag.max = 10, ic = "AIC", type = "const")
summary(var_sd)
coefficients <- coef(var_sd)
residuals <- resid(var_sd)
print(coefficients)


# Plotting the IRFs ------------------------------------------------------

# Computing impulse response functions with recursive-design wild bootstrap for var_mm_w
set.seed(123) # for reproducibility
nboot <- 1000 # number of bootstrap replications

irf1 <- vars::irf(var_mm, impulse = "ind_prod_log_diff", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
irf2 <- vars::irf(var_mm, impulse = "exports_total_log_diff", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
irf3 <- vars::irf(var_mm, impulse = "netlong_mm", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")

par(mfrow = c(3, 1), mar = c(2, 2, 2, 1))
plot(irf1, ylim = c(-0.04, 0.04), main = "Shock ind_prod_log_diff on p_wheat_log_diff", ylab = "")
abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
plot(irf2, ylim = c(-0.03, 0.02), main = "Shock exports_total_log_diff on p_wheat_log_diff", ylab = "")
abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
plot(irf3, ylim = c(-0.03, 0.08), main = "Shock netlong_mm on p_wheat_log_diff", ylab = "")
abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))


# Computing impulse response functions with recursive-design wild bootstrap for var_sd_w
set.seed(123) # for reproducibility
nboot <- 1000 # number of bootstrap replications

irf4 <- vars::irf(var_sd, impulse = "ind_prod_log_diff", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
irf5 <- vars::irf(var_sd, impulse = "exports_total_log_diff", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
irf6 <- vars::irf(var_sd, impulse = "netlong_swap", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")

par(mfrow=c(3,1),mar=c(4,4,2,2))
plot(irf4, ylim = c(-0.04,0.04), main="ind_prod_log_diff on p_wheat_log_diff", ylab="")
abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
plot(irf5, ylim = c(-0.04,0.03), main="exports_total_log_diff on p_wheat_log_diff", ylab="")
abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
plot(irf6, ylim = c(-0.03,0.03), main="netlong_swap on p_wheat_log_diff", ylab="")
abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))

# FEVDs -----------------------------------
fevd_mm<-vars::fevd(var_mm)
print(fevd_mm)
fevd_sd<-vars::fevd(var_sd)
print(fevd_sd)










##################### Forecasting VAR ######################

# Loading data --------------------------------------------
rm(list = ls())
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
netlong_mm <- data$netlong_mm[-1]/100000 # Dividing by 100000 to make numbers comparable
netlong_swap<-data$netlong_swap[-1]/100000 # otherwise there was an error 
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
lag_orders <- 0:9
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
lag_order <- 7
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
rmse_var <- sqrt(mean(forecast_errors^2))
print(rmse_var)

accuracy_var <- accuracy(forecasts_var$forecast, actual_values)
print(accuracy_var)

par(mfrow=c(1,1))
plot(actual_values, type = "l", col = "blue", ylim = range(c(actual_values, forecasted_values)),
     xlab = "Time", main="12 Month Wheat Price Forecast (VAR)")
lines(forecasted_values, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)


# # Theil’s U-statistic -----------------------------------------------------
# rmse_proposed <- sqrt(mean((forecasted_values - actual_values)^2))
# rmse_naive <- sqrt(mean((actual_values - mean(actual_values))^2))
# U_var <- rmse_proposed / rmse_naive
# print(U_var)
# #Theil's U-statistic compares the RMSE of the proposed forecasting method 
# #(rmse_proposed) to the RMSE of the no-change model (rmse_naive).
# #The value of Theil's U-statistic is less than 1 indicating that the proposed 
# #forecasting model performs better than the no-change (naïve) model. 


