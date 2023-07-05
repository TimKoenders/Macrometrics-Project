# Setup --------------------
rm(list=ls())

pacman::p_load(
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
  BVAR
)

# Loading data --------------------------------------------

data <- read_excel("./data/data_com_fin.xlsx") 

colnames(data)[2] <- "p_wheat" # Renaming wheat price and s&p
colnames(data)[7] <- "sp"

data$month <- gsub("M", "", data$month) # Transforming col "month" into date format
data$month <- ym(data$month, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))

# Setting up data --------------------------------------------
## Is it really neccesarry to divide financialization data by 1000?
## It works also if not divided

p_wheat_log_diff<-diff(log(data$p_wheat))
p_oil_log_diff<-diff(log(data$p_oil))
p_cotton_log_diff<-diff(log(data$p_cotton))               
reer_log<-log(data$reer)
ind_prod_log_diff<-diff(log(data$ind_prod))
sp_log_diff<-diff(log(data$sp))
ir_log_diff<-diff(log(data$ir))
cpi_log_diff<-diff(log(data$cpi))
exports_total_log_diff<-diff(log(data$exports_total))
netlong_mm <- data$netlong_mm[-1] # Dividing by 1000 to make numbers comparable
netlong_swap<-data$netlong_swap[-1] # otherwise there was an error 
reer_log<-reer_log[-1]
merged_data<-data.frame(p_wheat_log_diff,p_oil_log_diff,reer_log,ind_prod_log_diff
                        ,sp_log_diff,ir_log_diff,cpi_log_diff,exports_total_log_diff,
                        netlong_mm,netlong_swap)

date <- data$month[-1]
merged_data<-data.frame(date,merged_data)
subset_data <- merged_data[merged_data$date >= "2006-06-01" & merged_data$date <= "2012-07-01", ]


# BVAR for Money managers --------------------------
set.seed(123)
df_mm <- data.frame(subset_data[c(5,9,10,2)]) 
df_mm <- na.omit(df_mm)
bvar_mm <- bvar(df_mm,lags = 10,
                  irf =  bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000), 
                  fcast = NULL)
summary(bvar_mm)

# BVAR for Swap dealers --------------------------
set.seed(123)
df_sd <- data.frame(subset_data[c(5,9,11,2)]) 
df_sd <- na.omit(df_sd)
bvar_sd <- bvar(df_sd,lags = 1,
                  irf =  bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000), 
                  fcast = NULL)

# Estimating bvar and calculating irfs, fevd and forecasts
# Cholesky decomposition as "identification = TRUE"
bvar_sd_w <- bvar(df_sd_w,lags = 1,
                  irf =  bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000), 
                  fcast = bv_fcast(horizon = 12, cond_path = NULL, cond_vars = NULL))






# IRFs for Money managers ---------
# Selecting the three impulse variables and the one response variable
set.seed(123)
plot(bvar_mm$irf, 
     vars_impulse = c("ind_prod_log_diff","exports_total_log_diff","netlong_mm"),
     vars_response = "p_wheat_log_diff",
     mfcol=c(3,1))

# IRFs Swap dealers --------
# Selecting the three impulse variables and the one response variable
set.seed(123)
plot(bvar_sd$irf,
     vars_impulse = c("ind_prod_log_diff","exports_total_log_diff","netlong_swap"),
     vars_response = "p_wheat_log_diff",
     mfcol=c(3,1))




# FEVDs for Money managers --------
set.seed(123)

# FEVDs forS wap dealers --------
set.seed(123)





# Forecasts for Money managers --------------------------------------------
#Already specified in the BVAR (horizon=12)

set.seed(123)
predicted_values <- predict(bvar_mm, horizon = 12, conf_bands = c(0.05))
forecasts <- predicted_values$quants
forecast_4_50 <- forecasts[,,4][2,]
forecasted_values <- as.vector(forecast_4_50)
actual_values <- tail(subset_data$p_wheat_log_diff, 12)
forecast_errors <- forecasted_values - actual_values
rmse <- sqrt(mean(forecast_errors^2))
print (rmse)

plot(predict(bvar_mm,conf_bands=c(0.05))) # Confidence interval at 5% and 95%

# Forecasts for Swap dealers ----------------------------------------------
#Already specified in the BVAR (horizon=12)
set.seed(123)
predicted_values <- predict(bvar_sd, horizon = 12, conf_bands = c(0.05))
forecasts <- predicted_values$quants
forecast_4_50 <- forecasts[,,4][2,]
forecasted_values <- as.vector(forecast_4_50)
actual_values <- tail(subset_data$p_wheat_log_diff, 12)
forecast_errors <- forecasted_values - actual_values
rmse <- sqrt(mean(forecast_errors^2))
print (rmse)

plot(predict(bvar_sd,conf_bands=c(0.05))) # Confidence interval at 5% and 95%

