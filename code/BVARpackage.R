##################### IRFs, FEVDs and Forecasting BVAR ##################### 

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
netlong_mm <- data$netlong_mm[-1]/100000 # Dividing by 100000 to make numbers comparable
netlong_swap<-data$netlong_swap[-1]/100000 # otherwise there was an error 
reer_log<-reer_log[-1]
merged_data<-data.frame(p_wheat_log_diff,p_oil_log_diff,reer_log,ind_prod_log_diff
                        ,sp_log_diff,ir_log_diff,cpi_log_diff,exports_total_log_diff,
                        netlong_mm,netlong_swap)

date <- data$month[-1]
merged_data<-data.frame(date,merged_data)
subset_data <- merged_data[merged_data$date >= "2006-06-01" & merged_data$date <= "2012-07-01", ]





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

# FEVDs for Swap dealers --------
set.seed(123)





# BVAR for Money managers -------------------------------------------------
prior_settings <- bv_priors(hyper = "full")  ## now adjust prior settings

set.seed(123)
df_mm <- data.frame(subset_data[c(5,9,10,2)]) 
df_mm <- na.omit(df_mm)
bvar_mm <- bvar(df_mm,
                lags = 7,
                priors = prior_settings,  
                irf = bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000),
                fcast = NULL)

# RMSE for Money managers -------------------------------------------------
holdout <- tail(subset_data, 12)
rmse.bvar <- function(bvar_mm, holdout) {
  
  if(missing(holdout)) { # In-sample
    apply(resid(bvar_mm, type = "mean"), 2, function(r) sqrt(sum(r^2) / length(r)))
  } else { # Out-of-sample
    fit <- apply(predict(bvar_mm, horizon = NROW(holdout))$fcast, c(2, 3), mean)
    err <- fit - holdout
    apply(err, 2, function(r) sqrt(sum(r^2) / length(r)))
  }
}
rmse_bvar <- rmse.bvar(bvar_mm)
rmse_bvar <- rmse_bvar["p_wheat_log_diff"]
list(rmse_bvar)

# Theil’s U-statistic for Money managers -----------------------------------------------------
actual_values <- tail(subset_data$p_wheat_log_diff, 12)
rmse_naive <- sqrt(mean((actual_values - mean(actual_values))^2))
U_bvar <- rmse_bvar / rmse_naive
print(U_bvar)
#Theil's U-statistic compares the RMSE of the proposed forecasting method 
#(rmse_proposed) to the RMSE of the no-change model (rmse_naive).
#The value of Theil's U-statistic is bigger than 1 indicating that the proposed 
#forecasting model performs worse than the no-change (naïve) model. 


# Forecast plots for Money managers --------------------------------------------
plot(predict(bvar_mm,conf_bands=c(0.05))) # Confidence interval at 5% and 95%



# BVAR for Swap dealers --------------------------
prior_settings <- bv_priors(hyper = "full")  ## now adjust prior settings

set.seed(123)
df_sd <- data.frame(subset_data[c(5,9,11,2)]) 
df_sd <- na.omit(df_sd)
bvar_sd <- bvar(df_sd,lags = 7,
                priors = prior_settings, 
                irf =  bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000), 
                fcast = NULL)

# RMSE for Swap dealers -------------------------------------------------
holdout <- tail(subset_data, 12)
rmse.bvar <- function(bvar_sd, holdout) {
  
  if(missing(holdout)) { # In-sample
    apply(resid(bvar_sd, type = "mean"), 2, function(r) sqrt(sum(r^2) / length(r)))
  } else { # Out-of-sample
    fit <- apply(predict(bvar_sd, horizon = NROW(holdout))$fcast, c(2, 3), mean)
    err <- fit - holdout
    apply(err, 2, function(r) sqrt(sum(r^2) / length(r)))
  }
}
rmse_bvar <- rmse.bvar(bvar_sd)
rmse_bvar <- rmse_bvar["p_wheat_log_diff"]
list(rmse_bvar)

# Theil’s U-statistic for Swap dealers -----------------------------------------------------
actual_values <- tail(subset_data$p_wheat_log_diff, 12)
rmse_naive <- sqrt(mean((actual_values - mean(actual_values))^2))
U_bvar <- rmse_bvar / rmse_naive
print(U_bvar)
#Theil's U-statistic compares the RMSE of the proposed forecasting method 
#(rmse_proposed) to the RMSE of the no-change model (rmse_naive).
#The value of Theil's U-statistic is bigger than 1 indicating that the proposed 
#forecasting model performs worse than the no-change (naïve) model. 

# Forecast plot for Swap dealers ----------------------------------------------
plot(predict(bvar_sd,conf_bands=c(0.05))) # Confidence interval at 5% and 95%

