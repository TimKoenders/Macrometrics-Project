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
  BVAR,
  forecast
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
train_data <- subset_data[1:(nrow(subset_data) - 12), ]
test_data <- subset_data[(nrow(subset_data) - 11):nrow(subset_data), ]




# Selecting which BVAR to run -------------------------------------------
mm <- T
sd <- F




# BVAR for Money Managers -------------------------------------------------
# IRFs --------------------------------------------------------------------
if (mm==T) {
prior_settings <- bv_priors(hyper = "full")  
set.seed(123)
df_mm <- data.frame(subset_data[c(5,9,10,2)]) 
df_mm <- na.omit(df_mm)
bvar_mm <- bvar(df_mm,
                lags = 1,
                priors = prior_settings,  
                irf = bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000),
                fcast = NULL)
plot(bvar_mm$irf, 
     vars_impulse = c("ind_prod_log_diff","exports_total_log_diff","netlong_mm"),
     vars_response = "p_wheat_log_diff",col="lightblue",
     mfcol=c(3,1))


# FEVDs -------------------------------------------------------------------
set.seed(123)
apply(fevd(bvar_mm)$fevd, c(2, 3, 4), mean)[,,4]
}
# Out-of-sample forecasts ---------------------------------------------------------------
if (mm==T) {
    prior_settings <- bv_priors(hyper = "full")  ## now adjust prior settings
    
    set.seed(123)
    df_mm_train <- data.frame(train_data[c(5,9,10,2)]) 
    df_mm_train <- na.omit(df_mm_train)
    bvar_mm <- bvar(df_mm_train,
                    lags = 1,
                    priors = prior_settings,  
                    irf = bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000),
                    fcast = NULL)
    df_mm_test <- data.frame(test_data[c(5,9,10,2)]) 
    df_mm_test <- na.omit(df_mm_test)
    forecasts <- predict(bvar_mm, bv_fcast(horizon = NROW(test_data), cond_path = NULL, cond_vars = NULL), conf_bands = 0.025, df_mm_test)
    par(mfrow=c(1,1))
    actual_values <- df_mm_test[, 4]
    forecasted_values <- predict(bvar_mm, horizon = NROW(test_data))$quants[2,,4]
    c_low <- predict(bvar_mm, horizon = NROW(test_data))$quants[1,,4] # CI values 
    c_high <- predict(bvar_mm, horizon = NROW(test_data))$quants[3,,4]
    plot(actual_values, type = "l", col = "blue", ylim = range(c(actual_values, forecasted_values)),
         xlab = "Time", main="12 Month Wheat Price Forecast (BVAR)")
    lines(forecasted_values, col = "red")
    lines(c_low, col = "lightblue")
    lines(c_high, col = "lightblue")
    legend("topleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)

# RMSE -------------------------------------------------
rmse.bvar <- function(bvar_mm, test_data) {
  
  if(missing(test_data)) { # In-sample
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
}






# BVAR for Swap dealers ---------------------------------------------------
# IRFs --------------------------------------------------------------------
if (sd==T) {
  prior_settings <- bv_priors(hyper = "full")  
  set.seed(123)
  df_sd <- data.frame(subset_data[c(5,9,11,2)]) 
  df_sd <- na.omit(df_sd)
  bvar_sd <- bvar(df_sd,
                  lags = 1,
                  priors = prior_settings,  
                  irf = bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000),
                  fcast = NULL)
  plot(bvar_sd$irf, 
       vars_impulse = c("ind_prod_log_diff","exports_total_log_diff","netlong_swap"),
       vars_response = "p_wheat_log_diff",
       mfcol=c(3,1))


# FEVDs -------------------------------------------------------------------
set.seed(123)
fevd <- fevd(bvar_sd)
print(fevd$quants[, , , 4])
}

# Out-of-sample forecasts ---------------------------------------------------------------
if (sd==T) {
  prior_settings <- bv_priors(hyper = "full")  ## now adjust prior settings
  
  set.seed(123)
  df_sd_train <- data.frame(train_data[c(5,9,11,2)]) 
  df_sd_train <- na.omit(df_sd_train)
  bvar_sd <- bvar(df_sd_train,
                  lags = 1,
                  priors = prior_settings,  
                  irf = bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000),
                  fcast = NULL)
  df_sd_test <- data.frame(test_data[c(5,9,11,2)]) 
  df_sd_test <- na.omit(df_sd_test)
  forecasts <- predict(bvar_sd, bv_fcast(horizon = NROW(test_data), cond_path = NULL, cond_vars = NULL), conf_bands = 0.025, df_sd_test)
  par(mfrow=c(1,1))
  actual_values <- data.frame(df_sd_test[, 4])
  plot(predict(bvar_sd, horizon = NROW(test_data), conf_bands = 0.025), vars = 4, col = "lightblue",
       ylim = c(-5, 5)) # Set the y-axis limits to -0.5 and 1
  lines(actual_values, col = "red")
  legend("topleft", legend = c("Predicted", "Actual"), col = c("lightblue", "red"), lty = 1)


# RMSE  -------------------------------------------------
rmse.bvar <- function(bvar_sd, test_data) {
  
  if(missing(test_data)) { # In-sample
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
}
apply(fevd(bvar_mm)$fevd, c(2, 3, 4), mean)





