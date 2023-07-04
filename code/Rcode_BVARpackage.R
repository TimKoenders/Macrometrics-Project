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

d <- data$month[-1]
merged_data<-data.frame(d,merged_data)

## Money managers --------------------------
# Needed time series: ind_prod, exports, netlong_mm, p_wheat
# Selecting the needed time series in the correct order:
df_mm_w <- data.frame(merged_data[c(5,9,10,2)]) 
df_mm_w <- na.omit(df_mm_w)

## Swap dealers --------------------------
# Needed time series: ind_prod, exports, netlong_swap, p_wheat
# Selecting the needed time series in the correct order:
df_sd_w <- data.frame(merged_data[c(5,9,11,2)]) 
df_sd_w <- na.omit(df_sd_w)

# BVARs ----------------

## Money Managers --------
# Estimating bvar and calculating irfs, fevd and forecasts
# Cholesky decomposition as "identification = TRUE"
bvar_mm_w <- bvar(df_mm_w,lags = 1,
                  irf =  bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000), 
                  fcast = bv_fcast(horizon = 12, cond_path = NULL, cond_vars = NULL))

## Swap dealers --------
# Estimating bvar and calculating irfs, fevd and forecasts
# Cholesky decomposition as "identification = TRUE"
bvar_sd_w <- bvar(df_sd_w,lags = 1,
                  irf =  bv_irf(horizon = 12, fevd = TRUE, identification = TRUE, sign_restr = NULL, sign_lim = 1000), 
                  fcast = bv_fcast(horizon = 12, cond_path = NULL, cond_vars = NULL))

# IRFs ---------
## Money Managers --------
# Selecting the three impulse variables and the one response variable
plot(bvar_mm_w$irf, 
     vars_impulse = c("ind_prod_log_diff","exports_total_log_diff","netlong_mm"),
     vars_response = "p_wheat_log_diff",
     mfcol=c(3,1))
## Swap dealers --------
# Selecting the three impulse variables and the one response variable
plot(bvar_sd_w$irf,
     vars_impulse = c("ind_prod_log_diff","exports_total_log_diff","netlong_swap"),
     vars_response = "p_wheat_log_diff",
     mfcol=c(3,1))

# FEVDs ---------
## Money Managers --------

## Swap dealers --------


# Forecasts ---------
# Already specified in the BVAR (horizon=12)
## Money Managers --------
plot(predict(bvar_mm_w,conf_bands=c(0.05))) # Confidence interval at 5% and 95%
## Swap dealers --------
plot(predict(bvar_sd_w,conf_bands=c(0.05))) # Confidence interval at 5% and 95%
