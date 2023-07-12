##################### IRFs and FEVDS of VAR #################

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
  forecast,
  xtable
)

# Read Data ------------------------------------------------------------
data <- read_excel("./data/data_com_fin.xlsx") 
colnames(data)[2] <- "p_wheat" # Renaming wheat price and s&p
colnames(data)[7] <- "sp"

data$month <- gsub("M", "", data$month) # Transforming col "month" into date format
data$month <- ym(data$month, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))


# Data inspection------------------------------------------------------------
section_data <- data[data$month >= as.Date("2006-06-01") & data$month <= as.Date("2012-07-01"), ]
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

# Unit root tests ---------------------------------------------------------
test1 = ur.df(data$p_wheat, type = "drift", selectlags = "BIC")
summary(test1) #p_wheat is non-stationary
test2 = ur.df(data$ind_prod[!is.na(data$ind_prod)], type = "drift", selectlags = "BIC")
summary(test2) #ind_prod is non-stationary
test3 = ur.df(data$netlong_mm[!is.na(data$netlong_mm)], type = "drift", selectlags = "BIC")
summary(test3) #netlong_mm is stationary
test4 = ur.df(data$exports_total[!is.na(data$exports_total)], type = "drift", selectlags = "BIC")
summary(test4) #exports is stationary! Different from Ederer et al..
test5 = ur.df(data$p_oil[!is.na(data$p_oil)], type = "drift", selectlags = "BIC")
summary(test5) #price of oil is non-stationary
test6 = ur.df(data$reer[!is.na(data$reer)], type = "drift", selectlags = "BIC")
summary(test6) #real exchange rate is non-stationary but keep in levels
test7 = ur.df(data$sp[!is.na(data$sp)], type = "drift", selectlags = "BIC")
summary(test7) #S&P is non-stationary
test8 = ur.df(data$ir[!is.na(data$ir)], type = "drift", selectlags = "BIC")
summary(test8) #tested non-stationary but theoretically impossible -> keep in levels
test9 = ur.df(data$cpi[!is.na(data$cpi)], type = "drift", selectlags = "BIC")
summary(test9) #CPI is non-stationary

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
ir <-data$ir
cpi_log_diff<-diff(log(data$cpi))
exports_total_log_diff<-diff(log(data$exports_total))
netlong_mm <- data$netlong_mm[-1]/100000 # Dividing by 100000 to make numbers comparable to BVAR
netlong_sd<-data$netlong_swap[-1]/100000 # "" 
reer_log<-reer_log[-1]
ir <- ir[-1]
ir <- ir/100
merged_data<-data.frame(p_wheat_log_diff,p_oil_log_diff,reer_log,ind_prod_log_diff
                   ,sp_log_diff,ir,cpi_log_diff,exports_total_log_diff,
                   netlong_mm,netlong_sd)

date <- data$month[-1]
merged_data<-data.frame(date,merged_data)
subset_data <- merged_data[merged_data$date >= "2006-06-01" & merged_data$date <= "2012-07-01", ]





# Selecting which VAR to run -------------------------------------------
mm <- T
sd <- F









# Small-scale Var for Money managers --------------------------------------------------
if (mm==T) {
  df_var_mm<-(data.frame(ind_prod_log_diff,exports_total_log_diff,netlong_mm,p_wheat_log_diff))
  is.na(df_var_mm)
  var_mm<-VAR(na.omit(df_var_mm), lag.max = 1, ic = "SC", type = "const")
  summary(var_mm)
  coefficients <- coef(var_mm)
  residuals <- resid(var_mm)
  print(coefficients)
}

# IRFs --------------------------------------------------------------------
# Computing impulse response functions with recursive-design wild bootstrap for var_mm
if (mm==T) {
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
}

# FEVDs -------------------------------------------------------------------
if (mm==T) {
fevd_mm<-vars::fevd(var_mm)
print(fevd_mm)

print(xtable(fevd_mm$p_wheat_log_diff, type = "latex"), file = "./tables/fevd_var_mm.tex")
}










# Medium-scale Var for Money managers -------------------------------------
if (mm==T) {
  df_var_mm <- data.frame(subset_data[c(5,9,10,3,2,7,6,4)]) 
  is.na(df_var_mm)
  var_mm<-VAR(na.omit(df_var_mm), lag.max = 1, ic = "SC", type = "const")
  coefficients <- coef(var_mm)
  residuals <- resid(var_mm)
  print(coefficients)
}

# IRFs --------------------------------------------------------------------
# Computing impulse response functions with recursive-design wild bootstrap for var_mm
if (mm==T) {
  set.seed(123) # for reproducibility
  nboot <- 1000 # number of bootstrap replications
  irf1 <- vars::irf(var_mm, impulse = "ind_prod_log_diff", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
  irf2 <- vars::irf(var_mm, impulse = "exports_total_log_diff", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
  irf3 <- vars::irf(var_mm, impulse = "netlong_mm", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
  irf4 <- vars::irf(var_mm, impulse = "p_oil_log_diff", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
  irf5 <- vars::irf(var_mm, impulse = "reer_log", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
  irf6 <- vars::irf(var_mm, impulse = "ir", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
  irf7 <- vars::irf(var_mm, impulse = "sp_log_diff", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
  
  plot(irf1, ylim = c(-0.04, 0.04), main = "Shock ind_prod_log_diff on p_wheat_log_diff", ylab = "")
  abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
  axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
  plot(irf2, ylim = c(-0.03, 0.02), main = "Shock exports_total_log_diff on p_wheat_log_diff", ylab = "")
  abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
  axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
  plot(irf3, ylim = c(-0.03, 0.08), main = "Shock netlong_mm on p_wheat_log_diff", ylab = "")
  abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
  axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
  plot(irf4, ylim = c(-0.04, 0.04), main = "net long mm on p_wheat_log_diff", ylab = "")
  abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
  axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
  plot(irf5, ylim = c(-0.03, 0.02), main = "reer_log on p_wheat_log_diff", ylab = "")
  abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
  axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
  plot(irf6, ylim = c(-0.03, 0.08), main = "Ir on p_wheat_log_diff", ylab = "")
  abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
  axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
  plot(irf7, ylim = c(-0.03, 0.08), main = "sp_log_diff on p_wheat_log_diff", ylab = "")
  abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
  axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
}

# FEVDs -------------------------------------------------------------------
if (mm==T) {
  fevd_mm<-vars::fevd(var_mm)
  print(fevd_mm)
  
  print(xtable(fevd_mm$p_wheat_log_diff, type = "latex"), file = "./tables/fevd_var_mm.tex")
}


# Var for Swap dealers --------------------------------------------------
if (sd==T) {
  df_var_sd<-(data.frame(ind_prod_log_diff,exports_total_log_diff,netlong_sd,p_wheat_log_diff))
  is.na(df_var_sd)
  var_sd<-VAR(na.omit(df_var_sd), lag.max = 10, ic = "AIC", type = "const")
  summary(var_sd)
  coefficients <- coef(var_sd)
  residuals <- resid(var_sd)
  print(coefficients)
}

# IRFs --------------------------------------------------------------------
# Computing impulse response functions with recursive-design wild bootstrap for var_mm
if (sd==T) {
  set.seed(123) # for reproducibility
  nboot <- 1000 # number of bootstrap replications
  irf1 <- vars::irf(var_sd, impulse = "ind_prod_log_diff", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
  irf2 <- vars::irf(var_sd, impulse = "exports_total_log_diff", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
  irf3 <- vars::irf(var_sd, impulse = "netlong_sd", response = "p_wheat_log_diff", n.ahead = 12, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
  par(mfrow = c(3, 1), mar = c(2, 2, 2, 1))
  plot(irf1, ylim = c(-0.04, 0.04), main = "Shock ind_prod_log_diff on p_wheat_log_diff", ylab = "")
  abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
  axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
  plot(irf2, ylim = c(-0.03, 0.02), main = "Shock exports_total_log_diff on p_wheat_log_diff", ylab = "")
  abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
  axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
  plot(irf3, ylim = c(-0.03, 0.08), main = "Shock netlong_sd on p_wheat_log_diff", ylab = "")
  abline(v = seq(1, by = 2), col = "lightgrey", lty = 2)
  axis(1, at = seq(1, by = 2), labels = seq(0, by = 2))
}

# FEVDs -------------------------------------------------------------------
if (sd==T) {
  fevd_sd<-vars::fevd(var_sd)
  print(fevd_sd)
  print(xtable(fevd_sd$p_wheat_log_diff, type = "latex"), file = "./tables/fevd_var_sd.tex")
}












##################### Forecasting VAR ######################

# Loading data --------------------------------------------
rm(list = ls())
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
  forecast,
  vars,
  OOS
)
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
  VAR_data <- data.frame(subset_data[c(2,5,9,10)])
}

if(type == "sd"){
  df <- data.frame(subset_data[c(1,5,9,11,2)]) 
  df <- na.omit(df)
  Traw <- nrow (df)
  Yraw <- df
  VAR_data <- data.frame(subset_data[c(2,5,9,11)])
}


# Selecting lag-order----------------------------------------------------
VARselect(VAR_data, lag.max = 12, type = c("const"))
## The SC criterion is the BIC.  
## Care should be taken when using the AIC as it tends to choose large numbers of lags. 
## Instead, for VAR models, we prefer to use the BIC. Hence optimal lag is 1. 

# Forecasting -------------------------------------------------------------
lag_order <- 1
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
