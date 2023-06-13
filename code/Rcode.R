# Git--------------

# Header ------------------------------------------------------------------

rm(list = ls())
gc()

pacman::p_load(
  tidyverse,
  urca,
  readxl,
  ggplot2,
  lubridate,
  vars,
  dplyr
)

# Read in Data ------------------------------------------------------------

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


# Plotting time series ----------------------------------------------------
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

#Transforming the data------------------------------------------

p_wheat_log_diff<-diff(log(data$p_wheat))
p_oil_log_diff<-diff(log(data$p_oil))
p_cotton_log_diff<-diff(log(data$p_cotton))               
reer_log<-log(data$reer)
ind_prod_log_diff<-diff(log(data$ind_prod))
sp_log_diff<-diff(log(data$sp))
ir_log_diff<-diff(log(data$ir))
cpi_log_diff<-diff(log(data$cpi))
exports_total_log_diff<-diff(log(data$exports_total))
netlong_mm <- data$netlong_mm[-1]
netlong_swap<-data$netlong_swap[-1]
reer_log<-reer_log[-1]
merged_data<-data.frame(p_wheat_log_diff,p_oil_log_diff,reer_log,ind_prod_log_diff
                   ,sp_log_diff,ir_log_diff,cpi_log_diff,exports_total_log_diff,
                   netlong_mm,netlong_swap)

d <- data$month[-1]
merged_data<-data.frame(d,merged_data)


#estimating VARs------------------------------

### VAR for Money Managers and Wheat Specifying dates###

# df_var_mm_w<-(data.frame(d,ind_prod_log_diff,exports_total_log_diff,netlong_mm_diff,p_wheat_log_diff))
# df_var_mm_w$d<- as.Date(df_var_mm_w$d, format = "%Y-%m-%d")
# start_date <- as.Date("2006-07-01")
# end_date <- as.Date("2012-07-01")
# df_var_mm_w_final <- df_var_mm_w %>% filter(d >= start_date & d<= end_date)
# is.na(df_var_mm_w_final)
# var_mm_w<- VAR(df_var_mm_w_final[,-1], lag.max = 10, ic = "AIC", type = "const")
# summary(var_mm_w)
# coefficients <- coef(var_mm_w)
# residuals <- resid(var_mm_w)
# print(coefficients)

### VAR for Money Managers and Wheat###

df_var_mm_w<-(data.frame(ind_prod_log_diff,exports_total_log_diff,netlong_mm,p_wheat_log_diff))
is.na(df_var_mm_w)
var_mm_w<-VAR(na.omit(df_var_mm_w), lag.max = 10, ic = "AIC", type = "const")
summary(var_mm_w)
coefficients <- coef(var_mm_w)
residuals <- resid(var_mm_w)
print(coefficients)

# Computing impulse response functions with recursive-design wild bootstrap for var_mm_w
set.seed(123) # for reproducibility
nboot <- 1000 # number of bootstrap replications
irf1 <- irf(var_mm_w, impulse = "ind_prod_log_diff", response = "p_wheat_log_diff", n.ahead = 10, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
plot(irf1, ylim = c(-0.1,0.1), main="Demand Shock", ylab="Price of Wheat")
irf2 <- irf(var_mm_w, impulse = "exports_total_log_diff", response = "p_wheat_log_diff", n.ahead = 10, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
plot(irf2, ylim = c(-0.1,0.1), main="Supply Shock", ylab="Price of Wheat")
irf3 <- irf(var_mm_w, impulse = "netlong_mm", response = "p_wheat_log_diff", n.ahead = 10, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
plot(irf3, ylim = c(-0.1,0.1), main="Financial Shock (MM)", ylab="Price of Wheat")


##VAR for Swap Dealers and Wheat###
df_var_sd_w<-(data.frame(ind_prod_log_diff,exports_total_log_diff,netlong_swap,p_wheat_log_diff))
is.na(df_var_sd_w)
var_sd_w<-VAR(na.omit(df_var_sd_w), lag.max = 10, ic = "AIC", type = "const")
summary(var_sd_w)
coefficients <- coef(var_sd_w)
residuals <- resid(var_sd_w)
print(coefficients)

# Computing impulse response functions with recursive-design wild bootstrap for var_sd_w
set.seed(123) # for reproducibility
nboot <- 1000 # number of bootstrap replications
irf4 <- irf(var_sd_w, impulse = "ind_prod_log_diff", response = "p_wheat_log_diff", n.ahead = 10, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
plot(irf4, ylim = c(-0.1,0.1), main="Demand Shock", ylab="Price of Wheat")
irf5 <- irf(var_sd_w, impulse = "exports_total_log_diff", response = "p_wheat_log_diff", n.ahead = 10, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
plot(irf5, ylim = c(-0.1,0.1), main="Supply Shock", ylab="Price of Wheat")
irf6 <- irf(var_sd_w, impulse = "netlong_swap", response = "p_wheat_log_diff", n.ahead = 10, boot = TRUE, nboot = nboot, ci = 0.95, boot.type = "rdwb")
plot(irf6, ylim = c(-0.1,0.1), main="Financial Shock (Swap)", ylab="Price of Wheat")


###forecast error variance decomposition
fevd_mm<-fevd(var_mm_w)
print(fevd_mm)
fevd_sd<-fevd(var_sd_w)
print(fevd_sd)







