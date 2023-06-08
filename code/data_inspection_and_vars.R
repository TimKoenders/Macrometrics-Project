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
  vars
)

# Read in Data ------------------------------------------------------------

data <- read_excel("./data/data_com_fin.xlsx") 

colnames(data)[2] <- "p_wheat" # Renaming wheat price and s&p
colnames(data)[7] <- "sp"

data$month <- gsub("M", "", data$month) # Transforming col "month" into date format
data$month <- ym(data$month, quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"))

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
cpi_log<-diff(log(data$cpi))
exports_total_log_diff<-diff(log(data$exports_total))
netlong_mm_diff<-diff(data$netlong_mm)
netlong_swap_diff<-diff(data$netlong_swap)
reer_log<-reer_log[-1]
merged_data<-data.frame(p_wheat_log_diff,p_oil_log_diff,reer_log,ind_prod_log_diff
                   ,sp_log_diff,ir_log_diff,cpi_log,exports_total_log_diff,
                   netlong_mm_diff,netlong_swap_diff)

d <- data$month[-1]
merged_data<-data.frame(d,merged_data)
#estimating VARs------------------------------
install.packages("vars")

### VAR for Money Managers###
df_var_mm<-(data.frame(d,ind_prod_log_diff,exports_total_log_diff,netlong_mm_diff,p_wheat_log_diff))
library(dplyr)
df_var_mm$d<- as.Date(df_var_mm$d, format = "%Y-%m-%d")
start_date <- as.Date("2006-07-01")
end_date <- as.Date("2012-07-01")
df_var_mm_final <- df_var_mm %>% filter(d >= start_date & d<= end_date)
library(vars)
is.na(df_var_mm_final)
var_mm<- VAR(df_var_mm_final[,-1], lag.max = 10, ic = "AIC", type = "const")
summary(var_mm)

##VAR for Swap Dealers###
df_var_sd<-(data.frame(d,ind_prod_log_diff,exports_total_log_diff,netlong_swap_diff,p_wheat_log_diff))
library(dplyr)
df_var_sd$d<- as.Date(df_var_mm$d, format = "%Y-%m-%d")
start_date <- as.Date("2006-07-01")
end_date <- as.Date("2012-07-01")
df_var_sd_final <- df_var_sd %>% filter(d >= start_date & d<= end_date)
library(vars)
is.na(df_var_sd_final)
var_sd<- VAR(df_var_sd_final[,-1], lag.max = 10, ic = "AIC", type = "const")
summary(var_sd)

###identify VAR and calculate impulse response functions###

bmat <- matrix(0, nrow = 4, ncol = 4)
bmat[lower.tri(bmat)] <- NA
diag(bmat)<-NA

### var_mm
svar_mm <- SVAR(var_mm, estmethod = "direct", Bmat = bmat)
n <- 10 
ir_mm <- irf(svar_mm, n.ahead = n, cumulative = FALSE, boot = TRUE, ci = 0.9, runs = 300)
plot(ir_mm)

###var_sd

svar_sd<- SVAR(var_sd, estmethod = "direct", Bmat = bmat)
n <- 10 
ir_sd <- irf(svar_sd, n.ahead = n, cumulative = FALSE, boot = TRUE, ci = 0.9, runs = 300)
plot(ir_sd)

###forecast error variance decomposition
fevd_mm<-fevd(svar_mm)
print(fevd_mm)
fevd_sd<-fevd(svar_sd)
print(fevd_sd)
