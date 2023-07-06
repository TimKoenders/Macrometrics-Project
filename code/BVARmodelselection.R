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
  forecast,
  flexmix
)

# Selecting which BVAR to run:
mm <- T
sd <- T

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

# BVARs for Money managers -------------------------------------------------
if (mm==T){
  prior_settings <- bv_priors(hyper = "full") 
  ## using "full"instead of "auto" yields a lot larger ML
  
  set.seed(123)
  df_mm <- data.frame(subset_data[c(5,9,10,2)]) 
  df_mm <- na.omit(df_mm)
  
# Lag orders 1-12
summary_BIC_mm <- list()
for(i in 1:12){
  summary_BIC_mm[i] <- BIC(bvar(df_mm,
                  lags = i,
                  priors = prior_settings,  
                  irf = NULL,
                  fcast = NULL))
  }
}
# Lag order 1 yields the lowest BIC, hence seems to be the best model
# Lag order 2 is only slightly worse, but lag order 1
# also has a higher marginal likelihood

  # BVAR for Swap dealers --------------------------
if (sd==T){
    
    prior_settings <- bv_priors(hyper = "full")  
    ## using "full"instead of "auto" yields a lot larger ML
    
    set.seed(123)
    df_sd <- data.frame(subset_data[c(5,9,11,2)]) 
    df_sd <- na.omit(df_sd)
    
    
    # Lag orders 1-12
    summary_BIC_sd <- list()
    for(i in 1:12){
      summary_BIC_sd[i] <- BIC(bvar(df_sd,
                                    lags = i,
                                    priors = prior_settings, 
                                    irf =  NULL,
                                    fcast = NULL))
    }
}
# Lag order 2 yields the lowest BIC, hence seems to be the best model according to BIC.
# Lag order 1 is only slightly worse, but yields higher posterior marginal likelihood