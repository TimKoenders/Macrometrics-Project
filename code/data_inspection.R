# Header ------------------------------------------------------------------

rm(list = ls())
gc()

pacman::p_load(
  tidyverse,
  urca,
  readxl,
  ggplot2,
  lubridate
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

