#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# Stocks Iterations
# Max Franke & Damian Etchevest
# 02/12/20
#=======================================================================================================================================

# Install packages
#install.packages("alphavantager")
#install.packages("jsonlite")
#install.packages("TTR")
#install.packages("ncar")
#install.packages("xts")
#install.packages("dplyr")
#install.packages("quantmod")
#install.packages("tidyverse")
#install.packages("quantmod)
#install.packages("magrittr)

# Load packages
#library(alphavantager)
library(jsonlite)
library(forecast)
#library(TTR)
#library(ncar)
#library(dplyr)
#library(xts)
#library(quantmod)
#library(tidyverse)
library(RMySQL)
library(quantmod)
library(magrittr)

# Time Series strategy for buy
#---------------------------------------------------------------------------------------------------------------------------------------

time_series_buy <- function(df_price){
  # Create prediction for next minute
  ts_object <- ts(df_price)
  # Define the current price
  current_price <- tail(Cl(df_price),1)
  # If clause
  if (current_price <= round(ts_object$`Lo 80`, digits = 2)) {
    decision <- TRUE
  } else{
    decision <- FALSE
  }
  return(decision)
}

# Time Series strategy for sell
#---------------------------------------------------------------------------------------------------------------------------------------

time_series_sell <- function(df_price){
  # Create prediction for next minute
  ts_object <- ts(df_price)
  # Define the current price
  current_price <- tail(Cl(df_price),1)
  # If clause
  if (current_price >= round(ts_object$`Hi 80`, digits = 2)) {
    decision <- TRUE
  } else{
    decision <- FALSE
  }
  return(decision)
}


