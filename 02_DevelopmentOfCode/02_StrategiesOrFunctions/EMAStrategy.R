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

# EMA strategy 
#---------------------------------------------------------------------------------------------------------------------------------------
EMA_Strategy <- function(df_price){
  # Define the current price
  current_price <- tail(Cl(df_price),1)[[1]]
  # Default for long and short
  long <- FALSE
  short <- FALSE 
  # Define open long and open short
  open_long <- FALSE
  open_short <- FALSE
  close_short <- FALSE
  close_long <- FALSE
  # If clause preparation
  if((tail(Cl(df_price[-nrow(df_price),]),1)[[1]] < Indicators$EMA_50) & 
     (tail(Cl(df_price[-nrow(df_price),]),1)[[1]] > Indicators$EMA_100)){
    long <- TRUE
  } else if ((tail(Cl(df_price[-nrow(df_price),]),1)[[1]] > Indicators$EMA_50) & 
             (tail(Cl(df_price[-nrow(df_price),]),1)[[1]] < Indicators$EMA_100)){
    short <- TRUE
  }
  # If clause open long and open short
  if ((Indicators$slope_150 >= 30) == TRUE & (Indicators$slope_150 <= 90) == TRUE |
      (Indicators$slope_100 >= 30) == TRUE & (Indicators$slope_100 <= 90) == TRUE | 
      (Indicators$slope_50 >= 30) == TRUE & (Indicators$slope_50 <= 90) == TRUE |
      long == TRUE |
      current_price > Indicators$EMA_50){
    
    open_long == TRUE
  } else if (Indicators$slope_150 < 0 &
             Indicators$slope_100 < 0 &
             Indicators$slope_50 < 0 &
             short == TRUE |
             current_price < Indicators$EMA_50) {
    open_short == TRUE
  }
  #If clause close short
  if (Indicators$rsi <= 30 |
      Indicators$stochastic <= 20 |
      (tail(Cl(df_price),1) >= (Indicators$EMA_100 - Indicators$EMA_100*0.001))) {
    close_short <- TRUE
  }
  #close long
  if (Indicators$rsi <= 70 |
      Indicators$stochastic <= 80 |
      (tail(Cl(df_price),1) <= (Indicators$EMA_100 - Indicators$EMA_100*0.001))) {
    close_long <- TRUE
  }
  
  Objects <- list(open_long, open_short, close_long, close_short)
  return(Objects)
}



