#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# Just buy and sell to play for the start 
# Max Franke & Damian Etchevest
# 02/05/20
#=======================================================================================================================================

# Install packages
'install.packages("alphavantager")
install.packages("jsonlite")
install.packages("TTR")
install.packages("ncar")
install.packages("xts")
install.packages("dplyr")
install.packages("quantmod")
install.packages("tidyverse")
install.packages("quantmod)
install.packages("magrittr)'
as.Datet(Sys.time(), )
# Load packages
#library(alphavantager)
library(jsonlite)
#library(forecast)
#library(TTR)
#library(ncar)
library(dplyr)
#library(xts)
#library(quantmod)
#library(tidyverse)
library(RMySQL)
library(quantmod)
library(magrittr)


# Indicators
#---------------------------------------------------------------------------------------------------------------------------------------
indicators <- function(df_price){
  
  # Slope for EMA
  slope_EMA_150 <- slope(as.data.frame(tail(EMA(Cl(df_price),150),150)))
  slope_EMA_100 <- slope(as.data.frame(tail(EMA(Cl(df_price),100),100)))
  slope_EMA_50 <- slope(as.data.frame(tail(EMA(Cl(df_price),50),50)))
  # EMA
  EMA_150 <- tail(EMA(Cl(df_price),150),1) 
  EMA_100 <-tail(EMA(Cl(df_price),100),1) 
  EMA_50 <- tail(EMA(Cl(df_price),50),1) 
  # BB & RSI
  bb <- BBands(Cl(df_price),s.d=2) %>% tail(n=1)
  rsi <- RSI(Cl(df_price),n=8) %>% tail(n=1)
  # MACD
  macd <- MACD(Cl(df_price), nFast=12, nSlow=26,
               nSig=9, maType=SMA) %>% tail(n=1)
  # Save to an Object
  Objects <- list("slope_150" = slope_EMA_150,
                  "slope_100" = slope_EMA_100,
                  "slope_50" = slope_EMA_50,
                  "EMA_150" = EMA_150, 
                  "EMA_100" = EMA_100,
                  "EMA_50" = EMA_50, 
                  "bb" = bb,
                  "rsi" = rsi, 
                  "macd" = macd)
  # Save to the global environment
  assign("Indicators",as.data.frame(Objects),envir = .GlobalEnv)
  return(Objects)
}

