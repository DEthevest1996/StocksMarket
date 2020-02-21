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

#=======================================================================================================================================
# GLOBAL FUNCTIONS
#=======================================================================================================================================

# Get the price
#---------------------------------------------------------------------------------------------------------------------------------------

Price <- function(symbol,key){
  df_price <- read.csv.zoo(paste("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=",
                                 symbol,
                                 "&interval=1min&outputsize=full&apikey=",
                                 key,"&datatype=csv", sep = ""))
  return(df_price)
}

# Slope Function, calculate the degree of slope [FOR THE FIRST COLUMN OF THE INPUT DATAFRAME]
#---------------------------------------------------------------------------------------------------------------------------------------

slope <- function(x){
  # Load data
  x <- as.data.frame(x)
  # Colname
  colnames(x) <- "Price"
  # Count every Minute
  x$Min <- c(1:nrow(x))
  # Normalize the distance in order to calculate the slope
  x$Min_nom <- (x$Min - min(x$Min)) / (max(x$Min) - min(x$Min))
  # slope from last point to the first point
  slope <- (tail(x[,1], 1) - head(x[which(!is.na(x$Price)),1], 1))/(tail(x$Min_nom, 1) - head(x$Min_nom, 1))
  # Position in translation dataframe
  position <- which.min(abs(degree_m$m - slope))
  # Degree
  slope_to_degree <- degree_m[position, 1] 
  # Return degree
  return(slope_to_degree)
}

# Buy call
#---------------------------------------------------------------------------------------------------------------------------------------

buy <- function(symbol_to_buy, quantity_to_buy) {
  url <- paste("https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=buy&quantity=", quantity_to_buy, "&symbol=", symbol_to_buy, sep = "")
  return(url)
}

# Sell call
#---------------------------------------------------------------------------------------------------------------------------------------

sell <- function(symbol_to_sell, quantity_to_sell){
  url <- paste("https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=sell&quantity=", quantity_to_sell, "&symbol=", symbol_to_sell, sep = "")
  return(url)
}

# Indicators
#---------------------------------------------------------------------------------------------------------------------------------------
indicators <- function(df_price){
  
  # Slope for EMA
  slope_EMA_150 <- slope(as.data.frame(tail(EMA(Cl(df_price),150),150)))
  slope_EMA_100 <- slope(as.data.frame(tail(EMA(Cl(df_price),100),100)))
  slope_EMA_50 <- slope(as.data.frame(tail(EMA(Cl(df_price),50),50)))
  # EMA
  EMA_150 <- tail(EMA(Cl(df_price),150),1)[[1]] 
  EMA_100 <-tail(EMA(Cl(df_price),100),1)[[1]] 
  EMA_50 <- tail(EMA(Cl(df_price),50),1)[[1]] 
  # BB & RSI
  bb <- BBands(Cl(df_price),s.d=2) %>% tail(n=1)
  rsi <- RSI(Cl(df_price),n=8) %>% tail(n=1)
  # MACD
  macd <- MACD(Cl(df_price), nFast=12, nSlow=26,
               nSig=9, maType=SMA) %>% tail(n=1)
  # Stochastic
  stochastic <- mean(tail(stoch(df_price[,c("high","low","close")]),1)[,2:3]) * 100
  # Save to an Object
  Objects <- list("slope_150" = slope_EMA_150,
                  "slope_100" = slope_EMA_100,
                  "slope_50" = slope_EMA_50,
                  "EMA_150" = EMA_150, 
                  "EMA_100" = EMA_100,
                  "EMA_50" = EMA_50, 
                  "bb" = bb,
                  "rsi" = rsi, 
                  "macd" = macd,
                  "stochastic" = stochastic)
  # Save to the global environment
  assign("Indicators",as.data.frame(Objects),envir = .GlobalEnv)
  return(Objects)
}

# Time Series
#---------------------------------------------------------------------------------------------------------------------------------------

ts <- function(df_price){
  fit <- auto.arima(Cl(df_price))
  fit.forecast <- forecast(fit,h=1)
  fitted <- as.data.frame(fit.forecast)
  return(fitted)
}



