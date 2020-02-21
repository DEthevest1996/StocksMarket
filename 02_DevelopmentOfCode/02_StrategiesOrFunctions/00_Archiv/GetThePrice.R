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

