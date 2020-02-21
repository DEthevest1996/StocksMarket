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


# Keys and Symbols
#---------------------------------------------------------------------------------------------------------------------------------------

Symbols_and_Keys <- data.frame(Symbols = c("AAPL", "SINA", "MSFT", "BA","BAC"),
                                           #, "AAOI", "BHC", "SAN", "SNE", "MU"),
                                           #"USDJPY","GBPUSD","AUDUSD","NZDUSD","EURUSD","USDJPY","GBPUSD","AUDUSD","NZDUSD"),
                               Keys = c("PS4R6O34SKO7QMI7","OZ1OB4JLZO38F64B","LJN45VDK4LQUODHI",
                                        "MAZM65O84SDP2ZSI","626T9PJYPCBZXJH4"))
                                        #,"LMRU8T7Q1DVRE7ZV",
                                        #"ECYLECGWH064HPP8","73IWZI0WQ5ROP6Y6",
                                        #"GCTSR9ASVV0D8W7S","6NF4N7TVGI7UFVYC"))


