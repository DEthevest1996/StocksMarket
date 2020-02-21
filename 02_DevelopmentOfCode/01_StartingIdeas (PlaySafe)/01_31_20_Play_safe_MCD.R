#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# Just buy and sell to play for the start 
# Max Franke & Damian Etchevest
# 01/29/20
#=======================================================================================================================================

# Install packages
# install.packages("alphavantager")
# install.packages("jsonlite")
# Load packages
# library(alphavantager)
library(jsonlite)

# CONSTANT VARIABLES (CALLS are for gaining informations, Transactions are for buying and selling)
# ---------------------------------------------------------------------------------------------------------------------------------------
TOTAL_TIME_SEC <- 21600
TOTAL_CALLS_TRANSACTION <- 300
CALLS <- 50
TRANSACTION <- 250

# Inconstant variables
transaction <- 0
calls <- 0

#=======================================================================================================================================
# While loop for transactions (buy and sell)
#=======================================================================================================================================

while (transaction <= TRANSACTION){
  
  # Buy
  # ------------------------------------------------------------------------------------------------------------------------------------
  url_buy <- "https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=buy&quantity=1&symbol=MCD"
  buy_it <- fromJSON(url_buy)
  # Count transaction
  transaction <- transaction + 1
  # Safe to daily log book
  write.table(buy_it, 
              file = paste("Transactions ", format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%m-%d"), ".txt", sep = ""),
              append = TRUE,
              row.names = transaction)
  # Sleep for 85 seconds
  Sys.sleep(80)
  
  # Sell
  # ------------------------------------------------------------------------------------------------------------------------------------
  url_sell <- "https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=sell&quantity=1&symbol=MCD"
  sell_it <- fromJSON(url_sell)
  # Count transaction
  transaction <- transaction + 1
  # Safe to daily log book
  write.table(sell_it, 
              file = paste("Transactions ", format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%m-%d"), ".txt", sep = ""),
              append = TRUE,
              row.names = transaction)
  # Sleep for 85 seconds
  Sys.sleep(80)
}







