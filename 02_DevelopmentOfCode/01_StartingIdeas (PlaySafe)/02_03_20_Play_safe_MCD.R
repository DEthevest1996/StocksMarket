#=======================================================================================================================================
# Class: CIS-544 DATA MINING & MACHINE LRNG
# Just buy and sell to play for the start 
# Max Franke & Damian Etchevest
# 02/02/20
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

# Create dataframe for daily log book
all_transactions <- data.frame(matrix(ncol = 11, nrow = 0))
col_names <- c("status", "description", "symbol", "price", "quantity", "budget", "Month", "Day", "Time", "Transaction", "TNo")
colnames(all_transactions) <- col_names

# Save empty daily log book
write.table(all_transactions,
            file = paste("Transactions ", format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%m-%d"), ".csv", sep = ""),
            sep = ",")

#=======================================================================================================================================
# While loop for transactions (buy and sell)
#=======================================================================================================================================

while (transaction <= TRANSACTION){
  
  # Buy
  # ------------------------------------------------------------------------------------------------------------------------------------
  url_buy <- "https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=buy&quantity=1&symbol=MCD"
  buy_it <- fromJSON(url_buy)
  # Add the Date & Time
  buy_it$Month <- format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%m")
  buy_it$Day <- format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%d")
  buy_it$Time <- format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
  # Add the type of transactoin
  buy_it$Transaction <- "Buy"
  # Count transaction
  transaction <- transaction + 1
  # Add transaction number to dataframe
  buy_it$Number <- transaction
  # Safe to daily log book
  write.table(buy_it,
              file = paste("Transactions ", format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%m-%d"), ".csv", sep = ""),
              append = TRUE,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE)
  # Sleep
  Sys.sleep(2)
  
  # Sell
  # ------------------------------------------------------------------------------------------------------------------------------------
  url_sell <- "https://projectrex.net/stocks/?key=2b726457&av_key=OBEKR8AXV71DDV2I&request=sell&quantity=1&symbol=MCD"
  sell_it <- fromJSON(url_sell)
  # Add the Date & Time
  sell_it$Month <- format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%m")
  sell_it$Day <- format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%d")
  sell_it$Time <- format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%H:%M:%S")
  # Add the type of transactoin
  sell_it$Transaction <- "Sell"
  # Count transaction
  transaction <- transaction + 1
  # Add transaction number to dataframe
  sell_it$Number <- transaction
  # Safe to daily log book
  write.table(sell_it,
              file = paste("Transactions ", format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%m-%d"), ".csv", sep = ""),
              append = TRUE,
              sep = ",",
              row.names = FALSE,
              col.names = FALSE)
  # Sleep
  Sys.sleep(60)
}


# Reset the transactions to 0 for the next day
while(format(strptime(Sys.time(),"%Y-%m-%d %H:%M:%S"),"%H:%M") < "21:10"){
  
} 
transaction <- 0

